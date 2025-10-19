use crate::data_model::{ProcessProfile, ProfileData, ProfilerMetadata};
use crate::process_tree::ProcessTree;
use crate::ptrace_wrapper::PtraceTracker;
use color_eyre::Result;

use chrono::Utc;
use nix::sys::wait::WaitStatus;
use nix::unistd::{fork, ForkResult};
use std::collections::HashSet;
use std::path::PathBuf;
use std::time::Duration;
use tokio::time::interval;
use std::os::unix::process::CommandExt;
use color_eyre::eyre::eyre;
use nix::sys::wait::waitpid;
use nix::unistd::Pid;
use tracing::{info, error};





pub struct ProcessProfiler {
    command: String,
    args: Vec<String>,
    workdir: Option<PathBuf>,
    sampling_interval: Duration,
    verbose: bool,
    process_tree: ProcessTree,
    ptrace_tracker: PtraceTracker,
    active_pids: HashSet<u32>,
}

impl ProcessProfiler {
    pub fn new(
        command: String,
        args: Vec<String>,
        workdir: Option<PathBuf>,
        interval_ms: u64,
        verbose: bool,
    ) -> Result<Self> {
        Ok(Self {
            command,
            args,
            workdir,
            sampling_interval: Duration::from_millis(interval_ms),
            verbose,
            process_tree: ProcessTree::new(verbose),
            ptrace_tracker: PtraceTracker::new(verbose),
            active_pids: HashSet::new(),
        })
    }
    
    pub async fn run(&mut self) -> Result<ProfileData> {
        let start_time = Utc::now();
        
        if self.verbose {
            info!("Starting profiler at: {}", start_time);
        }
    
        // Fork and exec the target process
        let child_pid = self.spawn_target_process()?;
        
        // Set up the main profiling loop
        let mut sampling_timer = interval(self.sampling_interval);
        sampling_timer.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
        
        loop {
            tokio::select! {
                _ = sampling_timer.tick() => {
                    if self.verbose {
                        info!("Sampling {} active processes", self.active_pids.len());
                    }
                    self.sample_all_processes().await?;
                }
            }
            
            // Check for ptrace events (non-blocking)
            if !self.handle_ptrace_events().await? {
                if self.verbose {
                    info!("All processes have terminated");
                }
                break;
            }
            
            // Small delay to prevent busy-waiting
            tokio::time::sleep(Duration::from_millis(1)).await;
        }
    
        let end_time = Utc::now();
        let duration = (end_time - start_time).num_milliseconds() as u64;
    
        if self.verbose {
            info!("Profiling completed at: {}", end_time);
            info!("Total duration: {}ms", duration);
        }
    
        // Build the final process tree
        let root_process = self.process_tree.build_tree(child_pid)
            .ok_or_else(|| eyre!("Failed to build process tree"))?;
    
        let profile_data = ProfileData {
            root_process,
            total_duration_ms: duration,
            sampling_interval_ms: self.sampling_interval.as_millis() as u64,
            profiler_metadata: ProfilerMetadata {
                profiler_version: env!("CARGO_PKG_VERSION").to_string(),
                start_time,
                end_time,
                hostname: self.get_hostname(),
                kernel_version: self.get_kernel_version(),
            },
        };
    
        Ok(profile_data)
    }
    
    async fn handle_ptrace_events(&mut self) -> Result<bool> {
        // Process all pending events
        let mut had_events = false;
        
        while let Some((pid, status)) = self.ptrace_tracker.wait_for_event()? {
            had_events = true;
            
            if self.verbose {
                info!("Got event for PID {}: {:?}", pid, status);
            }
            
            match status {
                WaitStatus::Stopped(_, signal) => {
                    if self.verbose {
                        info!("Process {} stopped with signal {:?}", pid, signal);
                    }
                    
                    // Check if this is a new process (fork/clone event)
                    if !self.active_pids.contains(&pid) {
                        self.handle_new_process(pid).await?;
                    }
                    
                    // Continue the process
                    self.ptrace_tracker.continue_process(pid)?;
                }

                WaitStatus::PtraceEvent(child_pid, _, event) => {
                    if self.verbose {
                        info!("Ptrace event from {}: {}", child_pid, event);
                    }
                    
                    // Handle fork/clone/vfork events
                    const PTRACE_EVENT_FORK: i32 = 1;
                    const PTRACE_EVENT_VFORK: i32 = 2;
                    const PTRACE_EVENT_CLONE: i32 = 3;
                    
                    match event {
                        PTRACE_EVENT_FORK | 
                        PTRACE_EVENT_VFORK | 
                        PTRACE_EVENT_CLONE => {
                            // Get the new child PID
                            if let Ok(new_pid) = nix::sys::ptrace::getevent(Pid::from_raw(i32::from(child_pid))) {
                                let new_pid = new_pid as u32;
                                if self.verbose {
                                    info!("New child process detected: {}", new_pid);
                                }
                                self.handle_new_process(new_pid).await?;
                            }
                        }
                        _ => {}
                    }
                    
                    self.ptrace_tracker.continue_process(pid)?;
                }


                WaitStatus::Exited(_, exit_code) => {
                    if self.verbose {
                        info!("Process {} exited with code {}", pid, exit_code);
                    }
                    
                    self.process_tree.mark_process_ended(pid, Some(exit_code));
                    self.active_pids.remove(&pid);
                    self.ptrace_tracker.untrack_pid(pid);
                }
                WaitStatus::Signaled(_, signal, _) => {
                    if self.verbose {
                        info!("Process {} terminated by signal {:?}", pid, signal);
                    }
                    
                    self.process_tree.mark_process_ended(pid, None);
                    self.active_pids.remove(&pid);
                    self.ptrace_tracker.untrack_pid(pid);
                }
                _ => {
                    if self.verbose {
                        info!("Unhandled wait status for PID {}: {:?}", pid, status);
                    }
                }
            }
        }
        
        // Return true if we still have active processes
        let has_active = !self.active_pids.is_empty();
        if self.verbose && !had_events && has_active {
            info!("No events, but still have {} active PIDs", self.active_pids.len());
        }
        
        Ok(has_active)
    }

    fn spawn_target_process(&mut self) -> Result<u32> {
        match unsafe { fork() } {
            Ok(ForkResult::Parent { child }) => {
                let child_pid = child.as_raw() as u32;
                
                if self.verbose {
                    info!("Spawned child process with PID: {}", child_pid);
                }
    
                // Wait for the child to stop itself after calling traceme()
                match waitpid(Some(child), None) {
                    Ok(WaitStatus::Stopped(_, signal)) => {
                        if self.verbose {
                            info!("Child stopped with signal: {:?}", signal);
                        }
                    }
                    Ok(status) => {
                        return Err(eyre!("Unexpected wait status: {:?}", status));
                    }
                    Err(e) => {
                        return Err(eyre!("Failed to wait for child: {}", e));
                    }
                }
                
                // Now set up trace options (no need to attach - traceme already did that)
                self.ptrace_tracker.setup_trace_options(child_pid)?;
                
                // Track the child
                self.ptrace_tracker.track_pid(child_pid);
                
                // Create the root process profile
                let working_dir = self.workdir.as_ref()
                    .map(|p| p.to_string_lossy().to_string())
                    .unwrap_or_else(|| std::env::current_dir()
                        .unwrap_or_default()
                        .to_string_lossy()
                        .to_string());
    
                let root_profile = ProcessProfile::new(
                    child_pid,
                    Some(std::process::id()),
                    self.command.clone(),
                    self.args.clone(),
                    working_dir,
                );
    
                self.process_tree.add_process(root_profile);
                self.active_pids.insert(child_pid);
    
                // Continue the child process
                self.ptrace_tracker.continue_process(child_pid)?;
    
                Ok(child_pid)
            }
            Ok(ForkResult::Child) => {
                // Child process
                self.exec_target_process()
            }
            Err(e) => Err(e.into()),
        }
    }
    
    fn exec_target_process(&self) -> Result<u32> {
        use nix::sys::ptrace;
        use nix::sys::signal::{raise, Signal};
        
        // Enable tracing for this process FIRST
        ptrace::traceme()?;
        
        // Stop ourselves to let parent set up tracing
        raise(Signal::SIGSTOP)?;
        
        // When we continue, parent will be tracing us
        
        // Change working directory if specified
        if let Some(workdir) = &self.workdir {
            std::env::set_current_dir(workdir)?;
        }
    
        // Execute the target command
        let mut cmd = std::process::Command::new(&self.command);
        cmd.args(&self.args);
        
        let error = cmd.exec();
        
        // If we reach here, exec failed
        error!("Failed to execute command: {}", error);
        std::process::exit(1);
    }

    async fn handle_new_process(&mut self, pid: u32) -> Result<()> {
        if let Ok(process) = procfs::process::Process::new(pid as i32) {
            if let (Ok(stat), Ok(cmdline)) = (process.stat(), process.cmdline()) {
                let ppid = stat.ppid as u32;
                let command = cmdline.first().cloned().unwrap_or_else(|| "unknown".to_string());
                let args = cmdline.into_iter().skip(1).collect();
    
                let working_dir = process.cwd()
                    .map(|p| p.to_string_lossy().to_string())
                    .unwrap_or_else(|_| "unknown".to_string());
    
                let profile = ProcessProfile::new(pid, Some(ppid), command, args, working_dir);
                
                self.process_tree.add_process(profile);
                self.active_pids.insert(pid);
                
                // Attach to the new process
                self.ptrace_tracker.attach_to_new_child(pid)?;
                self.ptrace_tracker.setup_trace_options(pid)?;
            }
        }
    
        Ok(())
    }

    async fn sample_all_processes(&mut self) -> Result<()> {
        let pids: Vec<u32> = self.active_pids.iter().cloned().collect();
        
        for pid in pids {
            if let Err(e) = self.process_tree.sample_resources(pid) {
                if self.verbose {
                    info!("Failed to sample resources for PID {}: {}", pid, e);
                }
                // Process might have died, remove it from active list
                self.active_pids.remove(&pid);
            }
        }

        Ok(())
    }

    fn get_hostname(&self) -> String {
        std::fs::read_to_string("/proc/sys/kernel/hostname")
            .unwrap_or_else(|_| "unknown".to_string())
            .trim()
            .to_string()
    }

    fn get_kernel_version(&self) -> String {
        std::fs::read_to_string("/proc/version")
            .unwrap_or_else(|_| "unknown".to_string())
            .lines()
            .next()
            .unwrap_or("unknown")
            .to_string()
    }
}
