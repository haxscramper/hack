use nix::sys::ptrace;
use nix::sys::wait::{waitpid, WaitStatus, WaitPidFlag};
use nix::sys::signal::Signal;
use nix::unistd::Pid;
use std::collections::HashSet;
use color_eyre::Result;
use color_eyre::eyre::eyre;

pub struct PtraceTracker {
    traced_pids: HashSet<u32>,
    verbose: bool,
}

impl PtraceTracker {
    pub fn new(verbose: bool) -> Self {
        Self {
            traced_pids: HashSet::new(),
            verbose,
        }
    }

    pub fn track_pid(&mut self, pid: u32) {
        self.traced_pids.insert(pid);
    }

    pub fn untrack_pid(&mut self, pid: u32) {
        self.traced_pids.remove(&pid);
    }

    pub fn attach_to_new_child(&mut self, pid: u32) -> Result<()> {
        let pid = Pid::from_raw(pid as i32);
        
        if self.verbose {
            println!("Attaching to new child PID: {}", pid);
        }

        ptrace::attach(pid)?;
        
        // Wait for the process to stop
        match waitpid(Some(pid), None) {
            Ok(WaitStatus::Stopped(_, _)) => {
                self.traced_pids.insert(pid.as_raw() as u32);
                Ok(())
            }
            Ok(status) => Err(eyre!("Unexpected status after attach: {:?}", status)),
            Err(e) => Err(e.into()),
        }
    }

    pub fn detach(&mut self, pid: u32) -> Result<()> {
        let pid = Pid::from_raw(pid as i32);
        
        if self.traced_pids.contains(&(pid.as_raw() as u32)) {
            if self.verbose {
                println!("Detaching from PID: {}", pid);
            }
            // Only detach if process is still alive
            if let Ok(_) = waitpid(Some(pid), Some(WaitPidFlag::WNOHANG)) {
                let _ = ptrace::detach(pid, None);
            }
            self.traced_pids.remove(&(pid.as_raw() as u32));
        }
        Ok(())
    }

    pub fn continue_process(&self, pid: u32) -> Result<()> {
        let pid = Pid::from_raw(pid as i32);
        if self.verbose {
            println!("Continuing PID: {}", pid);
        }
        ptrace::cont(pid, None)?;
        Ok(())
    }

    pub fn wait_for_event(&self) -> Result<Option<(u32, WaitStatus)>> {
        // Use WNOHANG to not block
        match waitpid(None, Some(WaitPidFlag::WNOHANG | WaitPidFlag::__WALL)) {
            Ok(WaitStatus::StillAlive) => Ok(None),
            Ok(status) => {
                if let Some(pid) = status.pid() {
                    Ok(Some((pid.as_raw() as u32, status)))
                } else {
                    Ok(None)
                }
            }
            Err(nix::errno::Errno::ECHILD) => {
                // No children to wait for
                Ok(None)
            }
            Err(e) => Err(e.into()),
        }
    }

    pub fn setup_trace_options(&self, pid: u32) -> Result<()> {
        let pid = Pid::from_raw(pid as i32);
        
        if self.verbose {
            println!("Setting trace options for PID: {}", pid);
        }
        
        // Set ptrace options to trace forks, vforks, clones, and execs
        let options = ptrace::Options::PTRACE_O_TRACEFORK
            | ptrace::Options::PTRACE_O_TRACEVFORK
            | ptrace::Options::PTRACE_O_TRACECLONE
            | ptrace::Options::PTRACE_O_TRACEEXEC
            | ptrace::Options::PTRACE_O_EXITKILL;
            
        ptrace::setoptions(pid, options)?;
        Ok(())
    }
}

impl Drop for PtraceTracker {
    fn drop(&mut self) {
        for &pid in &self.traced_pids.clone() {
            let _ = self.detach(pid);
        }
    }
}
