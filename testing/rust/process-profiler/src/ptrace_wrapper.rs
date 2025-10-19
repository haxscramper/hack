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

    pub fn track_pid(&mut self, pid: u32) {
        self.traced_pids.insert(pid);
    }

    pub fn detach(&mut self, pid: u32) -> Result<()> {
        let pid = Pid::from_raw(pid as i32);
        
        if self.traced_pids.contains(&(pid.as_raw() as u32)) {
            if self.verbose {
                println!("Detaching from PID: {}", pid);
            }
            ptrace::detach(pid, None)?;
            self.traced_pids.remove(&(pid.as_raw() as u32));
        }
        Ok(())
    }

    pub fn continue_process(&self, pid: u32) -> Result<()> {
        let pid = Pid::from_raw(pid as i32);
        ptrace::cont(pid, None)?;
        Ok(())
    }

    pub fn wait_for_event(&self) -> Result<Option<(u32, WaitStatus)>> {
        match waitpid(None, Some(WaitPidFlag::WNOHANG)) {
            Ok(WaitStatus::Stopped(pid, _)) => {
                Ok(Some((pid.as_raw() as u32, WaitStatus::Stopped(pid, Signal::SIGTRAP))))
            }
            Ok(WaitStatus::Exited(pid, exit_code)) => {
                Ok(Some((pid.as_raw() as u32, WaitStatus::Exited(pid, exit_code))))
            }
            Ok(WaitStatus::Signaled(pid, signal, _)) => {
                Ok(Some((pid.as_raw() as u32, WaitStatus::Signaled(pid, signal, false))))
            }
            Ok(_) => Ok(None),
            Err(nix::errno::Errno::ECHILD) => Ok(None),
            Err(e) => Err(e.into()),
        }
    }

    pub fn setup_trace_options(&self, pid: u32) -> Result<()> {
        let pid = Pid::from_raw(pid as i32);
        
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
