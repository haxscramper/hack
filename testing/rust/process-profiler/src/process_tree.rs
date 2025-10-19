use crate::data_model::{ProcessProfile, ResourceSample};
use procfs::process::Process;
use std::collections::HashMap;
use color_eyre::Result;
use tracing::info;

use chrono::Utc;

pub struct ProcessTree {
    processes: HashMap<u32, ProcessProfile>,
    verbose: bool,
}

impl ProcessTree {
    pub fn new(verbose: bool) -> Self {
        Self {
            processes: HashMap::new(),
            verbose,
        }
    }

    pub fn add_process(&mut self, profile: ProcessProfile) {
        if self.verbose {
            info!("Adding process: PID {} - {}", profile.pid, profile.command);
        }
        self.processes.insert(profile.pid, profile);
    }

    pub fn sample_resources(&mut self, pid: u32) -> Result<()> {
        if let Ok(process) = Process::new(pid as i32) {
            if let (Ok(stat), Ok(_status)) = (process.stat(), process.status()) {
                let sample = ResourceSample {
                    timestamp: Utc::now(),
                    cpu_percent: self.calculate_cpu_percent(&stat)?,
                    memory_rss_kb: stat.rss * 4, // RSS is in pages, typically 4KB each
                    memory_vms_kb: stat.vsize / 1024,
                    num_threads: stat.num_threads as u32,
                    num_fds: self.count_file_descriptors(pid)?,
                };

                if let Some(profile) = self.processes.get_mut(&pid) {
                    profile.add_resource_sample(sample);
                }
            }
        }
        Ok(())
    }

    fn calculate_cpu_percent(&self, stat: &procfs::process::Stat) -> Result<f64> {
        // Simple CPU calculation - in a real implementation, you'd want to track
        // this over time intervals for more accurate percentage
        let total_time = stat.utime + stat.stime;
        // This is a simplified calculation - proper CPU% requires time deltas
        Ok(total_time as f64 / 100.0) // Convert from jiffies, simplified
    }

    fn count_file_descriptors(&self, pid: u32) -> Result<u32> {
        let fd_dir = format!("/proc/{}/fd", pid);
        match std::fs::read_dir(fd_dir) {
            Ok(entries) => Ok(entries.count() as u32),
            Err(_) => Ok(0),
        }
    }

    pub fn mark_process_ended(&mut self, pid: u32, exit_code: Option<i32>) {
        if let Some(profile) = self.processes.get_mut(&pid) {
            profile.set_end_time(exit_code);
            if self.verbose {
                info!("Process ended: PID {} - exit code: {:?}", pid, exit_code);
            }
        }
    }

    pub fn build_tree(&self, root_pid: u32) -> Option<ProcessProfile> {
        let mut root = self.processes.get(&root_pid)?.clone();
        self.attach_children(&mut root);
        Some(root)
    }

    fn attach_children(&self, parent: &mut ProcessProfile) {
        for (_, process) in &self.processes {
            if let Some(ppid) = process.ppid {
                if ppid == parent.pid {
                    let mut child = process.clone();
                    self.attach_children(&mut child);
                    parent.add_child(child);
                }
            }
        }
    }
}
