use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use chrono::{DateTime, Utc};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessProfile {
    pub pid: u32,
    pub ppid: Option<u32>,
    pub command: String,
    pub args: Vec<String>,
    pub working_directory: String,
    pub start_time: DateTime<Utc>,
    pub end_time: Option<DateTime<Utc>>,
    pub exit_code: Option<i32>,
    pub children: Vec<ProcessProfile>,
    pub resource_samples: Vec<ResourceSample>,
    pub environment: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceSample {
    pub timestamp: DateTime<Utc>,
    pub cpu_percent: f64,
    pub memory_rss_kb: u64,
    pub memory_vms_kb: u64,
    pub num_threads: u32,
    pub num_fds: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProfileData {
    pub root_process: ProcessProfile,
    pub total_duration_ms: u64,
    pub sampling_interval_ms: u64,
    pub profiler_metadata: ProfilerMetadata,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProfilerMetadata {
    pub profiler_version: String,
    pub start_time: DateTime<Utc>,
    pub end_time: DateTime<Utc>,
    pub hostname: String,
    pub kernel_version: String,
}

impl ProcessProfile {
    pub fn new(
        pid: u32,
        ppid: Option<u32>,
        command: String,
        args: Vec<String>,
        working_directory: String,
    ) -> Self {
        Self {
            pid,
            ppid,
            command,
            args,
            working_directory,
            start_time: Utc::now(),
            end_time: None,
            exit_code: None,
            children: Vec::new(),
            resource_samples: Vec::new(),
            environment: std::env::vars().collect(),
        }
    }

    pub fn add_child(&mut self, child: ProcessProfile) {
        self.children.push(child);
    }

    pub fn add_resource_sample(&mut self, sample: ResourceSample) {
        self.resource_samples.push(sample);
    }

    pub fn set_end_time(&mut self, exit_code: Option<i32>) {
        self.end_time = Some(Utc::now());
        self.exit_code = exit_code;
    }
}
