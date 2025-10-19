mod profiler;
mod process_tree;
mod data_model;
mod ptrace_wrapper;

use clap::Parser;
use std::path::PathBuf;
use color_eyre::Result;


#[derive(Parser)]
#[command(name = "process-profiler")]
#[command(about = "A process profiler that tracks process execution and resource usage")]
struct Args {
    /// Command to execute
    #[arg(short, long)]
    command: String,

    /// Arguments for the command
    #[arg(short, long, allow_hyphen_values = true)]
    args: Vec<String>,

    /// Working directory for the command
    #[arg(short, long)]
    workdir: Option<PathBuf>,

    /// Output JSON file
    #[arg(short, long, default_value = "profile.json")]
    output: PathBuf,

    /// Sampling interval in milliseconds
    #[arg(short, long, default_value = "100")]
    interval: u64,

    /// Verbose output
    #[arg(short, long)]
    verbose: bool,
}

#[tokio::main]
async fn main() -> Result<()> {
    use tracing_error::ErrorLayer;
    use tracing_subscriber::prelude::*;
    
    tracing_subscriber::registry()
        .with(ErrorLayer::default())
        .init();
        
    color_eyre::config::HookBuilder::default()
        .capture_span_trace_by_default(true)
        .install()?;

    let args = Args::parse();

    if args.verbose {
        println!("Starting process profiler...");
        println!("Command: {} {:?}", args.command, args.args);
        println!("Output: {:?}", args.output);
        println!("Sampling interval: {}ms", args.interval);
    }

    let mut profiler = profiler::ProcessProfiler::new(
        args.command,
        args.args,
        args.workdir,
        args.interval,
        args.verbose,
    )?;

    let profile_data = profiler.run().await?;

    // Write to JSON file
    let json = serde_json::to_string_pretty(&profile_data)?;
    std::fs::write(&args.output, json)?;

    if args.verbose {
        println!("Profile data written to {:?}", args.output);
    }

    Ok(())
}
