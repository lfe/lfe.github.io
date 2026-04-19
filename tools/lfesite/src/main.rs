use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, Subcommand};

mod cmd;

/// Build orchestration and data pre-rendering for the LFE website.
#[derive(Debug, Parser)]
#[command(name = "lfesite", version, about)]
struct Cli {
    /// Path to the project root directory.
    #[arg(short = 'C', long = "project-dir", default_value = ".")]
    project_dir: PathBuf,

    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// One-shot Zola to Cobalt migration.
    Migrate,
    /// Walk _data/*.yml, render *_md fields to *_html.
    Prerender,
    /// Full build pipeline (prerender, sass, tailwind, cobalt).
    Build,
    /// Dev server with file watching.
    Serve {
        /// Port for the dev server.
        #[arg(short, long, default_value_t = 3000)]
        port: u16,
    },
    /// Sanity checks on data files.
    Validate,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let project_dir = cli.project_dir.canonicalize().unwrap_or(cli.project_dir);

    match cli.command {
        Command::Migrate => cmd::migrate::run(&project_dir),
        Command::Prerender => cmd::prerender::run(&project_dir),
        Command::Build => cmd::build::run(&project_dir),
        Command::Serve { port } => cmd::serve::run(&project_dir, port),
        Command::Validate => cmd::validate::run(&project_dir),
    }
}
