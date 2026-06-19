use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, Subcommand};

mod cmd;
mod util;

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
    /// Migrate Jekyll blog posts to Cobalt format.
    BlogMigrate {
        /// Path to the old Jekyll blog repository root.
        #[arg(long)]
        source: PathBuf,
    },
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
    /// Create a new blog post.
    NewPost {
        /// Post title
        title: String,
        /// Create as draft
        #[arg(long)]
        draft: bool,
        /// Override the URL slug
        #[arg(long)]
        slug: Option<String>,
        /// Post category (default: tutorials). No spaces — use hyphens (e.g. user-groups)
        #[arg(long, default_value = "tutorials")]
        category: String,
        /// Comma-separated tags. No spaces within a tag — use hyphens (e.g. lfe-friday)
        #[arg(long, default_value = "")]
        tags: String,
        /// Override timestamp (YYYY-MM-DD HH:MM)
        #[arg(long)]
        date: Option<String>,
    },
    /// Publish a draft post (set is_draft: false).
    PublishPost {
        /// Path to the post file
        file: PathBuf,
    },
    /// Convert a post to draft, or create a new draft interactively.
    DraftPost {
        /// Path to an existing post file (omit to create new draft)
        file: Option<PathBuf>,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let project_dir = cli.project_dir.canonicalize().unwrap_or(cli.project_dir);

    match cli.command {
        Command::Migrate => cmd::migrate::run(&project_dir),
        Command::BlogMigrate { source } => cmd::blog_migrate::run(&project_dir, &source),
        Command::Prerender => cmd::prerender::run(&project_dir),
        Command::Build => cmd::build::run(&project_dir),
        Command::Serve { port } => cmd::serve::run(&project_dir, port),
        Command::Validate => cmd::validate::run(&project_dir),
        Command::NewPost { title, draft, slug, category, tags, date } => {
            cmd::new_post::run(
                &project_dir,
                &title,
                draft,
                slug.as_deref(),
                &category,
                &tags,
                date.as_deref(),
            )
        }
        Command::PublishPost { file } => cmd::publish_post::run(&file),
        Command::DraftPost { file } => {
            cmd::draft_post::run(&project_dir, file.as_deref())
        }
    }
}
