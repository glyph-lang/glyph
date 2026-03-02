use std::fs;
use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, Subcommand};
use glyph_frontend::{FrontendOptions, compile_source};
use serde::Serialize;
use tower_lsp::{LspService, Server};

mod diagnostics;
mod document;
mod position;
mod server;

use server::GlyphLanguageServer;

#[derive(Parser, Debug)]
#[command(name = "glyphlsp", about = "Glyph language server")]
struct Args {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Probe a file and emit diagnostics as JSON
    Probe {
        #[arg(value_name = "PATH")]
        path: PathBuf,
    },
}

#[derive(Serialize)]
struct ProbeResult {
    diagnostics: usize,
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();

    match args.command {
        Some(Commands::Probe { path }) => {
            let source = fs::read_to_string(&path)?;
            let output = compile_source(&source, FrontendOptions::default());
            let payload = ProbeResult {
                diagnostics: output.diagnostics.len(),
            };
            let json = serde_json::to_string_pretty(&payload)?;
            println!("{}", json);
            Ok(())
        }
        None => {
            // Default: run the LSP server on stdio
            let stdin = tokio::io::stdin();
            let stdout = tokio::io::stdout();

            let (service, socket) = LspService::new(GlyphLanguageServer::new);
            Server::new(stdin, stdout, socket).serve(service).await;
            Ok(())
        }
    }
}
