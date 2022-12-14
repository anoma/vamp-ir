extern crate pest;
extern crate pest_derive;
use clap::{Parser, Subcommand};
use vamp_ir::*;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Sets up the public parameters required for proving
    Setup(Setup),
    /// Compiles a given source file to a circuit
    Compile(Compile),
    /// Proves knowledge of witnesses satisfying a circuit
    Prove(Prove),
    /// Verifies that a proof is a correct one
    Verify(Verify),
}

/* Main entry point for vamp-ir compiler, prover, and verifier. */
fn main() {
    let cli = Cli::parse();
    match &cli.command {
        Commands::Setup(args) => {
            setup_cmd(args);
        },
        Commands::Compile(args) => {
            compile_cmd(args);
        },
        Commands::Prove(args) => {
            prove_cmd(args);
        },
        Commands::Verify(args) => {
            verify_cmd(args);
        },
    }
}
