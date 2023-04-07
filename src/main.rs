use vamp_ir::halo2;
use vamp_ir::plonk;
use clap::{Parser, Subcommand, ValueEnum};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    backend: Backend,
}

#[derive(Subcommand)]
enum Backend {
    #[command(subcommand)]
    Plonk(plonk::cli::PlonkCommands),
    #[command(subcommand)]
    Halo2(halo2::cli::Halo2Commands),
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum ProofSystems {
    /// PLONK general-purpose zero-knowledge proof scheme
    Plonk,
    /// Halo 2 zero-knowledge proving system
    Halo2,
}

/* Main entry point for vamp-ir compiler, prover, and verifier. */
fn main() {

    let cli = Cli::parse();
    match &cli.backend {
        Backend::Plonk(plonk_commands) => plonk::cli::plonk(plonk_commands),
        Backend::Halo2(halo2_commands) => halo2::cli::halo2(halo2_commands),
    }
}
