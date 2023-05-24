use clap::{Parser, Subcommand, ValueEnum};
use vamp_ir::file_gen::cli::{generate, GenerateCommands};
use vamp_ir::halo2::cli::{halo2, Halo2Commands};
use vamp_ir::plonk::cli::{plonk, PlonkCommands};
use vamp_ir::util::Config;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    backend: Backend,

    #[clap(short, long, default_value = "false")]
    quiet: bool,
}

#[derive(Subcommand)]
enum Backend {
    #[command(subcommand)]
    Generate(GenerateCommands),
    #[command(subcommand)]
    Plonk(PlonkCommands),
    #[command(subcommand)]
    Halo2(Halo2Commands),
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
    let config = Config { quiet: cli.quiet };

    match &cli.backend {
        Backend::Generate(generate_commands) => generate(generate_commands, &config),
        Backend::Plonk(plonk_commands) => plonk(plonk_commands, &config),
        Backend::Halo2(halo2_commands) => halo2(halo2_commands, &config),
    }
}
