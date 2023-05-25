use crate::ast::{InfixOp, Module, Pat};
use crate::error::Error;
use crate::transform::{collect_module_variables, compile, FieldOps};

use serde_json::Map;
use std::collections::HashMap;

use std::fs;

use clap::{Args, Subcommand};
use std::path::PathBuf;

use num_bigint::BigInt;

use crate::qprintln;
use crate::util::Config;

#[derive(Subcommand)]
pub enum GenerateCommands {
    /// Export witnesses into a template JSON file
    WitnessFile(JSONWitnessFile),
}

#[derive(Args)]
pub struct JSONWitnessFile {
    /// Path to source file that witnesses come from
    #[arg(short, long)]
    source: PathBuf,
    /// Path to which the witness file is written
    #[arg(short, long)]
    output: PathBuf,
}

// Trivial FieldOps for witness file generation
impl FieldOps for () {
    fn canonical(&self, a: BigInt) -> BigInt {
        a
    }
    fn negate(&self, a: BigInt) -> BigInt {
        a
    }
    fn infix(&self, _: InfixOp, a: BigInt, _: BigInt) -> BigInt {
        a
    }
}

/* Implements the subcommand that writes witnesses to a JSON file. */
pub fn witness_file_cmd(
    JSONWitnessFile { source, output }: &JSONWitnessFile,
    config: &Config,
) -> Result<(), Error> {
    qprintln!(config, "** Reading file...");
    let unparsed_file = fs::read_to_string(source).expect("cannot read file");
    let module = Module::parse(&unparsed_file).unwrap();
    let module_3ac = compile(module.clone(), &(), config);

    qprintln!(config, "** Collecting variables...");
    // Collect unbound variables from module
    let mut input_variables = HashMap::new();
    collect_module_variables(&module_3ac, &mut input_variables);

    // Defined variables should not be written to file
    for def in &module.defs {
        if let Pat::Variable(var) = &def.0 .0.v {
            input_variables.remove(&var.id);
        }
    }

    // Put values into a serde JSON friendly format.
    let mut input_variables_m = Map::new();
    for val in input_variables.values() {
        match &val.name {
            None => {}
            Some(x) => {
                input_variables_m.insert(x.to_string(), "?".into());
            }
        }
    }

    qprintln!(config, "** Writing witnesses to file...");
    std::fs::write(
        output,
        serde_json::to_string_pretty(&input_variables_m).unwrap(),
    )
    .unwrap();

    qprintln!(config, "** Witnesses file generation success!");

    Ok(())
}

pub fn generate(generate_commands: &GenerateCommands, config: &Config) -> Result<(), Error> {
    match generate_commands {
        GenerateCommands::WitnessFile(args) => witness_file_cmd(args, config),
    }
}
