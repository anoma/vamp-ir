use crate::ast::{Module, Pat, InfixOp};
use crate::transform::{compile, collect_module_variables, FieldOps};

use std::collections::HashMap;
use serde_json::Map;

use std::fs; 

use clap::Args;
use std::path::PathBuf;

use num_bigint::BigInt;

#[derive(Args)]
pub struct Generate {
    /// Path to source file that witnesses come from
    #[arg(short, long)]
    source: PathBuf,
    /// Path to which the witness file is written
    #[arg(short, long)]
    output: PathBuf,
}

// Trivial FieldOps for witness file generation
impl FieldOps for () {
    fn canonical(&self, a: BigInt) -> BigInt { a }
    fn negate(&self, a: BigInt) -> BigInt { a }
    fn infix(&self, _: InfixOp, a: BigInt, _: BigInt) -> BigInt { a }
}

/* Implements the subcommand that writes witnesses to a JSON file. */
pub fn witness_file_cmd(Generate { source, output }: &Generate) {
    println!("* Type Checking...");
    let unparsed_file = fs::read_to_string(source).expect("cannot read file");
    let module = Module::parse(&unparsed_file).unwrap();
    let module_3ac = compile(module.clone(), &());
    
    // Collect unbound variables from module
    let mut input_variables = HashMap::new();
    collect_module_variables(&module_3ac, &mut input_variables);
    
    // Defined variables should not be written to file
    for def in &module.defs {
        if let Pat::Variable(var) = &def.0.0.v {
            input_variables.remove(&var.id);
        }
    }
    
    // Put values into a serde JSON friendly format.
    let mut input_variables_m = Map::new();
    for val in input_variables.values() {
      match &val.name {
        None => {},
        Some(x) => { input_variables_m.insert(x.to_string(), "?".into()); }
      }
    }
    
    println!("* Writing witnesses to file...");
    std::fs::write(
        output,
        serde_json::to_string_pretty(&input_variables_m).unwrap(),
    ).unwrap();

    println!("* Witnesses file generation success!");
}

