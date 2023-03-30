mod ast;
mod transform;
mod file_gen;
mod plonk;
mod halo2;
mod typecheck;
extern crate pest;
#[macro_use]
extern crate pest_derive;

use crate::ast::{Module, VariableId, Pat, parse_prefixed_num};
use crate::transform::{compile, collect_module_variables};

use std::collections::{HashMap, HashSet};

use crate::halo2::cli::{Halo2Commands, halo2};
use crate::plonk::cli::{PlonkCommands, plonk};
use crate::file_gen::cli::{Generate, witness_file_cmd};
use std::io::Write;

use std::fs::File;

use clap::{Parser, Subcommand, ValueEnum};
use std::path::PathBuf;

use std::ops::Neg;
use num_traits::Num;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    backend: Backend,
}

#[derive(Subcommand)]
enum Backend {
    Generate(Generate),
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

/* Read satisfying inputs to the given program from a file. */
fn read_inputs_from_file<F>(annotated: &Module, path_to_inputs: &PathBuf) -> HashMap<VariableId, F>
where F: Num + Neg<Output = F>, <F as num_traits::Num>::FromStrRadixErr: std::fmt::Debug {
    let inputs = File::open(path_to_inputs)
        .expect("Could not open inputs file");

    // Read the user-supplied inputs from the file
    let named_assignments: HashMap<String, String> = serde_json::from_reader(inputs).unwrap();

    // Get the expected inputs from the circuit module
    let mut input_variables = HashMap::new();
    collect_module_variables(&annotated, &mut input_variables);

    // Defined variables should not be requested from user
    for def in &annotated.defs {
        if let Pat::Variable(var) = &def.0.0.v {
            input_variables.remove(&var.id);
        }
    }

    let mut variable_assignments = HashMap::new();

    // Check that the user supplied the expected inputs
    for (id, expected_var) in input_variables {
        variable_assignments.insert(
            id,
            parse_prefixed_num(&named_assignments[&expected_var.name.unwrap()].clone())
                .expect("input not an integer")
        );
    }

    variable_assignments
    
}

/* Prompt for satisfying inputs to the given program. */
fn prompt_inputs<F>(annotated: &Module) -> HashMap<VariableId, F> where F: Num + Neg<Output = F>, <F as num_traits::Num>::FromStrRadixErr: std::fmt::Debug {
    let mut input_variables = HashMap::new();
    collect_module_variables(&annotated, &mut input_variables);
    // Defined variables should not be requested from user
    for def in &annotated.defs {
        if let Pat::Variable(var) = &def.0.0.v {
            input_variables.remove(&var.id);
        }
    }
    // Collect all public variables in order to enable annotations
    let mut public_variables = HashSet::new();
    for var in &annotated.pubs {
        public_variables.insert(var.id);
    }

    let mut var_assignments = HashMap::new();

    // Solicit input variables from user and solve for choice point values
    for (id, var) in input_variables {
        let visibility = if public_variables.contains(&id) {
            "(public)"
        } else {
            "(private)"
        };
        print!("** {} {}: ", var, visibility);
        std::io::stdout().flush().expect("flush failed!");
        let mut input_line = String::new();
        std::io::stdin()
            .read_line(&mut input_line)
            .expect("failed to read input");
        let x = parse_prefixed_num(input_line.trim())
            .expect("input not an integer");
        var_assignments.insert(id, x);
    }
    var_assignments
}

/* Main entry point for vamp-ir compiler, prover, and verifier. */
fn main() {
    let cli = Cli::parse();
    match &cli.backend {
        Backend::Generate(args) => witness_file_cmd(args),
        Backend::Plonk(plonk_commands) => plonk(plonk_commands),
        Backend::Halo2(halo2_commands) => halo2(halo2_commands),
    }
}
