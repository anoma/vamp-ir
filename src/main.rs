mod ast;
mod transform;
mod synth;
mod typecheck;
extern crate pest;
#[macro_use]
extern crate pest_derive;
use std::fs;
use crate::ast::{Module, VariableId, Pattern};
use crate::transform::{compile, collect_module_variables};
use ark_bls12_381::{Bls12_381, Fr as BlsScalar};
use ark_ed_on_bls12_381::EdwardsParameters as JubJubParameters;
use plonk_core::circuit::verify_proof;
use ark_poly_commit::{sonic_pc::SonicKZG10, PolynomialCommitment};
use ark_poly::polynomial::univariate::DensePolynomial;
use rand_core::OsRng;
use plonk::error::to_pc_error;
use std::collections::HashMap;
use std::io::Write;
use plonk_core::prelude::VerifierData;
use crate::synth::PlonkModule;
use plonk_core::circuit::Circuit;
use ark_ff::PrimeField;

/* Prompt for satisfying inputs to the given program. */
fn prompt_inputs<F>(annotated: &Module) -> HashMap<VariableId, F> where F: PrimeField {
    let mut input_variables = HashMap::new();
    collect_module_variables(&annotated, &mut input_variables);
    // Defined variables should not be requested from user
    for def in &annotated.defs {
        if let Pattern::Variable(var) = &def.0.0 {
            input_variables.remove(&var.id);
        }
    }
    
    let mut var_assignments = HashMap::new();
    // Solicit input variables from user and solve for choice point values
    for (id, var) in input_variables {
        print!("{}: ", var);
        std::io::stdout().flush().expect("flush failed!");
        let mut input_line = String::new();
        std::io::stdin()
            .read_line(&mut input_line)
            .expect("failed to read input");
        let x: F = if let Ok(x) = input_line.trim().parse() {
            x
        } else {
            panic!("input not an integer");
        };
        var_assignments.insert(id, x);
    }
    var_assignments
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() < 2 {
        panic!("please supply the vamp-ir file path");
    }
    let unparsed_file = fs::read_to_string(args[1].clone()).expect("cannot read file");
    let module = Module::parse(&unparsed_file).unwrap();
    let module_3ac = compile(module);
    println!("{}\n", module_3ac);

    // Generate CRS
    type PC = SonicKZG10<Bls12_381, DensePolynomial<BlsScalar>>;
    let pp = PC::setup(1 << 10, None, &mut OsRng)
        .map_err(to_pc_error::<BlsScalar, PC>)
        .expect("unable to setup polynomial commitment scheme public parameters");
    let mut circuit = PlonkModule::<BlsScalar, JubJubParameters>::new(module_3ac.clone());
    // Compile the circuit
    let (pk_p, vk) = circuit.compile::<PC>(&pp).expect("unable to compile circuit");
    
    // Prover POV
    println!("Proving...");
    let mut circuit = PlonkModule::<BlsScalar, JubJubParameters>::new(module_3ac.clone());
    // Prompt for program inputs
    let var_assignments = prompt_inputs(&module_3ac);
    // Populate variable definitions
    circuit.populate_variables(var_assignments);
    // Start proving witnesses
    let (proof, pi) = circuit.gen_proof::<PC>(&pp, pk_p, b"Test").unwrap();

    // Verifier POV
    println!("Verifying...");
    let verifier_data = VerifierData::new(vk, pi);
    let verifier_result = verify_proof::<BlsScalar, JubJubParameters, PC>(
        &pp,
        verifier_data.key,
        &proof,
        &verifier_data.pi,
        b"Test",
    );
    println!("Verifier Result: {:?}", verifier_result);
}
