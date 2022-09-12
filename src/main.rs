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
use std::fs::File;
use ark_serialize::{CanonicalSerialize, CanonicalDeserialize};
use ark_ec::PairingEngine;
use plonk_core::proof_system::{ProverKey, VerifierKey, Proof};
use plonk_core::proof_system::pi::PublicInputs;

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
        println!("usage 1: vamp-ir setup");
        println!("usage 2: vamp-ir compile");
        println!("usage 3: vamp-ir prove");
        println!("usage 4: vamp-ir verify");
        return;
    }

    type PC = SonicKZG10<Bls12_381, DensePolynomial<BlsScalar>>;
    if args[1] == "setup" {
        if args.len() < 3 {
            println!("usage: vamp-ir setup params.pp");
            println!("randomly generates public parameters and saves them into params.pp");
            return;
        }
        // Generate CRS
        let pp = PC::setup(1 << 10, None, &mut OsRng)
            .map_err(to_pc_error::<BlsScalar, PC>)
            .expect("unable to setup polynomial commitment scheme public parameters");
        let mut pp_file = File::create(args[2].clone())
            .expect("unable to create public parameters file");
        pp.serialize(&mut pp_file).unwrap();
    } else if args[1] == "compile" {
        if args.len() < 5 {
            println!("usage: vamp-ir compile source.pir params.pp circuit.plonk");
            println!("reads the vamp-ir source code in source.pir and the public\n\
                      parameters at params.pp and compiles them into a circuit which\n\
                      is stored at circuit.plonk .");
            return;
        }
        let mut pp_file = File::open(args[3].clone())
            .expect("unable to load public parameters file");
        let pp = <PC as PolynomialCommitment<<Bls12_381 as PairingEngine>::Fr, DensePolynomial<BlsScalar>>>::UniversalParams::deserialize(&mut pp_file).unwrap();
        
        let unparsed_file = fs::read_to_string(args[2].clone()).expect("cannot read file");
        let module = Module::parse(&unparsed_file).unwrap();
        let module_3ac = compile(module);
        println!("{}\n", module_3ac);
        
        let mut circuit = PlonkModule::<BlsScalar, JubJubParameters>::new(module_3ac.clone());
        // Compile the circuit
        let (pk_p, vk) = circuit.compile::<PC>(&pp).expect("unable to compile circuit");
        let mut circuit_file = File::create(args[4].clone()).expect("unable to create circuit file");
        pk_p.serialize(&mut circuit_file).unwrap();
        vk.serialize(&mut circuit_file).unwrap();
        bincode::encode_into_std_write(
            circuit,
            &mut circuit_file,
            bincode::config::standard(),
        ).unwrap();
    } else if args[1] == "prove" {
        if args.len() < 5 {
            println!("usage: vamp-ir prove circuit.plonk params.pp proof.plonk");
            println!("reads the plonk circuit at circuit.plonk and the public\n\
                      parameters at params.pp, then interactively requests for\n\
                      private inputs, and then stores the proof at proof.plonk. Note\n\
                      that this subcommand makes no attempt to verify supplied inputs.");
            return;
        }
        let mut circuit_file = File::open(args[2].clone())
            .expect("unable to load circuit file");
        let pk_p = ProverKey::<_>::deserialize(&mut circuit_file).unwrap();
        let _vk = VerifierKey::<BlsScalar, PC>::deserialize(&mut circuit_file).unwrap();
        let mut circuit: PlonkModule::<BlsScalar, JubJubParameters> = bincode::decode_from_std_read(&mut circuit_file, bincode::config::standard()).unwrap();
        
        let mut pp_file = File::open(args[3].clone())
            .expect("unable to load public parameters file");
        let pp = <PC as PolynomialCommitment<<Bls12_381 as PairingEngine>::Fr, DensePolynomial<BlsScalar>>>::UniversalParams::deserialize(&mut pp_file).unwrap();
        // Prover POV
        println!("Proving...");
        // Prompt for program inputs
        let var_assignments = prompt_inputs(&circuit.module);
        // Populate variable definitions
        circuit.populate_variables(var_assignments);
        // Start proving witnesses
        let (proof, pi) = circuit.gen_proof::<PC>(&pp, pk_p, b"Test").unwrap();

        let mut proof_file = File::create(args[4].clone())
            .expect("unable to create proof file");
        proof.serialize(&mut proof_file).unwrap();
        pi.serialize(&mut proof_file).unwrap();
    } else if args[1] == "verify" {
        if args.len() < 5 {
            println!("usage: vamp-ir verify circuit.plonk params.pp proof.plonk");
            println!("reads the plonk circuit at circuit.plonk, the public\n\
                      parameters at params.pp, and the proof at proof.plonk and\n\
                      verifies whether the proof is a correct one.");
            return;
        }
        let mut circuit_file = File::open(args[2].clone())
            .expect("unable to load circuit file");
        let _pk_p = ProverKey::<BlsScalar>::deserialize(&mut circuit_file).unwrap();
        let vk = VerifierKey::<_, _>::deserialize(&mut circuit_file).unwrap();
        let _circuit: PlonkModule::<BlsScalar, JubJubParameters> = bincode::decode_from_std_read(&mut circuit_file, bincode::config::standard()).unwrap();

        let mut proof_file = File::open(args[4].clone())
            .expect("unable to load proof file");
        let proof = Proof::<_, _>::deserialize(&mut proof_file).unwrap();
        let pi = PublicInputs::<_>::deserialize(&mut proof_file).unwrap();

        let mut pp_file = File::open(args[3].clone())
            .expect("unable to load public parameters file");
        let pp = <PC as PolynomialCommitment<<Bls12_381 as PairingEngine>::Fr, DensePolynomial<BlsScalar>>>::UniversalParams::deserialize(&mut pp_file).unwrap();
        
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
        if let Ok(()) = verifier_result {
            println!("proof is valid");
        } else {
            println!("verifier result: {:?}", verifier_result);
        }
    } else {
        println!("usage 1: vamp-ir setup");
        println!("usage 2: vamp-ir compile");
        println!("usage 3: vamp-ir prove");
        println!("usage 4: vamp-ir verify");
        return;
    }
}
