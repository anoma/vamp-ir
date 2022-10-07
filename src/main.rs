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
use bincode::error::{DecodeError, EncodeError};
use ark_serialize::{Read, SerializationError};

type PC = SonicKZG10<Bls12_381, DensePolynomial<BlsScalar>>;

/* Captures all the data required to use a circuit. */
struct CircuitData {
    pk_p: ProverKey::<BlsScalar>,
    vk: VerifierKey::<BlsScalar, PC>,
    circuit: PlonkModule::<BlsScalar, JubJubParameters>,
}

impl CircuitData {
    fn read<R>(mut reader: R) -> Result<Self, DecodeError>
    where R: std::io::Read {
        let pk_p = ProverKey::<BlsScalar>::deserialize(&mut reader)
            .map_err(|x| DecodeError::OtherString(x.to_string()))?;
        let vk = VerifierKey::<_, _>::deserialize(&mut reader)
            .map_err(|x| DecodeError::OtherString(x.to_string()))?;
        let circuit: PlonkModule::<BlsScalar, JubJubParameters> =
            bincode::decode_from_std_read(&mut reader, bincode::config::standard())?;
        Ok(Self { pk_p, vk, circuit })
    }

    fn write<W>(&self, mut writer: W) -> Result<(), EncodeError>
    where W: std::io::Write {
        self.pk_p.serialize(&mut writer)
            .map_err(|x| EncodeError::OtherString(x.to_string()))?;
        self.vk.serialize(&mut writer)
            .map_err(|x| EncodeError::OtherString(x.to_string()))?;
        bincode::encode_into_std_write(
            &self.circuit,
            &mut writer,
            bincode::config::standard(),
        )?;
        Ok(())
    }
}

/* Captures all the data generated from proving circuit witnesses. */
#[derive(CanonicalSerialize, CanonicalDeserialize)]
struct ProofData {
    proof: Proof<BlsScalar, PC>,
    pi: PublicInputs<BlsScalar>,
}

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
        print!("** {}: ", var);
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

/* Implements the subcommand that tells user how to use this program. */
fn usage_cmd() {
    println!("Vampir Aliased Multivariate Polynomial Intermediate Representation");
    println!("usage 1: vamp-ir setup");
    println!("usage 2: vamp-ir compile");
    println!("usage 3: vamp-ir prove");
    println!("usage 4: vamp-ir verify");
}

/* Implements the subcommand that generates the public parameters for proofs. */
fn setup_cmd(args: &[String]) {
    if args.len() < 1 {
        println!("usage: vamp-ir setup params.pp");
        println!("randomly generates public parameters and saves them into params.pp");
        return;
    }
    // Generate CRS
    println!("* Setting up public parameters...");
    let pp = PC::setup(1 << 10, None, &mut OsRng)
        .map_err(to_pc_error::<BlsScalar, PC>)
        .expect("unable to setup polynomial commitment scheme public parameters");
    let mut pp_file = File::create(args[0].clone())
        .expect("unable to create public parameters file");
    pp.serialize(&mut pp_file).unwrap();
    println!("* Public parameter setup success!");
}

/* Implements the subcommand that compiles a vamp-ir file into a PLONK circuit.
 */
fn compile_cmd(args: &[String]) {
    if args.len() < 3 {
        println!("usage: vamp-ir compile source.pir params.pp circuit.plonk");
        println!("reads the vamp-ir source code in source.pir and the public\n\
                  parameters at params.pp and compiles them into a circuit which\n\
                  is stored at circuit.plonk .");
        return;
    }
    println!("* Reading public parameters...");
    let mut pp_file = File::open(args[1].clone())
        .expect("unable to load public parameters file");
    let pp = <PC as PolynomialCommitment<<Bls12_381 as PairingEngine>::Fr, DensePolynomial<BlsScalar>>>::UniversalParams::deserialize(&mut pp_file).unwrap();

    println!("* Compiling constraints...");
    let unparsed_file = fs::read_to_string(args[0].clone()).expect("cannot read file");
    let module = Module::parse(&unparsed_file).unwrap();
    let module_3ac = compile(module);

    println!("* Synthesizing arithmetic circuit...");
    let mut circuit = PlonkModule::<BlsScalar, JubJubParameters>::new(module_3ac.clone());
    // Compile the circuit
    let (pk_p, vk) = circuit.compile::<PC>(&pp)
        .expect("unable to compile circuit");
    println!("* Serializing circuit to storage...");
    let mut circuit_file = File::create(args[2].clone())
        .expect("unable to create circuit file");
    CircuitData { pk_p, vk, circuit }.write(&mut circuit_file).unwrap();

    println!("* Constraint compilation success!");
}

/* Implements the subcommand that creates a proof from interactively entered
 * inputs. */
fn prove_cmd(args: &[String]) {
    if args.len() < 3 {
        println!("usage: vamp-ir prove circuit.plonk params.pp proof.plonk");
        println!("reads the plonk circuit at circuit.plonk and the public\n\
                  parameters at params.pp, then interactively requests for\n\
                  private inputs, and then stores the proof at proof.plonk. Note\n\
                  that this subcommand makes no attempt to verify supplied inputs.");
        return;
    }
    println!("* Reading arithmetic circuit...");
    let mut circuit_file = File::open(args[0].clone())
        .expect("unable to load circuit file");
    let CircuitData { pk_p, vk: _vk, mut circuit} =
        CircuitData::read(&mut circuit_file).unwrap();

    println!("* Reading public parameters...");
    let mut pp_file = File::open(args[1].clone())
        .expect("unable to load public parameters file");
    let pp = <PC as PolynomialCommitment<<Bls12_381 as PairingEngine>::Fr, DensePolynomial<BlsScalar>>>::UniversalParams::deserialize(&mut pp_file).unwrap();
    // Prover POV
    println!("* Soliciting circuit witnesses...");
    // Prompt for program inputs
    let var_assignments = prompt_inputs(&circuit.module);
    // Populate variable definitions
    circuit.populate_variables(var_assignments);
    // Start proving witnesses
    println!("* Proving knowledge of witnesses...");
    let (proof, pi) = circuit.gen_proof::<PC>(&pp, pk_p, b"Test").unwrap();

    println!("* Serializing proof to storage...");
    let mut proof_file = File::create(args[2].clone())
        .expect("unable to create proof file");
    ProofData { proof, pi }.serialize(&mut proof_file).unwrap();

    println!("* Proof generation success!");
}

/* Implements the subcommand that verifies that a proof is correct. */
fn verify_cmd(args: &[String]) {
    if args.len() < 3 {
        println!("usage: vamp-ir verify circuit.plonk params.pp proof.plonk");
        println!("reads the plonk circuit at circuit.plonk, the public\n\
                  parameters at params.pp, and the proof at proof.plonk and\n\
                  verifies whether the proof is a correct one.");
        return;
    }
    println!("* Reading arithmetic circuit...");
    let mut circuit_file = File::open(args[0].clone())
        .expect("unable to load circuit file");
    let CircuitData { pk_p: _pk_p, vk, circuit: _circuit} =
        CircuitData::read(&mut circuit_file).unwrap();

    println!("* Reading zero-knowledge proof...");
    let mut proof_file = File::open(args[2].clone())
        .expect("unable to load proof file");
    let ProofData { proof, pi } = ProofData::deserialize(&mut proof_file).unwrap();

    println!("* Reading public parameters...");
    let mut pp_file = File::open(args[1].clone())
        .expect("unable to load public parameters file");
    let pp = <PC as PolynomialCommitment<<Bls12_381 as PairingEngine>::Fr, DensePolynomial<BlsScalar>>>::UniversalParams::deserialize(&mut pp_file).unwrap();
    
    // Verifier POV
    println!("* Verifying proof validity...");
    let verifier_data = VerifierData::new(vk, pi);
    let verifier_result = verify_proof::<BlsScalar, JubJubParameters, PC>(
        &pp,
        verifier_data.key,
        &proof,
        &verifier_data.pi,
        b"Test",
    );
    if let Ok(()) = verifier_result {
        println!("* Zero-knowledge proof is valid");
    } else {
        println!("* Result from verifier: {:?}", verifier_result);
    }
}

/* Main entry point for vamp-ir compiler, prover, and verifier. */
fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() < 2 {
        usage_cmd();
    } else if args[1] == "setup" {
        setup_cmd(&args[2..]);
    } else if args[1] == "compile" {
        compile_cmd(&args[2..]);
    } else if args[1] == "prove" {
        prove_cmd(&args[2..]);
    } else if args[1] == "verify" {
        verify_cmd(&args[2..]);
    } else {
        usage_cmd();
    }
}
