use crate::error::Error;
use crate::halo2::api::{prove_from_int_variable_assignments, ProofDataCliHalo2};
use crate::halo2::synth::verifier;

use crate::qprintln;
use crate::util::{get_circuit_assignments, prompt_inputs, read_inputs_from_file, Config};

use halo2_proofs::plonk::keygen_vk;

use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};

use clap::{Args, Subcommand};

use crate::halo2::api::HaloCircuitData;
use ff::PrimeField;
use halo2_proofs::pasta::Fp;
use num_bigint::BigInt;
use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::ops::Deref;
use std::path::PathBuf;

#[derive(Subcommand)]
pub enum Halo2Commands {
    /// Compiles a given source file to a circuit
    Compile(Halo2Compile),
    /// Proves knowledge of witnesses satisfying a circuit
    Prove(Halo2Prove),
    /// Verifies that a proof is a correct one
    Verify(Halo2Verify),
}

#[derive(Args)]
pub struct Halo2Compile {
    /// Path to source file to be compiled
    #[arg(short, long)]
    source: PathBuf,
    /// Path to which circuit is written
    #[arg(short, long)]
    output: PathBuf,
}

#[derive(Args)]
pub struct Halo2Prove {
    /// Path to circuit on which to construct proof[[bin]]
    #[arg(short, long)]
    circuit: PathBuf,
    /// Path to which the proof is written
    #[arg(short, long)]
    output: PathBuf,
    /// Path to prover's input file
    #[arg(short, long)]
    inputs: Option<PathBuf>,
}

#[derive(Args)]
pub struct Halo2Verify {
    /// Path to circuit on which to construct proof
    #[arg(short, long)]
    circuit: PathBuf,
    /// Path to the proof that is being verified
    #[arg(short, long)]
    proof: PathBuf,
}

/* Implements the subcommand that compiles a vamp-ir file into a Halo2 circuit.
 */
fn compile_halo2_cmd(
    Halo2Compile { source, output }: &Halo2Compile,
    config: &Config,
) -> Result<(), Error> {
    let source = fs::read_to_string(source).expect("cannot read file");
    let halo_circuit_data = crate::halo2::api::compile(source, config)?;
    let mut circuit_file = File::create(output).expect("unable to create circuit file");
    halo_circuit_data.write(&mut circuit_file).unwrap();

    qprintln!(config, "* Constraint compilation success!");

    Ok(())
}

fn prove_from_file(
    path_to_inputs: &PathBuf,
    circuit_data: &HaloCircuitData,
    config: &Config,
) -> Result<ProofDataCliHalo2, Error> {
    qprintln!(
        config,
        "* Reading inputs from file {}...",
        path_to_inputs.to_string_lossy()
    );
    let raw_inputs: HashMap<String, BigInt> = read_inputs_from_file(path_to_inputs).unwrap();
    let inputs =
        get_circuit_assignments::<BigInt>(circuit_data.circuit.module.deref(), &raw_inputs)?;
    prove_from_int_variable_assignments(circuit_data, &inputs, config)
}

/* Implements the subcommand that creates a proof from interactively entered
 * inputs. */
fn prove_halo2_cmd(
    Halo2Prove {
        circuit,
        output,
        inputs,
    }: &Halo2Prove,
    config: &Config,
) -> Result<(), Error> {
    qprintln!(config, "* Reading arithmetic circuit...");
    let mut circuit_file = File::open(circuit).expect("unable to load circuit file");

    let mut expected_path_to_inputs = circuit.clone();
    expected_path_to_inputs.set_extension("inputs");

    let circuit_data = HaloCircuitData::read(&mut circuit_file).unwrap();

    // Start proving witnesses
    qprintln!(config, "* Proving knowledge of witnesses...");
    let proof_data = match inputs {
        Some(path_to_inputs) => prove_from_file(path_to_inputs, &circuit_data, config),
        None => {
            if expected_path_to_inputs.exists() {
                prove_from_file(&expected_path_to_inputs, &circuit_data, config)
            } else {
                qprintln!(config, "* Soliciting circuit witnesses...");
                let inputs = prompt_inputs(&circuit_data.circuit.module);
                prove_from_int_variable_assignments(&circuit_data, &inputs, config)
            }
        }
    }?;

    qprintln!(config, "* Serializing proof to storage...");
    let mut proof_file = File::create(output).expect("unable to create proof file");
    proof_data
        .serialize(&mut proof_file)
        .expect("Proof serialization failed");

    qprintln!(config, "* Proof generation success!");
    Ok(())
}

/* Implements the subcommand that verifies that a proof is correct. */
fn verify_halo2_cmd(
    Halo2Verify { circuit, proof }: &Halo2Verify,
    config: &Config,
) -> Result<(), Error> {
    qprintln!(config, "* Reading arithmetic circuit...");
    let circuit_file = File::open(circuit).expect("unable to load circuit file");
    let HaloCircuitData { params, circuit } = HaloCircuitData::read(&circuit_file).unwrap();

    qprintln!(config, "* Generating verifying key...");
    let vk = keygen_vk(&params, &circuit)?;

    qprintln!(config, "* Reading zero-knowledge proof...");
    let mut proof_file = File::open(proof).expect("unable to load proof file");
    let ProofDataCliHalo2 {
        proof,
        public_inputs,
    } = ProofDataCliHalo2::deserialize(&mut proof_file).unwrap();

    let instances_vec: Vec<Fp> = public_inputs
        .chunks(32)
        .map(|chunk| {
            let mut array = [0u8; 32];
            array.copy_from_slice(&chunk[..32]);
            Fp::from_repr(array).unwrap()
        })
        .collect();

    let instances: &[Fp] = instances_vec.as_slice();

    // Veryfing proof
    qprintln!(config, "* Verifying proof validity...");
    let verifier_result = verifier(&params, &vk, &proof, instances);

    if let Ok(()) = verifier_result {
        qprintln!(config, "* Zero-knowledge proof is valid");
        Ok(())
    } else {
        qprintln!(config, "* Result from verifier: {:?}", verifier_result);
        Err(Error::ProofVerificationFailure)
    }
}

pub fn halo2(halo2_commands: &Halo2Commands, config: &Config) -> Result<(), Error> {
    match halo2_commands {
        Halo2Commands::Compile(args) => compile_halo2_cmd(args, config),
        Halo2Commands::Prove(args) => prove_halo2_cmd(args, config),
        Halo2Commands::Verify(args) => verify_halo2_cmd(args, config),
    }
}
