use crate::plonk::synth::make_constant;
use crate::plonk::{plonk_compile, plonk_prove, plonk_verify, PlonkCircuitData, PlonkProofData};
use crate::util::{prompt_inputs, read_inputs_from_file};

use ark_bls12_381::{Bls12_381, Fr as BlsScalar};
use ark_ec::PairingEngine;
use ark_poly::polynomial::univariate::DensePolynomial;
use ark_poly_commit::{sonic_pc::SonicKZG10, PolynomialCommitment};
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
use plonk::error::to_pc_error;

use rand_core::OsRng;
use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::path::PathBuf;

use clap::{Args, Subcommand};

type PC = SonicKZG10<Bls12_381, DensePolynomial<BlsScalar>>;
type UniversalParams = <PC as PolynomialCommitment<
    <Bls12_381 as PairingEngine>::Fr,
    DensePolynomial<BlsScalar>,
>>::UniversalParams;

#[derive(Subcommand)]
pub enum PlonkCommands {
    /// Sets up the public parameters required for proving
    Setup(Setup),
    /// Compiles a given source file to a circuit
    Compile(PlonkCompile),
    /// Proves knowledge of witnesses satisfying a circuit
    Prove(PlonkProve),
    /// Verifies that a proof is a correct one
    Verify(PlonkVerify),
}

#[derive(Args)]
pub struct Setup {
    /// Maximum degree exponent of the polynomial commitment scheme
    #[arg(short, long, default_value_t = 10)]
    pub max_degree: u128,
    /// Path to which the public parameters are written
    #[arg(short, long)]
    pub output: PathBuf,
    /// Disable validity checks on the generated public parameters
    #[arg(long)]
    pub unchecked: bool,
}

#[derive(Args)]
pub struct PlonkCompile {
    /// Path to public parameters
    #[arg(short, long)]
    pub universal_params: PathBuf,
    /// Path to source file to be compiled
    #[arg(short, long)]
    pub source: PathBuf,
    /// Path to which circuit is written
    #[arg(short, long)]
    pub output: PathBuf,
    /// Do not perform validity checks on public parameters
    #[arg(long)]
    pub unchecked: bool,
}

#[derive(Args)]
pub struct PlonkProve {
    /// Path to public parameters
    #[arg(short, long)]
    pub universal_params: PathBuf,
    /// Path to circuit on which to construct proof
    #[arg(short, long)]
    pub circuit: PathBuf,
    /// Path to which the proof is written
    #[arg(short, long)]
    pub output: PathBuf,
    /// Do not perform validity checks on public parameters
    #[arg(long)]
    pub unchecked: bool,
    /// Path to prover's input file
    #[arg(short, long)]
    pub inputs: Option<PathBuf>,
}

#[derive(Args)]
pub struct PlonkVerify {
    /// Path to public parameters
    #[arg(short, long)]
    pub universal_params: PathBuf,
    /// Path to circuit on which to construct proof
    #[arg(short, long)]
    pub circuit: PathBuf,
    /// Path to the proof that is being verified
    #[arg(short, long)]
    pub proof: PathBuf,
    /// Do not perform validity checks on public parameters
    #[arg(long)]
    pub unchecked: bool,
}

pub fn plonk(plonk_commands: &PlonkCommands) {
    match plonk_commands {
        PlonkCommands::Setup(args) => setup_plonk_cmd(args),
        PlonkCommands::Compile(args) => compile_plonk_cmd(args),
        PlonkCommands::Prove(args) => prove_plonk_cmd(args),
        PlonkCommands::Verify(args) => verify_plonk_cmd(args),
    }
}

/* Implements the subcommand that generates the public parameters for proofs. */
pub fn setup_plonk_cmd(
    Setup {
        max_degree,
        output,
        unchecked,
    }: &Setup,
) {
    // Generate CRS
    println!("* Setting up public parameters...");
    let pp = PC::setup(1 << max_degree, None, &mut OsRng)
        .map_err(to_pc_error::<BlsScalar, PC>)
        .expect("unable to setup polynomial commitment scheme public parameters");
    let mut pp_file = File::create(output).expect("unable to create public parameters file");
    if *unchecked {
        pp.serialize_unchecked(&mut pp_file)
    } else {
        pp.serialize(&mut pp_file)
    }
    .unwrap();
    println!("* Public parameter setup success!");
}

/* Implements the subcommand that compiles a vamp-ir file into a PLONK circuit.
 */
pub fn compile_plonk_cmd(
    PlonkCompile {
        universal_params,
        source,
        output,
        unchecked,
    }: &PlonkCompile,
) {
    println!("* Reading public parameters...");
    let mut pp_file = File::open(universal_params).expect("unable to load public parameters file");
    let pp = if *unchecked {
        UniversalParams::deserialize_unchecked(&mut pp_file)
    } else {
        UniversalParams::deserialize(&mut pp_file)
    }
    .unwrap();

    let unparsed_file = fs::read_to_string(source).expect("cannot read file");

    println!("* Synthesizing arithmetic circuit...");
    let circuit_data = plonk_compile(unparsed_file, &pp);

    println!("* Serializing circuit to storage...");
    let mut circuit_file = File::create(output).expect("unable to create circuit file");
    circuit_data.write(&mut circuit_file).unwrap();

    println!("* Constraint compilation success!");
}

/* Implements the subcommand that creates a proof from interactively entered
 * inputs. */
pub fn prove_plonk_cmd(
    PlonkProve {
        universal_params,
        circuit,
        output,
        unchecked,
        inputs,
    }: &PlonkProve,
) {
    println!("* Reading arithmetic circuit...");
    let mut circuit_file = File::open(circuit).expect("unable to load circuit file");

    let mut expected_path_to_inputs = circuit.clone();
    expected_path_to_inputs.set_extension("inputs");

    let mut circuit_data = PlonkCircuitData::read(&mut circuit_file).unwrap();

    // Prompt for program inputs
    let var_assignments_ints = match inputs {
        Some(path_to_inputs) => {
            println!(
                "* Reading inputs from file {}...",
                path_to_inputs.to_string_lossy()
            );
            read_inputs_from_file(&circuit_data.circuit.module, path_to_inputs)
        }
        None => {
            if expected_path_to_inputs.exists() {
                println!(
                    "* Reading inputs from file {}...",
                    expected_path_to_inputs.to_string_lossy()
                );
                read_inputs_from_file(&circuit_data.circuit.module, &expected_path_to_inputs)
            } else {
                println!("* Soliciting circuit witnesses...");
                prompt_inputs(&circuit_data.circuit.module)
            }
        }
    };

    let mut var_assignments = HashMap::new();
    for (k, v) in var_assignments_ints {
        var_assignments.insert(k, make_constant(&v));
    }

    // Populate variable definitions
    circuit_data.circuit.populate_variables(var_assignments);

    println!("* Reading public parameters...");
    let mut pp_file = File::open(universal_params).expect("unable to load public parameters file");
    let pp = if *unchecked {
        UniversalParams::deserialize_unchecked(&mut pp_file)
    } else {
        UniversalParams::deserialize(&mut pp_file)
    }
    .unwrap();

    // Start proving witnesses
    println!("* Proving knowledge of witnesses...");
    let proof_data = plonk_prove(&pp, &mut circuit_data);

    println!("* Serializing proof to storage...");
    let mut proof_file = File::create(output).expect("unable to create proof file");
    proof_data.serialize(&mut proof_file).unwrap();

    println!("* Proof generation success!");
}

/* Implements the subcommand that verifies that a proof is correct. */
pub fn verify_plonk_cmd(
    PlonkVerify {
        universal_params,
        circuit,
        proof,
        unchecked,
    }: &PlonkVerify,
) {
    println!("* Reading arithmetic circuit...");
    let mut circuit_file = File::open(circuit).expect("unable to load circuit file");
    let circuit_data = PlonkCircuitData::read(&mut circuit_file).unwrap();

    println!("* Reading zero-knowledge proof...");
    let mut proof_file = File::open(proof).expect("unable to load proof file");
    let proof_data = PlonkProofData::deserialize(&mut proof_file).unwrap();

    println!("* Public inputs:");
    for (var, val) in circuit_data
        .circuit
        .annotate_public_inputs(&circuit_data.vk.1, &proof_data.pi)
        .values()
    {
        println!("{} = {}", var, val);
    }

    println!("* Reading public parameters...");
    let mut pp_file = File::open(universal_params).expect("unable to load public parameters file");
    let pp = if *unchecked {
        UniversalParams::deserialize_unchecked(&mut pp_file)
    } else {
        UniversalParams::deserialize(&mut pp_file)
    }
    .unwrap();

    // Verifier POV
    println!("* Verifying proof validity...");
    let result = plonk_verify(&pp, circuit_data, proof_data);

    if let Ok(()) = result {
        println!("* Zero-knowledge proof is valid");
    } else {
        println!("* Result from verifier: {:?}", result);
    }
}
