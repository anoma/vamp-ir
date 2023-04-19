use crate::ast::Module;
use crate::plonk::synth::{make_constant, PlonkModule, PrimeFieldOps};
use crate::transform::compile;
use crate::util::{prompt_inputs, read_inputs_from_file};

use ark_bls12_381::{Bls12_381, Fr as BlsScalar};
use ark_ec::PairingEngine;
use ark_ed_on_bls12_381::EdwardsParameters as JubJubParameters;
use ark_poly::polynomial::univariate::DensePolynomial;
use ark_poly_commit::{sonic_pc::SonicKZG10, PolynomialCommitment};
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize, Read, SerializationError};
use plonk::error::to_pc_error;
use plonk_core::circuit::{verify_proof, Circuit};
use plonk_core::prelude::VerifierData;
use plonk_core::proof_system::pi::PublicInputs;
use plonk_core::proof_system::{Proof, ProverKey, VerifierKey};

use bincode::error::{DecodeError, EncodeError};
use rand_core::OsRng;
use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::io::Write;
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
    max_degree: u128,
    /// Path to which the public parameters are written
    #[arg(short, long)]
    output: PathBuf,
    /// Disable validity checks on the generated public parameters
    #[arg(long)]
    unchecked: bool,
}

#[derive(Args)]
pub struct PlonkCompile {
    /// Path to public parameters
    #[arg(short, long)]
    universal_params: PathBuf,
    /// Path to source file to be compiled
    #[arg(short, long)]
    source: PathBuf,
    /// Path to which circuit is written
    #[arg(short, long)]
    output: PathBuf,
    /// Do not perform validity checks on public parameters
    #[arg(long)]
    unchecked: bool,
}

#[derive(Args)]
pub struct PlonkProve {
    /// Path to public parameters
    #[arg(short, long)]
    universal_params: PathBuf,
    /// Path to circuit on which to construct proof
    #[arg(short, long)]
    circuit: PathBuf,
    /// Path to which the proof is written
    #[arg(short, long)]
    output: PathBuf,
    /// Do not perform validity checks on public parameters
    #[arg(long)]
    unchecked: bool,
    /// Path to prover's input file
    #[arg(short, long)]
    inputs: Option<PathBuf>,
}

#[derive(Args)]
pub struct PlonkVerify {
    /// Path to public parameters
    #[arg(short, long)]
    universal_params: PathBuf,
    /// Path to circuit on which to construct proof
    #[arg(short, long)]
    circuit: PathBuf,
    /// Path to the proof that is being verified
    #[arg(short, long)]
    proof: PathBuf,
    /// Do not perform validity checks on public parameters
    #[arg(long)]
    unchecked: bool,
}

pub fn plonk(plonk_commands: &PlonkCommands) {
    match plonk_commands {
        PlonkCommands::Setup(args) => setup_plonk_cmd(args),
        PlonkCommands::Compile(args) => compile_plonk_cmd(args),
        PlonkCommands::Prove(args) => prove_plonk_cmd(args),
        PlonkCommands::Verify(args) => verify_plonk_cmd(args),
    }
}

/* Captures all the data required to use a PLONK circuit. */
struct PlonkCircuitData {
    pk_p: ProverKey<BlsScalar>,
    vk: (VerifierKey<BlsScalar, PC>, Vec<usize>),
    circuit: PlonkModule<BlsScalar, JubJubParameters>,
}

impl PlonkCircuitData {
    fn read<R>(mut reader: R) -> Result<Self, DecodeError>
    where
        R: std::io::Read,
    {
        let pk_p = ProverKey::<BlsScalar>::deserialize(&mut reader)
            .map_err(|x| DecodeError::OtherString(x.to_string()))?;
        let vk = <(VerifierKey<_, _>, Vec<usize>)>::deserialize(&mut reader)
            .map_err(|x| DecodeError::OtherString(x.to_string()))?;
        let circuit: PlonkModule<BlsScalar, JubJubParameters> =
            bincode::decode_from_std_read(&mut reader, bincode::config::standard())?;
        Ok(Self { pk_p, vk, circuit })
    }

    fn write<W>(&self, mut writer: W) -> Result<(), EncodeError>
    where
        W: std::io::Write,
    {
        self.pk_p
            .serialize(&mut writer)
            .map_err(|x| EncodeError::OtherString(x.to_string()))?;
        self.vk
            .serialize(&mut writer)
            .map_err(|x| EncodeError::OtherString(x.to_string()))?;
        bincode::encode_into_std_write(&self.circuit, &mut writer, bincode::config::standard())?;
        Ok(())
    }
}

/* Captures all the data generated from proving circuit witnesses. */
#[derive(CanonicalSerialize, CanonicalDeserialize)]
struct ProofData {
    proof: Proof<BlsScalar, PC>,
    pi: PublicInputs<BlsScalar>,
}

/* Implements the subcommand that generates the public parameters for proofs. */
fn setup_plonk_cmd(
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
fn compile_plonk_cmd(
    PlonkCompile {
        universal_params,
        source,
        output,
        unchecked,
    }: &PlonkCompile,
) {
    println!("* Compiling constraints...");
    let unparsed_file = fs::read_to_string(source).expect("cannot read file");
    let module = Module::parse(&unparsed_file).unwrap();
    let module_3ac = compile(module, &PrimeFieldOps::<BlsScalar>::default());

    println!("* Reading public parameters...");
    let mut pp_file = File::open(universal_params).expect("unable to load public parameters file");
    let pp = if *unchecked {
        UniversalParams::deserialize_unchecked(&mut pp_file)
    } else {
        UniversalParams::deserialize(&mut pp_file)
    }
    .unwrap();

    println!("* Synthesizing arithmetic circuit...");
    let mut circuit = PlonkModule::<BlsScalar, JubJubParameters>::new(module_3ac.clone());
    // Compile the circuit
    let (pk_p, vk) = circuit
        .compile::<PC>(&pp)
        .expect("unable to compile circuit");
    println!("* Serializing circuit to storage...");
    let mut circuit_file = File::create(output).expect("unable to create circuit file");
    PlonkCircuitData { pk_p, vk, circuit }
        .write(&mut circuit_file)
        .unwrap();

    println!("* Constraint compilation success!");
}

/* Implements the subcommand that creates a proof from interactively entered
 * inputs. */
fn prove_plonk_cmd(
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

    let PlonkCircuitData {
        pk_p,
        vk: _vk,
        mut circuit,
    } = PlonkCircuitData::read(&mut circuit_file).unwrap();

    // Prompt for program inputs
    let var_assignments_ints = match inputs {
        Some(path_to_inputs) => {
            println!(
                "* Reading inputs from file {}...",
                path_to_inputs.to_string_lossy()
            );
            read_inputs_from_file(&circuit.module, path_to_inputs)
        }
        None => {
            if expected_path_to_inputs.exists() {
                println!(
                    "* Reading inputs from file {}...",
                    expected_path_to_inputs.to_string_lossy()
                );
                read_inputs_from_file(&circuit.module, &expected_path_to_inputs)
            } else {
                println!("* Soliciting circuit witnesses...");
                prompt_inputs(&circuit.module)
            }
        }
    };

    let mut var_assignments = HashMap::new();
    for (k, v) in var_assignments_ints {
        var_assignments.insert(k, make_constant(&v));
    }

    // Populate variable definitions
    circuit.populate_variables(var_assignments);

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
    let (proof, pi) = circuit.gen_proof::<PC>(&pp, pk_p, b"Test").unwrap();

    println!("* Serializing proof to storage...");
    let mut proof_file = File::create(output).expect("unable to create proof file");
    ProofData { proof, pi }.serialize(&mut proof_file).unwrap();

    println!("* Proof generation success!");
}

/* Implements the subcommand that verifies that a proof is correct. */
fn verify_plonk_cmd(
    PlonkVerify {
        universal_params,
        circuit,
        proof,
        unchecked,
    }: &PlonkVerify,
) {
    println!("* Reading arithmetic circuit...");
    let mut circuit_file = File::open(circuit).expect("unable to load circuit file");
    let PlonkCircuitData {
        pk_p: _pk_p,
        vk,
        circuit,
    } = PlonkCircuitData::read(&mut circuit_file).unwrap();

    println!("* Reading zero-knowledge proof...");
    let mut proof_file = File::open(proof).expect("unable to load proof file");
    let ProofData { proof, pi } = ProofData::deserialize(&mut proof_file).unwrap();

    println!("* Public inputs:");
    for (var, val) in circuit.annotate_public_inputs(&vk.1, &pi).values() {
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
    let verifier_data = VerifierData::new(vk.0, pi);
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
