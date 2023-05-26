use crate::ast::Module;
use crate::error::Error;
use crate::halo2::synth::{keygen, make_constant, prover, verifier, Halo2Module, PrimeFieldOps};
use crate::qprintln;
use crate::transform::compile;
use crate::util::{prompt_inputs, read_inputs_from_file, Config};

use halo2_proofs::pasta::{EqAffine, Fp};
use halo2_proofs::plonk::keygen_vk;
use halo2_proofs::poly::commitment::Params;

use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
use ark_serialize::{Read, SerializationError};
use std::io::Write;

use clap::{Args, Subcommand};

use bincode::error::{DecodeError, EncodeError};
use ff::PrimeField;
use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::path::PathBuf;
use std::rc::Rc;

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
    qprintln!(config, "* Compiling constraints...");
    let unparsed_file = fs::read_to_string(source).expect("cannot read file");
    let module = Module::parse(&unparsed_file).unwrap();
    let module_3ac = compile(module, &PrimeFieldOps::<Fp>::default(), config);

    qprintln!(config, "* Synthesizing arithmetic circuit...");
    let module_rc = Rc::new(module_3ac);
    let circuit = Halo2Module::<Fp>::new(module_rc);
    let params: Params<EqAffine> = Params::new(circuit.k);
    let mut circuit_file = File::create(output).expect("unable to create circuit file");
    HaloCircuitData { params, circuit }
        .write(&mut circuit_file)
        .unwrap();

    qprintln!(config, "* Constraint compilation success!");

    Ok(())
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

    let HaloCircuitData {
        params,
        mut circuit,
    } = HaloCircuitData::read(&mut circuit_file).unwrap();

    // Prompt for program inputs
    let var_assignments_ints = match inputs {
        Some(path_to_inputs) => {
            qprintln!(
                config,
                "* Reading inputs from file {}...",
                path_to_inputs.to_string_lossy()
            );
            read_inputs_from_file(&circuit.module, path_to_inputs)
        }
        None => {
            if expected_path_to_inputs.exists() {
                qprintln!(
                    config,
                    "* Reading inputs from file {}...",
                    expected_path_to_inputs.to_string_lossy()
                );
                read_inputs_from_file(&circuit.module, &expected_path_to_inputs)
            } else {
                qprintln!(config, "* Soliciting circuit witnesses...");
                prompt_inputs(&circuit.module)
            }
        }
    };

    let mut var_assignments = HashMap::new();
    for (k, v) in var_assignments_ints {
        var_assignments.insert(k, make_constant(v));
    }

    // Populate variable definitions
    circuit.populate_variables(var_assignments.clone());

    // Get public inputs Fp
    let binding = circuit
        .module
        .pubs
        .iter()
        .map(|inst| var_assignments[&inst.id])
        .collect::<Vec<Fp>>();
    let instances = binding.as_slice();

    // Generating proving key
    qprintln!(config, "* Generating proving key...");
    let (pk, _vk) = keygen(&circuit, &params)?;

    // Start proving witnesses
    qprintln!(config, "* Proving knowledge of witnesses...");
    let proof = prover(circuit, &params, &pk, instances)?;

    // Serilize Public Inputs
    // Convert Vec<Fp> to Vec<u8>
    let public_inputs: Vec<u8> = instances
        .iter()
        .flat_map(|fp| {
            let repr = fp.to_repr();
            repr.as_ref().to_vec()
        })
        .collect();

    qprintln!(config, "* Serializing proof to storage...");
    let mut proof_file = File::create(output).expect("unable to create proof file");
    ProofDataHalo2 {
        proof,
        public_inputs,
    }
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
    let ProofDataHalo2 {
        proof,
        public_inputs,
    } = ProofDataHalo2::deserialize(&mut proof_file).unwrap();

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

#[derive(CanonicalSerialize, CanonicalDeserialize)]
struct ProofDataHalo2 {
    proof: Vec<u8>,
    public_inputs: Vec<u8>,
}

/* Captures all the data required to use a Halo2 circuit. */
struct HaloCircuitData {
    params: Params<EqAffine>,
    circuit: Halo2Module<Fp>,
}

impl HaloCircuitData {
    fn read<R>(mut reader: R) -> Result<Self, DecodeError>
    where
        R: std::io::Read,
    {
        let params = Params::<EqAffine>::read(&mut reader)
            .map_err(|x| DecodeError::OtherString(x.to_string()))?;
        let circuit: Halo2Module<Fp> =
            bincode::decode_from_std_read(&mut reader, bincode::config::standard())?;
        Ok(Self { params, circuit })
    }

    fn write<W>(&self, mut writer: W) -> Result<(), EncodeError>
    where
        W: std::io::Write,
    {
        self.params
            .write(&mut writer)
            .expect("unable to create circuit file");
        bincode::encode_into_std_write(&self.circuit, &mut writer, bincode::config::standard())
            .expect("unable to create circuit file");
        Ok(())
    }
}

pub fn halo2(halo2_commands: &Halo2Commands, config: &Config) -> Result<(), Error> {
    match halo2_commands {
        Halo2Commands::Compile(args) => compile_halo2_cmd(args, &config),
        Halo2Commands::Prove(args) => prove_halo2_cmd(args, &config),
        Halo2Commands::Verify(args) => verify_halo2_cmd(args, &config),
    }
}
