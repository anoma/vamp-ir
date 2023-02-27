mod ast;
mod transform;
mod plonk_synth;
mod halo_synth;
mod typecheck;
extern crate pest;
#[macro_use]
extern crate pest_derive;
use std::fs;
use crate::ast::{Module, VariableId, Pat, parse_prefixed_num};
use crate::transform::{compile, collect_module_variables};
use ark_bls12_381::{Bls12_381, Fr as BlsScalar};
use ark_ed_on_bls12_381::EdwardsParameters as JubJubParameters;
use plonk_core::circuit::verify_proof;
use ark_poly_commit::{sonic_pc::SonicKZG10, PolynomialCommitment};
use ark_poly::polynomial::univariate::DensePolynomial;
use rand_core::OsRng;
use plonk::error::to_pc_error;
use std::collections::{HashMap, HashSet};
use std::io::Write;
use plonk_core::prelude::VerifierData;
use crate::plonk_synth::PlonkModule;
use crate::halo_synth::{Halo2Module, keygen, prover, verifier};
use plonk_core::circuit::Circuit;
use std::fs::File;
use ark_serialize::{CanonicalSerialize, CanonicalDeserialize};
use ark_ec::PairingEngine;
use plonk_core::proof_system::{ProverKey, VerifierKey, Proof};
use plonk_core::proof_system::pi::PublicInputs;
use bincode::error::{DecodeError, EncodeError};
use ark_serialize::{Read, SerializationError};
use clap::{Args, Parser, Subcommand, ValueEnum};
use std::path::PathBuf;
use halo2_proofs::poly::commitment::Params;
use halo2_proofs::pasta::{EqAffine, Fp};
use std::ops::Neg;
use std::time::Instant;
use halo2_proofs::plonk::keygen_vk;
use num_traits::Num;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Sets up the public parameters required for proving
    Setup(Setup),
    /// Compiles a given source file to a circuit
    Compile(Compile),
    /// Proves knowledge of witnesses satisfying a circuit
    Prove(Prove),
    /// Verifies that a proof is a correct one
    Verify(Verify),
}

#[derive(Args)]
struct Setup {
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

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum ProofSystems {
    /// PLONK general-purpose zero-knowledge proof scheme
    Plonk,
    /// Halo 2 zero-knowledge proving system
    Halo2,
}


/*impl FromStr for ProofSystems {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "halo2" => Self::Halo2,
            "plonk" => Self::Plonk,
            _ => panic!("unrecognized circuit format"),
        })
    }
}*/

#[derive(Args)]
struct Compile {
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
struct Prove {
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
}

#[derive(Args)]
struct Verify {
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

type PC = SonicKZG10<Bls12_381, DensePolynomial<BlsScalar>>;
type UniversalParams = <PC as PolynomialCommitment<<Bls12_381 as PairingEngine>::Fr, DensePolynomial<BlsScalar>>>::UniversalParams;

/* Captures all the data required to use a Halo2 circuit. */
struct HaloCircuitData {
    params: Params<EqAffine>,
    circuit: Halo2Module::<Fp>,
}

impl HaloCircuitData {
    fn read<R>(mut reader: R) -> Result<Self, DecodeError>
    where R: std::io::Read {
        let params = Params::<EqAffine>::read(&mut reader)
            .map_err(|x| DecodeError::OtherString(x.to_string()))?;
        let circuit: Halo2Module::<Fp> =
            bincode::decode_from_std_read(&mut reader, bincode::config::standard())?;
        Ok(Self { params, circuit })
    }
    
    fn write<W>(&self, mut writer: W) -> Result<(), EncodeError>
    where W: std::io::Write {
        self.params.write(&mut writer).expect("unable to create circuit file");
        bincode::encode_into_std_write(
            &self.circuit,
            &mut writer,
            bincode::config::standard(),
        ).expect("unable to create circuit file");
        Ok(())
    }
}

/* Captures all the data required to use a PLONK circuit. */
struct PlonkCircuitData {
    pk_p: ProverKey::<BlsScalar>,
    vk: (VerifierKey::<BlsScalar, PC>, Vec<usize>),
    circuit: PlonkModule::<BlsScalar, JubJubParameters>,
}

impl PlonkCircuitData {
    fn read<R>(mut reader: R) -> Result<Self, DecodeError>
    where R: std::io::Read {
        let pk_p = ProverKey::<BlsScalar>::deserialize(&mut reader)
            .map_err(|x| DecodeError::OtherString(x.to_string()))?;
        let vk = <(VerifierKey::<_, _>, Vec::<usize>)>::deserialize(&mut reader)
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

#[derive(CanonicalSerialize, CanonicalDeserialize)]
struct ProofDataHalo2 {
    proof: Vec<u8>,
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

/* Implements the subcommand that generates the public parameters for proofs. */
fn setup_cmd(Setup { max_degree, output, unchecked }: &Setup) {
    // Generate CRS
    println!("* Setting up public parameters...");
    let pp = PC::setup(1 << max_degree, None, &mut OsRng)
        .map_err(to_pc_error::<BlsScalar, PC>)
        .expect("unable to setup polynomial commitment scheme public parameters");
    let mut pp_file = File::create(output)
        .expect("unable to create public parameters file");
    if *unchecked {
        pp.serialize_unchecked(&mut pp_file)
    } else {
        pp.serialize(&mut pp_file)
    }.unwrap();
    println!("* Public parameter setup success!");
}

/* Extract the circuit format from the path. */
fn circuit_format(path_buf: &PathBuf) -> ProofSystems {
    const ERROR: &str =
        "output circuit must have one of following extensions: halo2, plonk";
    let ext = path_buf.extension().expect(ERROR);
    ProofSystems::from_str(ext.to_str().unwrap(), true).expect(ERROR)
}

/* Implements the subcommand that compiles a vamp-ir file into a PLONK circuit.
 */
fn compile_cmd(Compile { universal_params, source, output, unchecked }: &Compile) {
    match circuit_format(output) {
        ProofSystems::Plonk => {

            //create a file that has the timings of the compilation step
            let mut file = File::create("compilation_time.txt").unwrap();
            println!("* Compiling constraints...");
            let inst0 = Instant::now();
            let unparsed_file = fs::read_to_string(source).expect("cannot read file");
            let module = Module::parse(&unparsed_file).unwrap();
            let module_3ac = compile(module, &plonk_synth::PrimeFieldOps::<BlsScalar>::default());
            let inst1 = Instant::now();
            file.write_all(format!("Compiling constraints time: {:?}\n", inst1.duration_since(inst0)).as_bytes()).unwrap();
            println!("* Reading public parameters...");
            let inst2 = Instant::now();
            let mut pp_file = File::open(universal_params)
                .expect("unable to load public parameters file");
            let pp = if *unchecked {
                UniversalParams::deserialize_unchecked(&mut pp_file)
            } else {
                UniversalParams::deserialize(&mut pp_file)
            }.unwrap();
            let inst3 = Instant::now();
            file.write_all(format!("Reading public parameters time: {:?}\n", inst3.duration_since(inst2)).as_bytes()).unwrap();

            println!("* Synthesizing arithmetic circuit...");
            let inst4 = Instant::now();
            let mut circuit = PlonkModule::<BlsScalar, JubJubParameters>::new(module_3ac.clone());
            // Compile the circuit
            let inst4b = Instant::now();
            let (pk_p, vk) = circuit.compile::<PC>(&pp)
                .expect("unable to compile circuit");
            let inst5 = Instant::now();
            file.write_all(format!("Synthesizing arithmetic circuit time: {:?}\n", inst5.duration_since(inst4)).as_bytes()).unwrap();
            println!("* Serializing circuit to storage...");
            let inst6 = Instant::now();
            let mut circuit_file = File::create(output)
                .expect("unable to create circuit file");
            PlonkCircuitData { pk_p, vk, circuit }.write(&mut circuit_file).unwrap();
            let inst7 = Instant::now();
            file.write_all(format!("Serializing circuit to storage time: {:?}\n\n", inst7.duration_since(inst6)).as_bytes()).unwrap();
            println!("* Constraint compilation success!");
            //print total compilation time
            file.write_all(format!("Total compilation time: {:?}\n", inst7.duration_since(inst0)).as_bytes()).unwrap();
            //print compile the circuit time
            file.write_all(format!("Pure compilation time: {:?}\n", inst5.duration_since(inst4b)).as_bytes()).unwrap();
        },
        ProofSystems::Halo2 => {
            let mut file = File::create("compilation_time.txt").unwrap();
            println!("* Compiling constraints...");
            let inst0 = Instant::now();
            let unparsed_file = fs::read_to_string(source).expect("cannot read file");
            let module = Module::parse(&unparsed_file).unwrap();
            let module_3ac = compile(module, &halo_synth::PrimeFieldOps::<Fp>::default());
            let inst1 = Instant::now();
            file.write_all(format!("Compiling constraints time: {:?}\n", inst1.duration_since(inst0)).as_bytes()).unwrap();

            println!("* Synthesizing arithmetic circuit...");
            let inst2 = Instant::now();
            let circuit = Halo2Module::<Fp>::new(module_3ac.clone());
            let params: Params<EqAffine> = Params::new(circuit.k);
            let mut circuit_file = File::create(output)
                .expect("unable to create circuit file");
            HaloCircuitData { params, circuit }.write(&mut circuit_file).unwrap();
            let inst3 = Instant::now();
            file.write_all(format!("Synthesizing arithmetic circuit time: {:?}\n", inst3.duration_since(inst2)).as_bytes()).unwrap();

            println!("* Constraint compilation success!");
        },
    }
}

/* Implements the subcommand that creates a proof from interactively entered
 * inputs. */
fn prove_cmd(Prove { universal_params, circuit, output, unchecked }: &Prove) {
    match circuit_format(circuit) {
        ProofSystems::Plonk => {
            //create a file that has the timings of the proof generation step
            let mut file = File::create("proving_time.txt").unwrap();
            println!("* Reading arithmetic circuit...");
            let inst0 = Instant::now();
            let mut circuit_file = File::open(circuit)
                .expect("unable to load circuit file");
            let PlonkCircuitData { pk_p, vk: _vk, mut circuit} =
                PlonkCircuitData::read(&mut circuit_file).unwrap();
            let inst1 = Instant::now();
            file.write_all(format!("Reading arithmetic circuit time: {:?}\n", inst1.duration_since(inst0)).as_bytes()).unwrap();

            // Prover POV
            println!("* Soliciting circuit witnesses...");
            let inst2 = Instant::now();
            // Prompt for program inputs
            let var_assignments_ints = prompt_inputs(&circuit.module);
            let mut var_assignments = HashMap::new();
            for (k, v) in var_assignments_ints {
                var_assignments.insert(k, plonk_synth::make_constant(&v));
            }
            let inst3 = Instant::now();
            file.write_all(format!("Soliciting circuit witnesses time: {:?}\n", inst3.duration_since(inst2)).as_bytes()).unwrap();
            
            // Populate variable definitions
            circuit.populate_variables(var_assignments);
            
            println!("* Reading public parameters...");
            let inst4 = Instant::now();
            let mut pp_file = File::open(universal_params)
                .expect("unable to load public parameters file");
            let pp = if *unchecked {
                UniversalParams::deserialize_unchecked(&mut pp_file)
            } else {
                UniversalParams::deserialize(&mut pp_file)
            }.unwrap();
            let inst5 = Instant::now();
            file.write_all(format!("Reading public parameters time: {:?}\n", inst5.duration_since(inst4)).as_bytes()).unwrap();

            // Start proving witnesses
            println!("* Proving knowledge of witnesses...");
            let inst6 = Instant::now();
            let (proof, pi) = circuit.gen_proof::<PC>(&pp, pk_p, b"Test").unwrap();
            let inst7 = Instant::now();
            file.write_all(format!("Proving knowledge of witnesses time: {:?}\n", inst7.duration_since(inst6)).as_bytes()).unwrap();

            println!("* Serializing proof to storage...");
            let inst8 = Instant::now();
            let mut proof_file = File::create(output)
                .expect("unable to create proof file");
            ProofData { proof, pi }.serialize(&mut proof_file).unwrap();
            let inst9 = Instant::now();
            file.write_all(format!("Serializing proof to storage time: {:?}\n\n", inst9.duration_since(inst8)).as_bytes()).unwrap();
            println!("* Proof generation success!");
            //print total proof generation time
            file.write_all(format!("Total proof generation time: {:?}\n", inst9.duration_since(inst0)).as_bytes()).unwrap();
        },
        ProofSystems::Halo2 => {
            let mut file = File::create("proving_time.txt").unwrap();
            println!("* Reading arithmetic circuit...");
            let inst0 = Instant::now();
            let mut circuit_file = File::open(circuit)
                .expect("unable to load circuit file");
            let HaloCircuitData { params, mut circuit} =
                HaloCircuitData::read(&mut circuit_file).unwrap();
            let inst1 = Instant::now();
            file.write_all(format!("Reading arithmetic circuit time: {:?}\n", inst1.duration_since(inst0)).as_bytes()).unwrap();
            // Prover POV
            println!("* Soliciting circuit witnesses...");
            // Prompt for program inputs
            let inst2 = Instant::now();
            let var_assignments_ints = prompt_inputs(&circuit.module);
            let mut var_assignments = HashMap::new();
            for (k, v) in var_assignments_ints {
                var_assignments.insert(k, halo_synth::make_constant(v));
            }
            // Populate variable definitions
            circuit.populate_variables(var_assignments);
            let inst3 = Instant::now();
            file.write_all(format!("Soliciting circuit witnesses time: {:?}\n", inst3.duration_since(inst2)).as_bytes()).unwrap();
            let inst3 = Instant::now();
            file.write_all(format!("Soliciting circuit witnesses time: {:?}\n", inst3.duration_since(inst2)).as_bytes()).unwrap();

            // Generating proving key
            println!("* Generating proving key...");
            let inst4 = Instant::now();
            let (pk, _vk) = keygen(&circuit, &params);
            let inst5 = Instant::now();
            file.write_all(format!("Generating proving key time: {:?}\n", inst5.duration_since(inst4)).as_bytes()).unwrap();

            // Start proving witnesses
            println!("* Proving knowledge of witnesses...");
            let inst6 = Instant::now();
            let proof = prover(circuit, &params, &pk);
            let inst7 = Instant::now();
            file.write_all(format!("Proving knowledge of witnesses time: {:?}\n", inst7.duration_since(inst6)).as_bytes()).unwrap();

            println!("* Serializing proof to storage...");
            let inst8 = Instant::now();
            let mut proof_file = File::create(output)
                .expect("unable to create proof file");
            ProofDataHalo2 { proof }.serialize(&mut proof_file).expect("Proof serialization failed");
            let inst9 = Instant::now();
            file.write_all(format!("Serializing proof to storage time: {:?}\n\n", inst9.duration_since(inst8)).as_bytes()).unwrap();
            println!("* Proof generation success!");
            //print total proof generation time
            file.write_all(format!("Total proof generation time: {:?}\n", inst9.duration_since(inst0)).as_bytes()).unwrap();
        },
    }
}

/* Implements the subcommand that verifies that a proof is correct. */
fn verify_cmd(Verify { universal_params, circuit, proof, unchecked }: &Verify) {
    match circuit_format(circuit) {
        ProofSystems::Plonk => {
            let mut file = File::create("verifying_time.txt").unwrap();
            println!("* Reading arithmetic circuit...");
            let inst0 = Instant::now();
            let mut circuit_file = File::open(circuit)
                .expect("unable to load circuit file");
            let PlonkCircuitData { pk_p: _pk_p, vk, circuit } =
                PlonkCircuitData::read(&mut circuit_file).unwrap();
            let inst1 = Instant::now();
            file.write_all(format!("Reading arithmetic circuit time: {:?}\n", inst1.duration_since(inst0)).as_bytes()).unwrap();

            println!("* Reading zero-knowledge proof...");
            let inst2 = Instant::now();
            let mut proof_file = File::open(proof)
                .expect("unable to load proof file");
            let ProofData { proof, pi } = ProofData::deserialize(&mut proof_file).unwrap();
            let inst3 = Instant::now();
            file.write_all(format!("Reading zero-knowledge proof time: {:?}\n", inst3.duration_since(inst2)).as_bytes()).unwrap();

            println!("* Public inputs:");
            let inst4 = Instant::now();
            for (var, val) in circuit.annotate_public_inputs(&vk.1, &pi).values() {
                println!("{} = {}", var, val);
            }
            let inst5 = Instant::now();
            file.write_all(format!("Public inputs time: {:?}\n", inst5.duration_since(inst4)).as_bytes()).unwrap();

            println!("* Reading public parameters...");
            let inst6 = Instant::now();
            let mut pp_file = File::open(universal_params)
                .expect("unable to load public parameters file");
            let pp = if *unchecked {
                UniversalParams::deserialize_unchecked(&mut pp_file)
            } else {
                UniversalParams::deserialize(&mut pp_file)
            }.unwrap();
            let inst7 = Instant::now();
            file.write_all(format!("Reading public parameters time: {:?}\n", inst7.duration_since(inst6)).as_bytes()).unwrap();

            // Verifier POV
            println!("* Verifying proof validity...");
            let inst8 = Instant::now();
            let verifier_data = VerifierData::new(vk.0, pi);
            let verifier_result = verify_proof::<BlsScalar, JubJubParameters, PC>(
                &pp,
                verifier_data.key,
                &proof,
                &verifier_data.pi,
                b"Test",
            );
            let inst9 = Instant::now();
            file.write_all(format!("Verifying proof validity time: {:?}\n", inst9.duration_since(inst8)).as_bytes()).unwrap();
            if let Ok(()) = verifier_result {
                println!("* Zero-knowledge proof is valid");
            } else {
                println!("* Result from verifier: {:?}", verifier_result);
            }
            let inst10 = Instant::now();
            file.write_all(format!("Total verifying time: {:?}\n", inst10.duration_since(inst0)).as_bytes()).unwrap();
        },
        ProofSystems::Halo2 => {
            let mut file = File::create("verifying_time.txt").unwrap();
            println!("* Reading arithmetic circuit...");
            let inst0 = Instant::now();
            let circuit_file = File::open(circuit)
                .expect("unable to load circuit file");
            let HaloCircuitData { params, circuit} =
                HaloCircuitData::read(&circuit_file).unwrap();
            let inst1 = Instant::now();
            file.write_all(format!("Reading arithmetic circuit time: {:?}\n", inst1.duration_since(inst0)).as_bytes()).unwrap();

            println!("* Generating verifying key...");
            let inst2 = Instant::now();
            let vk = keygen_vk(&params, &circuit).expect("keygen_vk should not fail");
            let inst3 = Instant::now();
            file.write_all(format!("Generating verifying key time: {:?}\n", inst3.duration_since(inst2)).as_bytes()).unwrap();

            println!("* Reading zero-knowledge proof...");
            let inst4 = Instant::now();
            let mut proof_file = File::open(proof)
                .expect("unable to load proof file");
            let ProofDataHalo2 { proof } = ProofDataHalo2::deserialize(&mut proof_file).unwrap();
            let inst5 = Instant::now();
            file.write_all(format!("Reading zero-knowledge proof time: {:?}\n", inst5.duration_since(inst4)).as_bytes()).unwrap();

            // Veryfing proof
            println!("* Verifying proof validity...");
            let inst6 = Instant::now();
            let verifier_result = verifier(&params, &vk, &proof);
            let inst7 = Instant::now();
            file.write_all(format!("Verifying proof validity time: {:?}\n", inst7.duration_since(inst6)).as_bytes()).unwrap();

            if let Ok(()) = verifier_result {
                println!("* Zero-knowledge proof is valid");
            } else {
                println!("* Result from verifier: {:?}", verifier_result);
            }
            // print total verifying time
            let inst8 = Instant::now();MyCir
            file.write_all(format!("Total verifying time: {:?}\n", inst8.duration_since(inst0)).as_bytes()).unwrap();
        }
    }
}

/* Main entry point for vamp-ir compiler, prover, and verifier. */
fn main() {
    let cli = Cli::parse();
    match &cli.command {
        Commands::Setup(args) => {
            setup_cmd(args);
        },
        Commands::Compile(args) => {
            compile_cmd(args);
        },
        Commands::Prove(args) => {
            prove_cmd(args);
        },
        Commands::Verify(args) => {
            verify_cmd(args);
        },
    }
}
