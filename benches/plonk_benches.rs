use ark_bls12_381::{Bls12_381, Fr as BlsScalar};
use ark_ed_on_bls12_381::EdwardsParameters as JubJubParameters;
use ark_poly::polynomial::univariate::DensePolynomial;
use ark_poly_commit::{sonic_pc::SonicKZG10, PolynomialCommitment};
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize, Read, SerializationError};
use criterion::{criterion_group, criterion_main, Criterion};
use plonk::error::to_pc_error;
use plonk_core::circuit::{verify_proof, Circuit};
use plonk_core::prelude::VerifierData;
use plonk_core::proof_system::pi::PublicInputs;
use plonk_core::proof_system::{Proof, ProverKey, VerifierKey};

use rand_core::OsRng;
use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::rc::Rc;

use vamp_ir::ast::Module;
use vamp_ir::plonk::synth::PlonkModule;
use vamp_ir::plonk::synth::{make_constant, PrimeFieldOps};
use vamp_ir::transform::compile;
use vamp_ir::util::{prompt_inputs, Config};

use std::time::Instant;

type PC = SonicKZG10<Bls12_381, DensePolynomial<BlsScalar>>;

#[allow(dead_code)]
fn bench(pir_file: &str, max_degree: u32) {
    /* Captures all the data required to use a PLONK circuit. */
    struct PlonkCircuitData {
        pk_p: ProverKey<BlsScalar>,
        vk: (VerifierKey<BlsScalar, PC>, Vec<usize>),
        circuit: PlonkModule<BlsScalar, JubJubParameters>,
    }

    /* Captures all the data generated from proving circuit witnesses. */
    #[derive(CanonicalSerialize, CanonicalDeserialize)]
    struct ProofData {
        proof: Proof<BlsScalar, PC>,
        pi: PublicInputs<BlsScalar>,
    }

    let mut file = File::create("benches/plonk_bench_timing.txt").unwrap();

    // SETUP
    println!("* Setting up public parameters...");
    let inst0 = Instant::now();
    let pp = PC::setup(1 << max_degree, None, &mut OsRng)
        .map_err(to_pc_error::<BlsScalar, PC>)
        .expect("unable to setup polynomial commitment scheme public parameters");
    let inst1 = Instant::now();
    file.write_all(
        format!(
            "Setting up public parameters time: {:?}\n",
            inst1.duration_since(inst0)
        )
        .as_bytes(),
    )
    .unwrap();

    // COMPILATION
    println!("* Compiling constraints...");
    let inst2 = Instant::now();
    let unparsed_file = fs::read_to_string(pir_file).expect("cannot read file");
    let module = Module::parse(&unparsed_file).unwrap();
    let module_3ac = compile(
        module,
        &PrimeFieldOps::<BlsScalar>::default(),
        &Config { quiet: false },
    );
    let inst3 = Instant::now();
    file.write_all(
        format!(
            "Compiling constraints time: {:?}\n",
            inst3.duration_since(inst2)
        )
        .as_bytes(),
    )
    .unwrap();

    println!("* Synthesizing arithmetic circuit...");
    let inst4 = Instant::now();
    let module_rc = Rc::new(module_3ac);
    let mut circuit = PlonkModule::<BlsScalar, JubJubParameters>::new(module_rc);
    // Compile the circuit
    let (pk_p, vk) = circuit
        .compile::<PC>(&pp)
        .expect("unable to compile circuit");
    let inst5 = Instant::now();
    file.write_all(
        format!(
            "Synthesizing arithmetic circuit time: {:?}\n",
            inst5.duration_since(inst4)
        )
        .as_bytes(),
    )
    .unwrap();
    println!("* Constraint compilation success!");

    // PROOF GENERATION

    // Prover POV
    println!("* Soliciting circuit witnesses...");
    let inst6 = Instant::now();
    // Prompt for program inputs
    let var_assignments_ints = prompt_inputs(&circuit.module);
    let mut var_assignments = HashMap::new();
    for (k, v) in var_assignments_ints {
        var_assignments.insert(k, make_constant(&v));
    }
    let inst7 = Instant::now();
    file.write_all(
        format!(
            "Soliciting circuit witnesses time: {:?}\n",
            inst7.duration_since(inst6)
        )
        .as_bytes(),
    )
    .unwrap();

    // Populate variable definitions
    circuit.populate_variables(var_assignments);

    // Start proving witnesses
    println!("* Proving knowledge of witnesses...");
    let inst7 = Instant::now();
    let (proof, pi) = circuit.gen_proof::<PC>(&pp, pk_p, b"Test").unwrap();
    let inst8 = Instant::now();
    file.write_all(
        format!(
            "Proving knowledge of witnesses time: {:?}\n",
            inst8.duration_since(inst7)
        )
        .as_bytes(),
    )
    .unwrap();

    // Verifier POV
    println!("* Verifying proof validity...");
    let inst9 = Instant::now();
    let verifier_data = VerifierData::new(vk.0, pi);
    let verifier_result = verify_proof::<BlsScalar, JubJubParameters, PC>(
        &pp,
        verifier_data.key,
        &proof,
        &verifier_data.pi,
        b"Test",
    );
    let inst10 = Instant::now();
    file.write_all(
        format!(
            "Verifying proof validity time: {:?}\n",
            inst10.duration_since(inst9)
        )
        .as_bytes(),
    )
    .unwrap();
    if let Ok(()) = verifier_result {
        println!("* Zero-knowledge proof is valid");
    } else {
        println!("* Result from verifier: {verifier_result:?}");
    }
}

#[allow(dead_code)]
fn criterion_benchmark(_c: &mut Criterion) {
    let source = "tests/blake2s.pir".to_string();
    let max_degree = 19;
    //_c.bench_function("plonk_bench", |b| b.iter(|| bench(&source, max_degree)));
    bench(&source, max_degree);
}

criterion_group!(benches, criterion_benchmark);
/*
criterion_group!{
    name = benches;
    // This can be any expression that returns a `Criterion` object.
    config = Criterion::default().significance_level(0.1).sample_size(10);
    targets = criterion_benchmark
}
*/
criterion_main!(benches);
