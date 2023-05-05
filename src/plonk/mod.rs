use crate::ast::Module;
use crate::transform::compile;
use crate::plonk::synth::PrimeFieldOps;
use ark_bls12_381::{Bls12_381, Fr as BlsScalar};

pub mod cli;
pub mod synth;

pub fn plonk_compile(source: String, ) {
    println!("* Compiling constraints...");
    //let unparsed_file = fs::read_to_string(source).expect("cannot read file");
    let module = Module::parse(&source).unwrap();
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
    //let mut circuit = PlonkModule::<BlsScalar, JubJubParameters>::new(&module_3ac);
    let module_rc = Rc::new(module_3ac);
    let mut circuit = PlonkModule::<BlsScalar, JubJubParameters>::new(module_rc);

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
};