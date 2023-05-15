
pub mod cli;
pub mod synth;

use crate::ast::Module;
use crate::halo2::synth::{keygen, make_constant, prover, verifier, Halo2Module, PrimeFieldOps};
use crate::transform::compile;
use crate::util::{prompt_inputs, read_inputs_from_file};

use halo2_proofs::pasta::{EqAffine, Fp};
use halo2_proofs::plonk::{keygen_vk, Error};
use halo2_proofs::poly::commitment::Params;

use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
use ark_serialize::{Read, SerializationError};
use std::io::Write;

use clap::{Args, Subcommand};

use bincode::error::{DecodeError, EncodeError};
use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::path::PathBuf;
use std::rc::Rc;

#[derive(CanonicalSerialize, CanonicalDeserialize)]
pub struct Halo2ProofData {
    proof: Vec<u8>,
}

/* Captures all the data required to use a Halo2 circuit. */
pub struct Halo2CircuitData {
    params: Params<EqAffine>,
    circuit: Halo2Module<Fp>,
}

impl Halo2CircuitData {
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


/* Implements the subcommand that compiles a vamp-ir file into a Halo2 circuit.
 */
pub fn halo2_compile(source: String) -> Halo2CircuitData {
    let module = Module::parse(&source).unwrap();
    let module_3ac = compile(module, &PrimeFieldOps::<Fp>::default());
    let module_rc = Rc::new(module_3ac);
    let circuit = Halo2Module::<Fp>::new(module_rc);
    let params: Params<EqAffine> = Params::new(circuit.k);
    Halo2CircuitData { params, circuit }
}

/* Implements the subcommand that creates a proof from interactively entered
 * inputs. */
 pub fn halo2_prove(circuit_data: Halo2CircuitData) -> Halo2ProofData {
    // Generating proving key
    let (pk, _vk) = keygen(&circuit_data.circuit, &circuit_data.params);

    // Start proving witnesses
    let proof = prover(circuit_data.circuit, &circuit_data.params, &pk);
    Halo2ProofData { proof }
}

/* Implements the subcommand that verifies that a proof is correct. */
fn halo2_verify(circuit_data: Halo2CircuitData, proof_data: Halo2ProofData) -> Result<(), Error> {
    let vk = keygen_vk(&circuit_data.params, &circuit_data.circuit).expect("keygen_vk should not fail");
    
    // Veryfing proof
    verifier(&circuit_data.params, &vk, &proof_data.proof)
}