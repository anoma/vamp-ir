use crate::ast::Module;
use crate::plonk::synth::{PlonkModule, PrimeFieldOps};
use crate::transform::compile;

use ark_bls12_381::{Bls12_381, Fr as BlsScalar};
use ark_ec::PairingEngine;
use ark_ed_on_bls12_381::EdwardsParameters as JubJubParameters;
use ark_poly::polynomial::univariate::DensePolynomial;
use ark_poly_commit::{sonic_pc::SonicKZG10, PolynomialCommitment};
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize, Read, SerializationError};

use plonk_core::circuit::{verify_proof, Circuit};
use plonk_core::error::Error;
use plonk_core::prelude::VerifierData;
use plonk_core::proof_system::pi::PublicInputs;
use plonk_core::proof_system::{Proof, ProverKey, VerifierKey};

use bincode::error::{DecodeError, EncodeError};

use std::io::Write;
use std::rc::Rc;

pub mod cli;
pub mod synth;

type PC = SonicKZG10<Bls12_381, DensePolynomial<BlsScalar>>;
type UniversalParams = <PC as PolynomialCommitment<
    <Bls12_381 as PairingEngine>::Fr,
    DensePolynomial<BlsScalar>,
>>::UniversalParams;

/* Captures all the data required to use a PLONK circuit. */
pub struct PlonkCircuitData {
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
pub struct PlonkProofData {
    proof: Proof<BlsScalar, PC>,
    pi: PublicInputs<BlsScalar>,
}

pub fn plonk_compile(source: String, universal_params: &UniversalParams) -> PlonkCircuitData {
    let module = Module::parse(&source).unwrap();
    let module_3ac = compile(module, &PrimeFieldOps::<BlsScalar>::default());
    let module_rc = Rc::new(module_3ac);
    let mut circuit = PlonkModule::<BlsScalar, JubJubParameters>::new(module_rc);

    // Compile the circuit
    let (pk_p, vk) = circuit
        .compile::<PC>(&universal_params)
        .expect("unable to compile circuit");

    PlonkCircuitData { pk_p, vk, circuit }
}

pub fn plonk_prove(
    universal_params: &UniversalParams,
    circuit_data: &mut PlonkCircuitData,
) -> PlonkProofData {
    let (proof, pi) = circuit_data
        .circuit
        .gen_proof::<PC>(&universal_params, circuit_data.pk_p.clone(), b"Test")
        .unwrap();
    PlonkProofData { proof, pi }
}

pub fn plonk_verify(
    universal_params: &UniversalParams,
    circuit_data: PlonkCircuitData,
    proof_data: PlonkProofData,
) -> Result<(), Error> {
    let verifier_data = VerifierData::new(circuit_data.vk.0, proof_data.pi);
    verify_proof::<BlsScalar, JubJubParameters, PC>(
        universal_params,
        verifier_data.key,
        &proof_data.proof,
        &verifier_data.pi,
        b"Test",
    )
}
