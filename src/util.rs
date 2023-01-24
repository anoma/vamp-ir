use plonk_core::proof_system::VerifierKey as ZkGarageVerifierKey;
use ark_poly_commit::{sonic_pc::SonicKZG10, PolynomialCommitment};
use ark_poly::polynomial::univariate::DensePolynomial;
use ark_ff::PrimeField;
use ark_bn254::{Bn254, Fr, G2Affine};
use ark_ec::PairingEngine;
use ark_ed_on_bn254::EdwardsParameters as InnerParameters;
use ark_serialize::{CanonicalSerialize, CanonicalDeserialize};
use ark_serialize::{Read, SerializationError};
use num_bigint::BigUint;
use num_traits::Num;

type PC = SonicKZG10<Bn254, DensePolynomial<Fr>>;
type UniversalParams = <PC as PolynomialCommitment<<Bn254 as PairingEngine>::Fr, DensePolynomial<Fr>>>::UniversalParams;

#[derive(CanonicalDeserialize)]
pub struct AztecVerifierKey {
    // N
    n: [u8; 32],

    // NUM_INPUTS
    num_inputs: BigUint,

    // OMEGA
    omega: Fr,

    // DOMAIN_INVERSE
    omega_inv: Fr,

    // Q1
    q_l: PC,

    // Q2
    q_r: PC,

    // Q3
    q_o: PC,

    // QM
    q_m: PC,

    // QC
    q_c: PC,

    // SIGMA1
    left_sigma: PC,

    // SIGMA2
    right_sigma: PC,

    // SIGMA3
    out_sigma: PC,

    // CONTAINS_RECURSIVE_PROOF
    contains_recursive_proof: BigUint,

    // RECURSIVE_PROOF_PUBLIC_INPUT_INDICES
    recursive_proof_public_input_indices: BigUint,
}

pub struct AztecProofData {
    // W1
    w1: PC,

    // W2
    w2: PC,

    // W3
    w3: PC,

    // Z
    z: PC,

    // T1
    t1: PC,

    // T2
    t2: PC,

    // T3
    t3: PC,

    // W1_EVAL
    w1_eval: Fr,

    // W2_EVAL
    w2_eval: Fr,

    // W3_EVAL
    w3_eval: Fr,

    // SIGMA1_EVAL
    sigma1_eval: Fr,

    // SIGMA2_EVAL
    sigma2_eval: Fr,

    // Z_OMEGA_EVAL
    z_omega_eval: Fr,

    // PI_Z
    pi_z: PC,

    // PI_Z_OMEGA
    pi_z_omega: PC,
}