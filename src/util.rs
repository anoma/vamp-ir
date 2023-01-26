use ark_ff::FftField;
use plonk_core::proof_system::VerifierKey as ZkGarageVerifierKey;
use plonk_core::proof_system::Proof as ZkGarageProof;
use plonk_core::proof_system::pi::PublicInputs as ZkGaragePublicInputs;
use plonk_core::commitment::HomomorphicCommitment;
use plonk_core::error::Error;
use ark_poly_commit::{sonic_pc::SonicKZG10, PolynomialCommitment};
use ark_poly::polynomial::univariate::DensePolynomial;
use ark_poly::{EvaluationDomain, GeneralEvaluationDomain};
use ark_ff::PrimeField;
use ark_bn254::{Bn254, Fr, G2Affine};
use ark_ec::PairingEngine;
use ark_ed_on_bn254::EdwardsParameters as InnerParameters;
use ark_serialize::{CanonicalSerialize, CanonicalDeserialize};
use ark_serialize::{Read, SerializationError};
use num_bigint::BigUint;
use num_traits::Num;

//type PC = SonicKZG10<Bn254, DensePolynomial<Fr>>;
//type UniversalParams = <PC as PolynomialCommitment<<Bn254 as PairingEngine>::Fr, DensePolynomial<Fr>>>::UniversalParams;


struct MatterLabsVerificationKey<F, PC>
where
    F: PrimeField,
    PC: HomomorphicCommitment<F>,
{
    domain_size: [u8; 32],
    num_inputs: [u8; 32],
    omega: F,
    selector_commitments: [PC::Commitment; 6], //qm, ql, qr, qo, q4, qc
    next_step_selector_commitments: PC::Commitment,
    permutation_commitments: [PC::Commitment; 4], // l, r, o, 4
    permutation_non_residues: [F; 3], // ????
    g2_x: G2Affine,
}

struct MatterLabsProof<F, PC>
where
    F: PrimeField,
    PC: HomomorphicCommitment<F>,
{
    input_values: [u8; 32],
    wire_commitments: [PC::Commitment; 4],
    grand_product_commitment: PC::Commitment,
    quotient_poly_commitments: [PC::Commitment; 4],
    wire_values_at_z: [F; 4],
    wire_values_at_z_omega: F,
    grand_product_at_z_omega: F,
    quotient_polynomial_at_z: F,
    linearization_polynomial_at_z: F,
    permutation_polynomials_at_z: [F; 3],
    opening_at_z_proof: PC::Commitment,
    opening_at_z_omega_proof: PC::Commitment,
}

struct MatterLabsPartialVerifierState<F>
where
    F: PrimeField,
{
    alpha: F,
    beta: F,
    gamma: F,
    v: F,
    u: F,
    z: F,
    cached_lagrange_evals: Vec<F>,
}

pub fn adapter<F, PC>(
    zkg_vk: ZkGarageVerifierKey<F, PC>,
    zkg_pf: ZkGarageProof<F, PC>,
    pi: ZkGaragePublicInputs<F>,
) -> (
    MatterLabsVerificationKey<F, PC>,
    MatterLabsProof<F, PC>,
    MatterLabsPartialVerifierState<F>,
)
where
    F: PrimeField,
    PC: HomomorphicCommitment<F>,
{
    let padded_size = zkg_vk.padded_circuit_size();

    let padded_size_bytes = padded_size.to_be_bytes();
    let mut domain_size_bytes = vec![0u8; 32];
    domain_size_bytes.splice((32-padded_size_bytes.len()).., padded_size_bytes);
    let domain_size = domain_size_bytes.try_into().unwrap();

    let num_inputs = pi.get_vals().collect::<Vec<_>>().len();

    /*
        very annoying 

        it would be better to serialize and deserialize perhaps?

        no just write a function for it

        
    
    */


    let domain =
        GeneralEvaluationDomain::<F>::new(padded_size).ok_or(Error::InvalidEvalDomainSize {
            log_size_of_group: padded_size.trailing_zeros(),
            adicity: <<F as FftField>::FftParams as ark_ff::FftParameters>::TWO_ADICITY,
        }).unwrap();
    


    let ml_vk = MatterLabsVerificationKey{
        domain_size,
        num_inputs,
        omega: todo!(),
        selector_commitments: todo!(),
        next_step_selector_commitments: todo!(),
        permutation_commitments: todo!(),
        permutation_non_residues: todo!(),
        g2_x: todo!(),
        
    };
}

// struct AztecVerificationKey<F, PC>
// where
//     F: PrimeField,
//     PC: HomomorphicCommitment<F>,
// {
//     circuit_size: [u8; 32],
//     num_inputs: [u8; 32],
//     work_root: F,
//     domain_inverse: F,
//     work_root_inverse: F,
//     Q1: PC::Commitment,
//     Q2: PC::Commitment,
//     Q3: PC::Commitment,
//     QM: PC::Commitment,
//     QC: PC::Commitment,
//     SIGMA1: PC::Commitment,
//     SIGMA2: PC::Commitment,
//     SIGMA3: PC::Commitment,
//     contains_recursive_proof: bool,
//     recursive_proof_indices: [u8; 32],
//     g2_x: G2Affine,
//     // zeta challenge raised to the power of the circuit size.
//     // Not actually part of the verification key, but we put it here to prevent stack depth errors
//     zeta_pow_n: F,
// }

// struct AztecProof<F, PC>
// where
//     F: PrimeField,
//     PC: HomomorphicCommitment<F>,
// {
//     W1: PC::Commitment,
//     W2: PC::Commitment,
//     W3: PC::Commitment,
//     Z: PC::Commitment,
//     T1: PC::Commitment,
//     T2: PC::Commitment,
//     T3: PC::Commitment,
//     w1: F,
//     w2: F,
//     w3: F,
//     sigma1: F,
//     sigma2: F,
//     //    uint256 linearization_polynomial;
//     grand_product_at_z_omega: F,
//     PI_Z: PC::Commitment,
//     PI_Z_OMEGA: PC::Commitment,
//     recursive_P1: PC::Commitment,
//     recursive_P2: PC::Commitment,
//     r_0: F, // Changes owing to the simplified Plonk
// }

// struct AztecChallengeTranscript<F>
// where
//     F: PrimeField
// {
//     alpha_base: F,
//     alpha: F,
//     zeta: F,
//     beta: F,
//     gamma: F,
//     u: F,
//     v0: F,
//     v1: F,
//     v2: F,
//     v3: F,
//     v4: F,
//     v5: F,
//     v6: F,
//     // uint256 v7;
// }

// pub fn generate_verifier
//     //<F, PC>
//     (
//     //    vk: AztecVerificationKey<F, PC>,
//         name: String
//     ) -> String {
//     const starting_loc: usize = 512;
//     const size: usize = 32;
//     let mut curr = starting_loc;
//     let preamble: String = format!("pragma solidity >=0.8.4;");
//     let vk_mem_locs = vec![
//         "N_LOC",
//         "NUM_INPUTS_LOC",
//         "OMEGA_LOC",
//         "DOMAIN_INVERSE_LOC",
//         "Q1_X_LOC",
//         "Q1_Y_LOC",
//         "Q2_X_LOC",
//         "Q2_Y_LOC",
//         "Q3_X_LOC",
//         "Q3_Y_LOC",
//         "QM_X_LOC",
//         "QM_Y_LOC",
//         "QC_X_LOC",
//         "QC_Y_LOC",
//         "SIGMA1_X_LOC",
//         "SIGMA1_Y_LOC",
//         "SIGMA2_X_LOC",
//         "SIGMA2_Y_LOC",
//         "SIGMA3_X_LOC",
//         "SIGMA3_Y_LOC",
//         "CONTAINS_RECURSIVE_PROOF_LOC",
//         "RECURSIVE_PROOF_PUBLIC_INPUT_INDICES_LOC",
//         "G2X_X0_LOC",
//         "G2X_X1_LOC",
//         "G2X_Y0_LOC",
//         "G2X_Y1_LOC",
//     ];
//     let proof_mem_locs = vec![
//         "W1_X_LOC",
//         "W1_Y_LOC",
//         "W2_X_LOC",
//         "W2_Y_LOC",
//         "W3_X_LOC",
//         "W3_Y_LOC",
//         "Z_X_LOC",
//         "Z_Y_LOC",
//         "T1_X_LOC",
//         "T1_Y_LOC",
//         "T2_X_LOC",
//         "T2_Y_LOC",
//         "T3_X_LOC",
//         "T3_Y_LOC",
//         "W1_EVAL_LOC",
//         "W2_EVAL_LOC",
//         "W3_EVAL_LOC",
//         "SIGMA1_EVAL_LOC",
//         "SIGMA2_EVAL_LOC",
//         "Z_OMEGA_EVAL_LOC",
//         "PI_Z_X_LOC",
//         "PI_Z_Y_LOC",
//         "PI_Z_OMEGA_X_LOC",
//         "PI_Z_OMEGA_Y_LOC",
//     ];
//     let challenge_mem_locs = vec![
//         "C_BETA_LOC",
//         "C_GAMMA_LOC",
//         "C_ALPHA_LOC",
//         "C_ARITHMETIC_ALPHA_LOC",
//         "C_ZETA_LOC",
//         "C_CURRENT_LOC",
//         "C_V0_LOC",
//         "C_V1_LOC",
//         "C_V2_LOC",
//         "C_V3_LOC",
//         "C_V4_LOC",
//         "C_V5_LOC",
//         "C_U_LOC",
//     ];

//     fn format_mem_alloc(label: &str, location_names: Vec<&str>, curr: &mut usize) -> String {
//         format!("\t// {}\n{}",
//             label,// library VK {
//                 //     function get_verification_key() external pure returns (StandardTypes.VerificationKey memory) {
//                 //         StandardTypes.VerificationKey memory vk;
//                 //         return vk;
//                 //     }
//                 // }
                
//                 .map(|s| {
//                     let line = format!("\t{:<80}{:#x};\n",
//                         format!("uint256 internal constant {} = ", s),
//                         curr
//                     );
//                     *curr = *curr + size;
//                     line
//                 })
//                 .reduce(|acc, s| acc + &s)
//                 .unwrap()
//             )
//     }

//     let memory_alloc =
//         format_mem_alloc("VERIFIER KEY LOCATIONS", vk_mem_locs, &mut curr) + "\n"
//      + &format_mem_alloc("PROOF DATA LOCATIONS", proof_mem_locs, &mut curr) + "\n"
//      + &format_mem_alloc("CHALLENGE DATA LOCATIONS", challenge_mem_locs, &mut curr);

    
//     let verify_function = 
//         format!("\tfunction verify(bytes calldata, uint256 public_inputs_hash) external view override returns (bool) {{}}",

//     );
    
//     let contract_body = memory_alloc;

//     let contract: String = 
//         format!("contract {} {{\n{}}}", name, contract_body);

//     let verifier = [
//         preamble,
//         contract,
//     ]
//         .join("\n");

//     verifier
// }

// pub fn generate_verifier_key<F, PC>(vk: ZkGarageVerifierKey<F, PC>) -> String
// where
//     F: PrimeField,
//     PC: HomomorphicCommitment<F>,
// {
//     [
//         format!("vk.domain_size = {}", vk.n),
//         format!("vk.num_inputs = {}", vk.n),

//     ]
// }

#[cfg(test)]
mod test {
    use crate::util::generate_verifier;

    #[test]
    fn test_verifier_generation() {
        println!("{}", generate_verifier("TestVerifier".into()));
    }
}