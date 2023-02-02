use ark_ff::FftField;
//use plonk_core::proof_system::VerifierKey as ZkGarageVerifierKey;
use plonk_core::proof_system::Proof as ZkGarageProof;
use plonk_core::proof_system::pi::PublicInputs;
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

type KZG10 = SonicKZG10<Bn254, DensePolynomial<Fr>>;
type PC = <KZG10 as PolynomialCommitment<<Bn254 as PairingEngine>::Fr, DensePolynomial<Fr>>>::Commitment;
type UniversalParams = <KZG10 as PolynomialCommitment<<Bn254 as PairingEngine>::Fr, DensePolynomial<Fr>>>::UniversalParams;
type ZkGaragePublicInputs = PublicInputs<Fr>;

#[derive(CanonicalDeserialize)]
pub struct ZkGaragePermutationVerifierKey
{
    /// Left Permutation Commitment
    pub left_sigma: PC,

    /// Right Permutation Commitment
    pub right_sigma: PC,

    /// Output Permutation Commitment
    pub out_sigma: PC,

    /// Fourth Permutation Commitment
    pub fourth_sigma: PC,
}

#[derive(CanonicalDeserialize)]
pub struct ZkGarageArithmeticVerifierKey
{
    /// Multiplication Selector Commitment
    pub q_m: PC,

    /// Left Selector Commitment
    pub q_l: PC,

    /// Right Selector Commitment
    pub q_r: PC,

    /// Output Selector Commitment
    pub q_o: PC,

    /// Fourth Selector Commitment
    pub q_4: PC,

    /// Constant Selector Commitment
    pub q_c: PC,

    /// Arithmetic Selector Commitment
    pub q_arith: PC,
}

#[derive(CanonicalDeserialize)]
pub struct ZkGarageLookupVerifierKey
{
    /// Lookup Selector Commitment
    pub q_lookup: PC,
    /// Commitment to first table column
    pub table_1: PC,
    /// Commitment to second table column
    pub table_2: PC,
    /// Commitment to third table column
    pub table_3: PC,
    /// Commitment to fourth table column
    pub table_4: PC,
}

#[derive(CanonicalDeserialize)]
pub struct ZkGarageVerifierKey
{
    /// Circuit size (not padded to a power of two).
    pub(crate) n: usize,

    /// Arithmetic Verifier Key
    pub(crate) arithmetic: ZkGarageArithmeticVerifierKey,

    /// Range Gate Selector Commitment
    pub(crate) range_selector_commitment: PC,

    /// Logic Gate Selector Commitment
    pub(crate) logic_selector_commitment: PC,

    /// Fixed Group Addition Selector Commitment
    pub(crate) fixed_group_add_selector_commitment: PC,

    /// Variable Group Addition Selector Commitment
    pub(crate) variable_group_add_selector_commitment: PC,

    /// VerifierKey for permutation checks
    pub(crate) permutation: ZkGaragePermutationVerifierKey,

    /// VerifierKey for Lookup Gate
    pub(crate) lookup: ZkGarageLookupVerifierKey,
}

#[derive(Debug)]
pub struct MatterLabsVerificationKey
{
    domain_size: [u8; 32],
    num_inputs: [u8; 32],
    omega: Fr,
    selector_commitments: [PC; 6], //ql, qr, qo, q4, qm, qc
    //next_step_selector_commitments: PC::Commitment,
    permutation_commitments: [PC; 4], // l, r, o, 4
    permutation_non_residues: [Fr; 3], // ????
    g2_x: G2Affine,
}

pub struct MatterLabsProof
{
    input_values: [u8; 32],
    wire_commitments: [PC; 4],
    grand_product_commitment: PC,
    quotient_poly_commitments: [PC; 4],
    wire_values_at_z: [Fr; 4],
    wire_values_at_z_omega: Fr,
    grand_product_at_z_omega: Fr,
    quotient_polynomial_at_z: Fr,
    linearization_polynomial_at_z: Fr,
    permutation_polynomials_at_z: [Fr; 3],

    opening_at_z_proof: PC,
    opening_at_z_omega_proof: PC,
}

pub struct MatterLabsPartialVerifierState
{
    alpha: Fr,
    beta: Fr,
    gamma: Fr,
    v: Fr,
    u: Fr,
    z: Fr,
    cached_lagrange_evals: Vec<Fr>,
}

pub fn adapt_verifier_key(
    zkg_vk: ZkGarageVerifierKey,
    pi: ZkGaragePublicInputs,
    universal_params: UniversalParams,
) ->
    MatterLabsVerificationKey
{
    let padded_size = zkg_vk.n.next_power_of_two();
    let domain_size = pad_left(padded_size);
    let num_inputs = pad_left(pi.get_vals().collect::<Vec<_>>().len());

    let domain =
        GeneralEvaluationDomain::<Fr>::new(padded_size).ok_or(Error::InvalidEvalDomainSize {
            log_size_of_group: padded_size.trailing_zeros(),
            adicity: <<Fr as FftField>::FftParams as ark_ff::FftParameters>::TWO_ADICITY,
        }).unwrap();
    
    let omega = domain.element(1);
    let selector_commitments = [
        zkg_vk.arithmetic.q_l,
        zkg_vk.arithmetic.q_r,
        zkg_vk.arithmetic.q_o,
        zkg_vk.arithmetic.q_4,
        zkg_vk.arithmetic.q_m,
        zkg_vk.arithmetic.q_c,
    ];

    let permutation_commitments = [
        zkg_vk.permutation.left_sigma,
        zkg_vk.permutation.right_sigma,
        zkg_vk.permutation.out_sigma,
        zkg_vk.permutation.fourth_sigma,
    ];

    let permutation_non_residues = [
        Fr::from(7_u64),
        Fr::from(13_u64),
        Fr::from(17_u64),
    ];

    let g2_x = universal_params.h;

    MatterLabsVerificationKey {
        domain_size,
        num_inputs,
        omega,
        selector_commitments,
        //next_step_selector_commitments: todo!(),
        permutation_commitments,
        permutation_non_residues,
        g2_x,
    }
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

fn pad_left(u: usize) -> [u8; 32] {
    let u_bytes = u.to_be_bytes();

    // let mut padded_u_bytes = vec![0u8; 32];
    // padded_u_bytes.splice((32-padded_u_bytes.len()).., padded_u_bytes);
    let mut padded_u_bytes = vec![0u8; 32-u_bytes.len()];
    padded_u_bytes.extend(u_bytes);
    padded_u_bytes.try_into().unwrap()
}

#[cfg(test)]
mod test {
    //use crate::util::generate_verifier;

    use std::fs::File;

    use ark_serialize::{CanonicalSerialize, CanonicalDeserialize};
    use plonk::prelude::Circuit;
    use rand_core::OsRng;

    use crate::{ast::Module, transform::compile, synth::{PrimeFieldOps, PlonkModule}, prompt_inputs, ProofData, CircuitData, util::{ZkGarageVerifierKey, MatterLabsVerificationKey, adapt_verifier_key}};


    // #[test]
    // fn test_verifier_generation() {
    //     println!("{}", generate_verifier("TestVerifier".into()));
    // }
    #[test]
    fn test_adapter() {
        use ark_bn254::{Bn254, Fr as BnScalar};
        use ark_ed_on_bn254::EdwardsParameters as InnerParameters;
        use ark_poly_commit::{sonic_pc::SonicKZG10, PolynomialCommitment};
        use ark_poly::polynomial::univariate::DensePolynomial;
        use plonk::error::to_pc_error;
        use ark_ec::PairingEngine;

        type PC = SonicKZG10<Bn254, DensePolynomial<BnScalar>>;
        type UniversalParams = <PC as PolynomialCommitment<<Bn254 as PairingEngine>::Fr, DensePolynomial<BnScalar>>>::UniversalParams;


        // Generate CRS
        println!("* Setting up public parameters...");
        let max_degree = 10;

        let pp = PC::setup(1 << max_degree, None, &mut OsRng)
            .map_err(to_pc_error::<BnScalar, PC>)
            .expect("unable to setup polynomial commitment scheme public parameters");
        // let mut pp_file = File::create(output)
        //     .expect("unable to create public parameters file");
        // if *unchecked {
        //     pp.serialize_unchecked(&mut pp_file)
        // } else {
        //     pp.serialize(&mut pp_file)
        // }.unwrap();
        println!("* Public parameter setup success!");

        // Compile
        println!("* Compiling constraints...");
        let unparsed_file = r"
        pub x;
        // Ensure that the given argument is 1 or 0, and returns it

        def bool x = { x*(x-1) = 0; x };

        // Extract the 8 bits from a number argument

        def range5 a = {
            def a0 = bool (fresh ((a\1) % 2));
            def a1 = bool (fresh ((a\2) % 2));
            def a2 = bool (fresh ((a\4) % 2));
            def a3 = bool (fresh ((a\8) % 2));
            def a4 = bool (fresh ((a\16) % 2));
            a = a0 + 2*a1 + 4*a2 + 8*a3 + 16*a4;
            (a0, a1, a2, a3, a4, ())
        };

        def (0, 1, ar) = range5 x;
        ";
        let module = Module::parse(&unparsed_file).unwrap();
        let module_3ac = compile(module, &PrimeFieldOps::<BnScalar>::default());
    
        // println!("* Reading public parameters...");
        // let mut pp_file = File::open(universal_params)
        //     .expect("unable to load public parameters file");
        // let pp = if *unchecked {
        //     UniversalParams::deserialize_unchecked(&mut pp_file)
        // } else {
        //     UniversalParams::deserialize(&mut pp_file)
        // }.unwrap();
    
        println!("* Synthesizing arithmetic circuit...");
        let mut circuit = PlonkModule::<BnScalar, InnerParameters>::new(module_3ac.clone());
        // Compile the circuit
        let (pk_p, vk) = circuit.compile::<PC>(&pp)
            .expect("unable to compile circuit");
        println!("* Serializing circuit to storage...");
        let mut verifier_key_file = File::create("test.vk")
            .expect("unable to create circuit file");
        vk.0.serialize(&mut verifier_key_file).unwrap();
    
        println!("* Constraint compilation success!");

        // Prove
        // println!("* Reading arithmetic circuit...");
        // let mut circuit_file = File::open(circuit)
        //     .expect("unable to load circuit file");
        // let CircuitData { pk_p, vk: _vk, mut circuit} =
        //     CircuitData::read(&mut circuit_file).unwrap();
    
        // Prover POV
        println!("* Soliciting circuit witnesses...");
        // Prompt for program inputs
        let var_assignments = prompt_inputs(&circuit.module);
        // Populate variable definitions
        circuit.populate_variables(var_assignments);

        // println!("* Reading public parameters...");
        // let mut pp_file = File::open(universal_params)
        //     .expect("unable to load public parameters file");
        // let pp = if *unchecked {
        //     UniversalParams::deserialize_unchecked(&mut pp_file)
        // } else {
        //     UniversalParams::deserialize(&mut pp_file)
        // }.unwrap();

        // Start proving witnesses
        println!("* Proving knowledge of witnesses...");
        let (proof, pi) = circuit.gen_proof::<PC>(&pp, pk_p, b"Test").unwrap();
    
        println!("* Serializing proof to storage...");
        let mut proof_file = File::create("test.proof")
            .expect("unable to create proof file");
        ProofData { proof, pi: pi.clone() }.serialize(&mut proof_file).unwrap();
    
        println!("* Proof generation success!");

        let mut vk_file = File::open("test.vk")
            .expect("unable to load test verifier key file");

        let zkg_vk: ZkGarageVerifierKey = ZkGarageVerifierKey::deserialize(&mut vk_file).unwrap();

        let ml_vk = adapt_verifier_key(zkg_vk, pi, pp);

        println!("Adapted key:\n{:?}", ml_vk);

    }
}