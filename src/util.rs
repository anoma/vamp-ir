use ark_ff::FftField;
use plonk::proof_system::verifier;
use plonk_core::proof_system::VerifierKey;
use plonk_core::proof_system::Proof;
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
use std::fs::{self, File, write};
use std::path::PathBuf;

type KZG10 = SonicKZG10<Bn254, DensePolynomial<Fr>>;
type PC = <KZG10 as PolynomialCommitment<<Bn254 as PairingEngine>::Fr, DensePolynomial<Fr>>>::Commitment;
type UniversalParams = <KZG10 as PolynomialCommitment<<Bn254 as PairingEngine>::Fr, DensePolynomial<Fr>>>::UniversalParams;



pub fn write_verifier(source: PathBuf, out: PathBuf, pi: PublicInputs<Fr>, params: UniversalParams)
{
    let mut zkg_vk_file = File::open(source)
        .expect("unable to open verifier key file");

    let vk: EthVerifierKey = EthVerifierKey::deserialize(&mut zkg_vk_file).unwrap();

    let template_string = fs::read_to_string("contracts/verifier_template.sol")
    .expect("unable to load verifier template file \"/contracts/verifier_template.sol\"");

    let verifier_strings: Vec<String> = template_string.split("ZkGarage.VerifierKey vk;")
        .map(|s| format!("{}", s))
        .collect();

    let vk_string = vk.write(pi, params);

    let verifier = format!("{}{}{}", verifier_strings[0], vk_string, verifier_strings[1]);

    fs::write(out, verifier)
        .expect("unable to write verifier to file");
}


#[derive(CanonicalDeserialize)]
pub struct EthPermutationVerifierKey
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

impl EthPermutationVerifierKey {
    fn write(self) -> String {
        let labels = vec![
            "left_sigma_comm",
            "right_sigma_comm",
            "out_sigma_comm",
            "fourth_sigma_comm",
        ];
        let values = vec![
            self.left_sigma,
            self.right_sigma,
            self.out_sigma,
            self.fourth_sigma,
        ]
            .iter()
            .map(|p| format!(
                "PairingsBn254.G1Point(0x{}, 0x{})",
                p.0.x.into_repr(),
                p.0.y.into_repr(),
            ))
            .collect::<Vec<String>>();
        
        format!("\tZkGarage.PermutationVerifierKey perm = ZkGarage.PermutationVerifierKey({{\n{}\n\t}});",
            labels
                .iter()
                .zip(values)
                .map(|(l, v)| format!("\t\t{}: {}", l ,v))
                .collect::<Vec<String>>()
                .join(",\n")
        )
    }
}

#[derive(CanonicalDeserialize)]
pub struct EthArithmeticVerifierKey
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

impl EthArithmeticVerifierKey {
    fn write(self) -> String {
        let labels = vec![
            "q_m_comm",
            "q_l_comm",
            "q_r_comm",
            "q_o_comm",
            "q_4_comm",       
            "q_c_comm",
            "q_arith_comm",
        ];
        let values = vec![
            self.q_m,
            self.q_l,
            self.q_r,
            self.q_o,
            self.q_4,
            self.q_c,
            self.q_arith
        ]
            .iter()
            .map(|p| format!(
                "PairingsBn254.G1Point(0x{}, 0x{})",
                p.0.x.into_repr(),
                p.0.y.into_repr(),
            ))
            .collect::<Vec<String>>();
        
        format!("ZkGarage.ArithmeticVerifierKey arith = ZkGarage.ArithmeticVerifierKey({{\n{}\n\t}});",
            labels
                .iter()
                .zip(values)
                .map(|(l, v)| format!("\t\t{}: {}", l ,v))
                .collect::<Vec<String>>()
                .join(",\n")
        )
    }
}

#[derive(CanonicalDeserialize)]
pub struct EthLookupVerifierKey
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

impl EthLookupVerifierKey {
    fn write(self) -> String {
        let labels = vec![
            "q_lookup_comm",
            "table_1_comm",
            "table_2_comm",
            "table_3_comm",
            "table_4_comm",
        ];
        let values = vec![
            self.q_lookup,
            self.table_1,
            self.table_2,
            self.table_3,
            self.table_4,
        ]
            .iter()
            .map(|p| format!(
                "PairingsBn254.G1Point(0x{}, 0x{})",
                p.0.x.into_repr(),
                p.0.y.into_repr(),
            ))
            .collect::<Vec<String>>();
        
        format!("\tZkGarage.LookupVerifierKey lookuparith = ZkGarage.LookupVerifierKey({{\n{}\n\t}});",
            labels
                .iter()
                .zip(values)
                .map(|(l, v)| format!("\t\t{}: {}", l ,v))
                .collect::<Vec<String>>()
                .join(",\n")
        )
    }
}

#[derive(CanonicalDeserialize)]
pub struct EthVerifierKey
{
    /// Circuit size (not padded to a power of two).
    pub(crate) n: usize,

    /// Arithmetic Verifier Key
    pub(crate) arithmetic: EthArithmeticVerifierKey,

    /// Range Gate Selector Commitment
    pub(crate) range_selector_commitment: PC,

    /// Logic Gate Selector Commitment
    pub(crate) logic_selector_commitment: PC,

    /// Fixed Group Addition Selector Commitment
    pub(crate) fixed_group_add_selector_commitment: PC,

    /// Variable Group Addition Selector Commitment
    pub(crate) variable_group_add_selector_commitment: PC,

    /// VerifierKey for permutation checks
    pub(crate) permutation: EthPermutationVerifierKey,

    /// VerifierKey for Lookup Gate
    pub(crate) lookup: EthLookupVerifierKey,
}

impl EthVerifierKey {
    fn write(self, pi: PublicInputs<Fr>, universal_params: UniversalParams) -> String {
        let domain_size = self.n.next_power_of_two();
        let num_inputs = pi.get_vals().collect::<Vec<_>>().len();

        let domain =
            GeneralEvaluationDomain::<Fr>::new(domain_size).ok_or(Error::InvalidEvalDomainSize {
                log_size_of_group: domain_size.trailing_zeros(),
                adicity: <<Fr as FftField>::FftParams as ark_ff::FftParameters>::TWO_ADICITY,
            }).unwrap();
        
        let omega = domain.element(1);

        let (k1, k2, k3) = (
            Fr::from(7_u64),
            Fr::from(13_u64),
            Fr::from(17_u64),
        );

        let g2_x = universal_params.h;

        let arith_string = self.arithmetic.write();
        let perm_string = self.permutation.write();

        let lines = vec![
            format!("domain_size: {},", domain_size),
            format!("num_inputs: {},", num_inputs),
            format!("omega: PairingsBn254.new_fr(0x{}),", omega.into_repr()),
            "arith: arith,".into(),
            "perm: perm,".into(),
            format!("k1: PairingsBn254.new_fr(0x{}),", k1.into_repr()),
            format!("k2: PairingsBn254.new_fr(0x{}),", k2.into_repr()),
            format!("k3: PairingsBn254.new_fr(0x{}),", k3.into_repr()),                        
            format!("g2_x: PairingsBn254.G2Point({{\n\t\t\tX: [\n\t\t\t\t0x{},\n\t\t\t\t0x{}\n\t\t\t],\n\t\t\tY: [\n\t\t\t\t0x{},\n\t\t\t\t0x{}\n\t\t\t]\n\t\t}})",
                g2_x.x.c0.into_repr(),
                g2_x.x.c1.into_repr(),
                g2_x.y.c0.into_repr(),
                g2_x.y.c1.into_repr(),
            ),

        ];       
        format!("{}\n{}\n\tZkGarage.VerifierKey vk = ZkGarage.VerifierKey({{\n{}\n\t}});",
            arith_string,
            perm_string,
            lines
                .iter()
                .map(|l| format!("\t\t{}", l))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

#[cfg(test)]
mod test {
    use std::fs::File;

    use ark_serialize::CanonicalSerialize;
    use plonk::prelude::Circuit;
    use rand_core::OsRng;

    use crate::{ast::Module, transform::compile, synth::{PrimeFieldOps, PlonkModule}, prompt_inputs, ProofData, CircuitData, util::{EthVerifierKey, write_verifier}};

    #[test]
    fn test_verifier_generation() {
        use ark_bn254::{Bn254, Fr as BnScalar};
        use ark_ed_on_bn254::EdwardsParameters as InnerParameters;
        use ark_poly_commit::{sonic_pc::SonicKZG10, PolynomialCommitment};
        use ark_poly::polynomial::univariate::DensePolynomial;
        use plonk::error::to_pc_error;
        use ark_ec::PairingEngine;

        type PC = SonicKZG10<Bn254, DensePolynomial<BnScalar>>;

        // Generate CRS
        println!("* Setting up public parameters...");
        let max_degree = 10;

        let pp = PC::setup(1 << max_degree, None, &mut OsRng)
            .map_err(to_pc_error::<BnScalar, PC>)
            .expect("unable to setup polynomial commitment scheme public parameters");

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
    
        // Prover POV
        println!("* Soliciting circuit witnesses...");
        // Prompt for program inputs
        let var_assignments = prompt_inputs(&circuit.module);
        // Populate variable definitions
        circuit.populate_variables(var_assignments);

        // Start proving witnesses
        println!("* Proving knowledge of witnesses...");
        let (proof, pi) = circuit.gen_proof::<PC>(&pp, pk_p, b"Test").unwrap();
    
        println!("* Serializing proof to storage...");
        let mut proof_file = File::create("test.proof")
            .expect("unable to create proof file");
        ProofData { proof, pi: pi.clone() }.serialize(&mut proof_file).unwrap();
    
        println!("* Proof generation success!");

        write_verifier(
            "test.vk".into(), 
            "test.verifier.sol".into(),
            pi,
            pp
        );

    }
}