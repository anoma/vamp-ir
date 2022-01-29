use plonk_ir::{ast, synth};

use ark_poly_commit::{PolynomialCommitment, sonic_pc::SonicKZG10};
use rand::rngs::OsRng;
use ark_bls12_381::{Bls12_381, Fr as BlsScalar};
use ark_poly::polynomial::univariate::DensePolynomial;
use ark_ed_on_bls12_381::EdwardsParameters as JubJubParameters;

fn main() {
        let ast_circuit = ast::parse_circuit_from_string("
pub x
pubout_poly_gate[0 1 0 0 0 0] y y y y x
poly_gate[1 0 0 0 0 4] y y y y
");
        let mut circuit = synth::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit.from_ast(ast_circuit);
        type PC = SonicKZG10::<Bls12_381,DensePolynomial<BlsScalar>>;
        let pp = PC::setup(1 << 12, None, &mut OsRng).unwrap();

        let pk = circuit.compile_prover::<PC>(&pp).unwrap();
        println!("{:?}", pk);
}

