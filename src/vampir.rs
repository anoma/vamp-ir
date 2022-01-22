use crate::ast;
use std::collections::HashSet;
use std::marker::PhantomData;
use plonk_core::prelude::*;

use ark_ec::models::TEModelParameters;
use ark_ff::PrimeField;

#[derive(Default)]
pub struct Synthesizer<F, P>
where
    F: PrimeField,
    P: TEModelParameters<BaseField = F>,
{
    wires: HashSet<String>,
    pub_wires: HashSet<String>,
    out_wires: HashSet<String>,
    gates: Vec<ast::GateExpression>,
    _p: PhantomData<F>,
    __p: PhantomData<P>,
    // plonk_variables: HashMap<String, Variable>
}

impl<F, P> Synthesizer<F, P>
where
    F: PrimeField,
    P: TEModelParameters<BaseField = F>,
{
    pub fn synth(&mut self, ast_circuit: ast::Circuit) -> () {
        ast_circuit.statements.iter().for_each(|statement| {
            match statement {
                ast::Statement::PubStatement(st) => {
                    st.wires.iter().for_each(|id| {
                        self.wires.insert(id.value.clone());
                        self.pub_wires.insert(id.value.clone());
                    });
                },
                ast::Statement::AliasStatement(_) => {
                },
                ast::Statement::ConstraintStatement(st) => {
                    match st {
                        ast::ConstraintStatement::GateExpression(gate) => {
                            gate.expressions.iter().for_each(|id| {
                                self.wires.insert(id.value.clone());
                            });
                            self.gates.push(gate.clone());
                        },
                        _ => {}
                    }
                }
            }
        });
    }
}

impl<F, P> Circuit<F, P> for Synthesizer<F, P>
where
    F: PrimeField,
    P: TEModelParameters<BaseField = F>,
{
    const CIRCUIT_ID: [u8; 32] = [0xff; 32];

    fn gadget(
        &mut self,
        composer: &mut StandardComposer<F, P>,
    ) -> Result<(), Error> {
        let zero = composer.zero_var();
        let a = composer.add_input(F::zero());
        let b = composer.add_input(F::one());
        let c = composer.add_input(F::one());
        composer.width_4_poly_gate(a, b, c, zero, F::one(), F::zero(), F::zero(), F::one(), F::zero(), F::zero(), None);
        Ok(())
    }

    fn padded_circuit_size(&self) -> usize {
        1 << 9
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vampir;

    use ark_poly_commit::{PolynomialCommitment, sonic_pc::SonicKZG10};
    use rand::rngs::OsRng;
    use ark_bls12_381::{Bls12_381, Fr as BlsScalar};
    use ark_poly::polynomial::univariate::DensePolynomial;
    use ark_ed_on_bls12_381::EdwardsParameters as JubJubParameters;

    #[test]
    fn test_circuit_pub_and_gate() {
        let ast_circuit = ast::parse_circuit_from_string("
pub a d
gate a b
gate b c
");
        let mut circuit = vampir::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit.synth(ast_circuit);
        assert_eq!(circuit.pub_wires.len(), 2);
        assert_eq!(circuit.wires.len(), 4);
        assert_eq!(circuit.gates.len(), 2);
    }

    #[test]
    fn test_circuit_synthesis() {
        let ast_circuit = ast::parse_circuit_from_string("
pub x
pubout_poly_gate[0 1 0 0 0 0] y y y y x
poly_gate[1 0 0 0 0 4] y y y y
");
        let mut circuit = vampir::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit.synth(ast_circuit);
        type PC = SonicKZG10::<Bls12_381,DensePolynomial<BlsScalar>>;
        let pp = PC::setup(1 << 12, None, &mut OsRng).unwrap();

        let (_pk_p, _verifier_data) = circuit.compile::<PC>(&pp).unwrap();
    }
}
