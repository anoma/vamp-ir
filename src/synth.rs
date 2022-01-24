use crate::ast;
use std::collections::{HashSet, HashMap};
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
    /// Maps wire names to plonk-core variables
    wires: HashMap<String, Option<Variable>>,
    /// Maps pub wire names to plonk-core pi positions
    pub_wires: HashMap<String, Option<usize>>,
    _out_wires: HashSet<String>,
    gates: Vec<ast::GateInvocation>,
    _f: PhantomData<F>,
    _p: PhantomData<P>,
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
                        self.wires.insert(id.value.clone(), None);
                        self.pub_wires.insert(id.value.clone(), None);
                    });
                },
                ast::Statement::AliasStatement(_) => {
                },
                ast::Statement::ConstraintStatement(st) => {
                    match st {
                        ast::ConstraintStatement::GateInvocation(gate) => {
                            gate.wires.iter().for_each(|id| {
                                self.wires.insert(id.name.value.clone(), None);
                                println!("{}", id.typ);
                                if id.typ == "pub" {
                                    self.pub_wires.insert(id.name.value.clone(), None);
                                }
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
        let _zero = composer.zero_var();
        self.wires.iter_mut().for_each(|(_, plonk_var)| {
            *plonk_var = Some(composer.add_input(F::zero()));
        });
        self.gates.iter().for_each(|gate| {
            match gate.name.value.as_str() {
                "pubout_poly_gate" => {
                    let xl = self.wires.get(&gate.wires.get(0).unwrap().name.value).unwrap().unwrap();
                    let xr = self.wires.get(&gate.wires.get(1).unwrap().name.value).unwrap().unwrap();
                    let xo = self.wires.get(&gate.wires.get(2).unwrap().name.value).unwrap().unwrap();
                    let xf = self.wires.get(&gate.wires.get(3).unwrap().name.value).unwrap().unwrap();

                    let xp = gate.wires.get(4).unwrap().name.value.clone();
                    // assert!(self.pub_wires.contains(&xp));
                    // assert!(!self.pub_wire_pos.contains_key(&xp));
                    let pos = self.pub_wires.get_mut(&xp).unwrap();
                    *pos = Some(composer.circuit_size());

                    let m = F::from_str(&gate.parameters.get(0).unwrap().value).unwrap_or(F::zero());
                    let l = F::from_str(&gate.parameters.get(1).unwrap().value).unwrap_or(F::zero());
                    let r = F::from_str(&gate.parameters.get(2).unwrap().value).unwrap_or(F::zero());
                    let o = F::from_str(&gate.parameters.get(3).unwrap().value).unwrap_or(F::zero());
                    let c = F::from_str(&gate.parameters.get(4).unwrap().value).unwrap_or(F::zero());
                    let f = F::from_str(&gate.parameters.get(5).unwrap().value).unwrap_or(F::zero());

                    composer.width_4_poly_gate(
                        xl, xr, xo, xf, m, l, r, o, c, f, Some(F::zero()));
                },
                "poly_gate" => {
                    let xl = self.wires.get(&gate.wires.get(0).unwrap().name.value).unwrap().unwrap();
                    let xr = self.wires.get(&gate.wires.get(1).unwrap().name.value).unwrap().unwrap();
                    let xo = self.wires.get(&gate.wires.get(2).unwrap().name.value).unwrap().unwrap();
                    let xf = self.wires.get(&gate.wires.get(3).unwrap().name.value).unwrap().unwrap();

                    let m = F::from_str(&gate.parameters.get(0).unwrap().value).unwrap_or(F::zero());
                    let l = F::from_str(&gate.parameters.get(1).unwrap().value).unwrap_or(F::zero());
                    let r = F::from_str(&gate.parameters.get(2).unwrap().value).unwrap_or(F::zero());
                    let o = F::from_str(&gate.parameters.get(3).unwrap().value).unwrap_or(F::zero());
                    let c = F::from_str(&gate.parameters.get(4).unwrap().value).unwrap_or(F::zero());
                    let f = F::from_str(&gate.parameters.get(5).unwrap().value).unwrap_or(F::zero());

                    composer.width_4_poly_gate(
                        xl, xr, xo, xf, m, l, r, o, c, f, None);
                },
                _ => {

                }
            }
        });
        Ok(())
    }

    fn padded_circuit_size(&self) -> usize {
        1 << 9
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::synth;

    use ark_poly_commit::{PolynomialCommitment, sonic_pc::SonicKZG10};
    use rand::rngs::OsRng;
    use ark_bls12_381::{Bls12_381, Fr as BlsScalar};
    use ark_poly::polynomial::univariate::DensePolynomial;
    use ark_ed_on_bls12_381::EdwardsParameters as JubJubParameters;

    #[test]
    fn synth_pub_and_gate() {
        let ast_circuit = ast::parse_circuit_from_string("
pub a d
gate a pub b
gate b c
");
        let mut circuit = synth::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit.synth(ast_circuit);
        assert_eq!(circuit.pub_wires.len(), 3);
        assert_eq!(circuit.wires.len(), 4);
        assert_eq!(circuit.gates.len(), 2);
    }

    #[test]
    fn synth_builtin() {
        let ast_circuit = ast::parse_circuit_from_string("
pub x
pubout_poly_gate[0 1 0 0 0 0] y y y y x
poly_gate[1 0 0 0 0 4] y y y y
");
        let mut circuit = synth::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit.synth(ast_circuit);
        type PC = SonicKZG10::<Bls12_381,DensePolynomial<BlsScalar>>;
        let pp = PC::setup(1 << 12, None, &mut OsRng).unwrap();

        let (pk_p, _verifier_data) = circuit.compile::<PC>(&pp).unwrap();
        assert_eq!(pk_p.n, 8);
    }
}
