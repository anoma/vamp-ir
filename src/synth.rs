use crate::ast;
use std::collections::{HashSet, HashMap};
use std::marker::PhantomData;
use plonk_core::prelude::*;

use plonk_core::commitment::HomomorphicCommitment as HomomorphicCommitment;
use plonk_core::proof_system::proof::Proof as Proof;
use plonk_core::proof_system::verifier::Verifier as Verifier;
use plonk_core::proof_system::prover::Prover as Prover;

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
    /// Maps wire names to their values
    wire_vals: HashMap<String, String>,
    pub_wire_vals: HashMap<String, String>,
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

pub fn run_and_prove<F, P, PC> (
    u_params: &PC::UniversalParams,
    prover_key: ProverKey<F>,
    circuit: &mut Synthesizer<F, P>,
    public_inputs: &HashMap<String, String>,
    witnesses: &HashMap<String, String>
) -> Result<Proof<F, PC>, Error>
    where
        F: PrimeField,
        P: TEModelParameters<BaseField = F>,
        PC: HomomorphicCommitment<F>,
    {
    let circuit_size = circuit.padded_circuit_size();
    let (ck, _) = PC::trim(
        u_params,
        // +1 per wire, +2 for the permutation poly
        circuit_size + 6,
        0,
        None,
    ).unwrap();
    // New Prover instance
    let mut prover = Prover::new(b"test");
    // Fill witnesses for Prover
    circuit.pub_wire_vals = public_inputs.clone();
    circuit.wire_vals = witnesses.clone();

    circuit.gadget(prover.mut_cs())?;
    // Add ProverKey to Prover
    prover.prover_key = Some(prover_key);
    prover.prove(&ck)
}

pub fn verify<F, P, PC> (
    u_params: &PC::UniversalParams,
    plonk_verifier_key: VerifierKey<F, PC>,
    circuit: Synthesizer<F, P>,
    public_inputs: &HashMap<String, String>,
    proof: &Proof<F, PC>
) -> Result<(), Error> where
        F: PrimeField,
        P: TEModelParameters<BaseField = F>,
        PC: HomomorphicCommitment<F>,
    {
    let padded_circuit_size = plonk_verifier_key.padded_circuit_size();

    // Build public input vector
    let mut pi = vec![F::zero(); padded_circuit_size];
    circuit.pub_wires.iter().for_each(|(wire, pos)| {
        let val = public_inputs.get(&wire.clone()).unwrap();
        let val = F::from_str(val).unwrap_or(F::zero());
        if let Some(pos) = pos {
            pi.insert(*pos, -val)
        }
    });

    // Build verifier key
    let mut verifier: Verifier<F, P, PC> = Verifier::new(b"test");
    verifier.verifier_key = Some(plonk_verifier_key);
    let (_, vk) = PC::trim(
        u_params,
        // +1 per wire, +2 for the permutation poly
        padded_circuit_size + 6,
        0,
        None,
    ).unwrap();

    verifier.verify(
        proof,
        &vk,
        pi.as_slice(),
    )
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
        self.wires.iter_mut().for_each(|(wire, plonk_var)| {
            let val = match self.wire_vals.get(wire) {
                Some(s) => {
                    F::from_str(s).unwrap_or(F::zero())
                }
                _ => {
                    F::zero()
                }
            };
            *plonk_var = Some(composer.add_input(val));
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

                    let xp_val = match self.pub_wire_vals.get(&xp) {
                        Some(s) => {
                            F::from_str(s).unwrap_or(F::zero())
                        }
                        _ => {
                            F::zero()
                        }
                    };

                    let m = F::from_str(&gate.parameters.get(0).unwrap().value).unwrap_or(F::zero());
                    let l = F::from_str(&gate.parameters.get(1).unwrap().value).unwrap_or(F::zero());
                    let r = F::from_str(&gate.parameters.get(2).unwrap().value).unwrap_or(F::zero());
                    let o = F::from_str(&gate.parameters.get(3).unwrap().value).unwrap_or(F::zero());
                    let c = F::from_str(&gate.parameters.get(4).unwrap().value).unwrap_or(F::zero());
                    let f = F::from_str(&gate.parameters.get(5).unwrap().value).unwrap_or(F::zero());

                    composer.width_4_poly_gate(
                        xl, xr, xo, xf, m, l, r, o, c, f, Some(xp_val));
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
                "bit_range" => {
                    let x = self.wires.get(&gate.wires.get(0).unwrap().name.value).unwrap().unwrap();
                    let num_bits: usize = gate.parameters.get(0).unwrap().value.parse().unwrap();
                    composer.range_gate(x, num_bits);
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
        let pp = PC::setup(1 << 10, None, &mut OsRng).unwrap();

        let (pk_p, _verifier_data) = circuit.compile::<PC>(&pp).unwrap();
        assert_eq!(pk_p.n, 8);
    }

    #[test]
    fn synth_bit_range() {
        let ast_circuit = ast::parse_circuit_from_string("
pub x y
bit_range[4] x
bit_range[6] y
");
        let mut circuit = synth::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit.synth(ast_circuit);
        type PC = SonicKZG10::<Bls12_381,DensePolynomial<BlsScalar>>;
        let pp = PC::setup(1 << 10, None, &mut OsRng).unwrap();

        let (pk_p, _verifier_data) = circuit.compile::<PC>(&pp).unwrap();
        assert_eq!(pk_p.n, 16);
    }

    #[test]
    fn full_run() {
        // Generate CRS
        type PC = SonicKZG10::<Bls12_381,DensePolynomial<BlsScalar>>;
        let pp = PC::setup(1 << 10, None, &mut OsRng).unwrap();

        // Circuit
        let ast_circuit = ast::parse_circuit_from_string("
poly_gate[0 1 0 1 0 0] y y x y
");
        // Compile
        let mut circuit = synth::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit.synth(ast_circuit);
        let (pk_p, verifier_data) = circuit.compile::<PC>(&pp).unwrap();

        let public_inputs: HashMap<String, String> = HashMap::from([
                ("x".to_string(), "2".to_string()),
            ]);

        let witnesses: HashMap<String, String> = HashMap::from([
                ("y".to_string(), "2".to_string())
            ]);

        // Prover POV
        let proof = run_and_prove(&pp, pk_p, &mut circuit, &public_inputs, &witnesses).unwrap();

        // Verifier POV
        verify(&pp, verifier_data.key, circuit, &public_inputs, &proof).unwrap();
    }
}
