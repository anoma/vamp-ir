use crate::ast;
use std::collections::HashMap;
use std::marker::PhantomData;
use plonk_core::prelude::*;

use plonk_core::commitment::HomomorphicCommitment as HomomorphicCommitment;
use plonk_core::proof_system::proof::Proof as Proof;
use plonk_core::proof_system::verifier::Verifier as Verifier;
use plonk_core::proof_system::prover::Prover as Prover;
use plonk_core::error::to_pc_error;

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
    gates: Vec<ast::GateInvocation>,
    size: usize,
    _f: PhantomData<F>,
    _p: PhantomData<P>,
}

impl<F, P> Synthesizer<F, P>
where
    F: PrimeField,
    P: TEModelParameters<BaseField = F>,
{
    pub fn from_ast(&mut self, ast_circuit: ast::Circuit) -> () {
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

        // TODO: fill in the right size
        self.size = 1 << 9;
    }

    fn synth_using_composer(
        &mut self,
        composer: &mut StandardComposer<F, P>,
        pub_wire_vals: Option<&HashMap<String, String>>,
        wire_vals: Option<&HashMap<String, String>>,
    ) -> Result<(), Error> {
        let _zero = composer.zero_var();
        self.wires.iter_mut().for_each(|(wire, plonk_var)| {
            let val = if let Some(wire_vals) = wire_vals {
                match wire_vals.get(wire) {
                    Some(s) => {
                        F::from_str(s).unwrap_or(F::zero())
                    }
                    _ => F::zero()
                }
            } else {
                F::zero()
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

                    let m = F::from_str(&gate.parameters.get(0).unwrap().value).unwrap_or(F::zero());
                    let l = F::from_str(&gate.parameters.get(1).unwrap().value).unwrap_or(F::zero());
                    let r = F::from_str(&gate.parameters.get(2).unwrap().value).unwrap_or(F::zero());
                    let o = F::from_str(&gate.parameters.get(3).unwrap().value).unwrap_or(F::zero());
                    let c = F::from_str(&gate.parameters.get(4).unwrap().value).unwrap_or(F::zero());
                    let f = F::from_str(&gate.parameters.get(5).unwrap().value).unwrap_or(F::zero());

                    let xp_val = if let Some(pub_wire_vals) = pub_wire_vals {
                        match pub_wire_vals.get(&xp) {
                            Some(s) => {
                                F::from_str(s).unwrap_or(F::zero())
                            }
                            _ => {
                                F::zero()
                            }
                        }
                    } else {
                        F::zero()
                    };

                    composer.width_4_poly_gate(xl, xr, xo, xf, m, l, r, o, c, f, Some(xp_val));

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


    pub fn compile_prover<PC>(
        &mut self,
        u_params: &PC::UniversalParams,
    ) -> Result<ProverKey<F>, Error>
    where
        F: PrimeField,
        PC: HomomorphicCommitment<F>,
    {
        // Setup PublicParams
        let circuit_size = self.size;
        let (ck, _) = PC::trim(
            u_params,
            // +1 per wire, +2 for the permutation poly
            circuit_size + 6,
            0,
            None,
        )
        .map_err(to_pc_error::<F, PC>)?;

        // Generate & save `ProverKey` with some random values.
        let mut prover = Prover::<F, P, PC>::new(b"test");
        self.synth_using_composer(prover.mut_cs(), None, None)?;
        prover.preprocess(&ck)?;

        Ok(prover
            .prover_key
            .expect("Unexpected error. Missing ProverKey in compilation"))
    }

    pub fn compile_verifier<PC>(
        &mut self,
        u_params: &PC::UniversalParams,
    ) -> Result<VerifierKey<F, PC>, Error>
    where
        F: PrimeField,
        PC: HomomorphicCommitment<F>,
    {
        // Setup PublicParams
        let circuit_size = self.size;
        let (ck, _) = PC::trim(
            u_params,
            // +1 per wire, +2 for the permutation poly
            circuit_size + 6,
            0,
            None,
        )
        .map_err(to_pc_error::<F, PC>)?;

        // Generate & save `VerifierKey` with some random values.
        let mut verifier = Verifier::new(b"test");
        self.synth_using_composer(verifier.mut_cs(), None, None)?;
        verifier.preprocess(&ck)?;
        Ok(verifier.
            verifier_key.
            expect("Unexpected error. Missing VerifierKey in compilation"))
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
    let circuit_size = circuit.size;
    let (ck, _) = PC::trim(
        u_params,
        // +1 per wire, +2 for the permutation poly
        circuit_size + 6,
        0,
        None,
    ).unwrap();
    // New Prover instance
    let mut prover = Prover::new(b"test");

    circuit.synth_using_composer(prover.mut_cs(), Some(public_inputs), Some(witnesses))?;

    prover.mut_cs()
            .check_circuit_satisfied();

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
    // Build public input vector
    let mut pi = vec![F::zero(); circuit.size];
    circuit.pub_wires.iter().for_each(|(wire, pos)| {
        let val = public_inputs.get(&wire.clone()).unwrap();
        let val = F::from_str(val).unwrap_or(F::zero());
        if let Some(pos) = pos {
            pi[*pos] = -val
        }
    });

    // Build verifier key
    let mut verifier: Verifier<F, P, PC> = Verifier::new(b"test");
    verifier.verifier_key = Some(plonk_verifier_key);
    let (_, vk) = PC::trim(
        u_params,
        // +1 per wire, +2 for the permutation poly
        circuit.size + 6,
        0,
        None,
    ).unwrap();

    verifier.verify(
        proof,
        &vk,
        pi.as_slice(),
    )
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
        circuit.from_ast(ast_circuit);
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
        circuit.from_ast(ast_circuit);
        type PC = SonicKZG10::<Bls12_381,DensePolynomial<BlsScalar>>;
        let pp = PC::setup(1 << 10, None, &mut OsRng).unwrap();

        let pk_p = circuit.compile_prover::<PC>(&pp).unwrap();
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
        circuit.from_ast(ast_circuit);
        type PC = SonicKZG10::<Bls12_381,DensePolynomial<BlsScalar>>;
        let pp = PC::setup(1 << 10, None, &mut OsRng).unwrap();

        let pk_p = circuit.compile_prover::<PC>(&pp).unwrap();
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

        // Runtime inputs
        let public_inputs: HashMap<String, String> = HashMap::from([]);
        let witnesses: HashMap<String, String> = HashMap::from([
                ("x".to_string(), "1".to_string()),
                ("y".to_string(), "52435875175126190479447740508185965837690552500527637822603658699938581184512".to_string())
                // y = -1
            ]);

        // Prover POV
        let mut circuit_prover = synth::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit_prover.from_ast(ast_circuit.clone());
        let pk = circuit_prover.compile_prover::<PC>(&pp).unwrap();
        let proof = run_and_prove(&pp, pk, &mut circuit_prover, &public_inputs, &witnesses).unwrap();

        // Verifier POV
        let mut circuit_verifier = synth::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit_verifier.from_ast(ast_circuit.clone());
        let vk = circuit_verifier.compile_verifier::<PC>(&pp).unwrap();
        verify(&pp, vk, circuit_verifier, &public_inputs, &proof).unwrap();
    }
}
