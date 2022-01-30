use crate::ast;
use std::collections::{HashSet, HashMap};
use std::marker::PhantomData;
use plonk_core::prelude::*;

use plonk_core::commitment::HomomorphicCommitment as HomomorphicCommitment;
use plonk_core::proof_system::proof::Proof as Proof;
use plonk_core::proof_system::verifier::Verifier as Verifier;
use plonk_core::proof_system::prover::Prover as Prover;
use plonk_core::error::to_pc_error;

use ark_ec::models::TEModelParameters;
use ark_ff::PrimeField;


#[derive(Default, Debug)]
pub struct Gate {
    name: String,
    parameters: Vec<String>,
    /// Input wires
    wires: Vec<String>,
    out_wires: Vec<String>
}

#[derive(Default, Debug)]
pub struct Synthesizer<F, P>
where
    F: PrimeField,
    P: TEModelParameters<BaseField = F>,
{
    /// Maps wire names to plonk-core variables
    wires: HashMap<String, Option<Variable>>,
    /// Maps pub wire names to plonk-core pi positions
    pub_wires: HashMap<String, Option<usize>>,
    /// Set containing output wire names
    out_wires: HashSet<String>,
    gates: Vec<Gate>,
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
                            let name = gate.name.value.clone();
                            let wires = gate.wires.iter().map(|wire| {
                                let wire_name = wire.name.value.clone();
                                self.wires.insert(wire_name.clone(), None);
                                if wire.typ == "pub" {
                                    self.pub_wires.insert(wire_name.clone(), None);
                                }
                                wire_name
                            }).collect();
                            let parameters = gate.parameters.iter().map(|c| {
                                c.value.clone()
                            }).collect();
                            let gate: Gate = Gate { name, wires, out_wires: vec![], parameters };
                            self.gates.push(gate);
                        },
                        ast::ConstraintStatement::GateInvocationWithOutput(gate) => {
                            let name = gate.name.value.clone();
                            let wires = gate.wires.iter().map(|wire| {
                                let wire_name = wire.name.value.clone();
                                self.wires.insert(wire_name.clone(), None);
                                if wire.typ == "pub" {
                                    self.pub_wires.insert(wire_name.clone(), None);
                                }
                                wire_name
                            }).collect();
                            let out_wires = gate.out_wires.iter().map(|wire| {
                                let wire_name = wire.name.value.clone();
                                self.wires.insert(wire_name.clone(), None);
                                self.out_wires.insert(wire_name.clone());
                                if wire.typ == "pub" {
                                    self.pub_wires.insert(wire_name.clone(), None);
                                }
                                wire_name
                            }).collect();
                            let parameters = gate.parameters.iter().map(|c| {
                                c.value.clone()
                            }).collect();
                            let gate = Gate { name, wires, out_wires, parameters };
                            self.gates.push(gate);
                        },
                        _ => {}
                    }
                }
            }
        });

        // TODO: fill in the right size
        self.size = 1 << 6;
    }

    fn synth_using_composer(
        &mut self,
        composer: &mut StandardComposer<F, P>,
        pub_wire_vals: Option<&HashMap<String, String>>,
        wire_vals: Option<&HashMap<String, String>>,
    ) -> Result<HashMap<String, String>, Error> {
        let _zero = composer.zero_var();
        // Allocate wires
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
        // Synthesize circuit using standard composer
        self.gates.iter().for_each(|gate| {
            match gate.name.as_str() {
                "pubout_poly_gate" => {
                    let xl = self.wires.get(gate.wires.get(0).unwrap()).unwrap().unwrap();
                    let xr = self.wires.get(gate.wires.get(1).unwrap()).unwrap().unwrap();
                    let xo = self.wires.get(gate.wires.get(2).unwrap()).unwrap().unwrap();

                    let xp = gate.wires.get(3).unwrap().clone();
                    let pos = self.pub_wires.get_mut(&xp).unwrap();
                    *pos = Some(composer.circuit_size());

                    let m = F::from_str(gate.parameters.get(0).unwrap()).unwrap_or(F::zero());
                    let l = F::from_str(gate.parameters.get(1).unwrap()).unwrap_or(F::zero());
                    let r = F::from_str(gate.parameters.get(2).unwrap()).unwrap_or(F::zero());
                    let o = F::from_str(gate.parameters.get(3).unwrap()).unwrap_or(F::zero());
                    let c = F::from_str(gate.parameters.get(4).unwrap()).unwrap_or(F::zero());

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

                    composer.poly_gate(xl, xr, xo, m, l, r, o, c, Some(xp_val));

                },
                "poly_gate" => {
                    let xl = self.wires.get(gate.wires.get(0).unwrap()).unwrap().unwrap();
                    let xr = self.wires.get(gate.wires.get(1).unwrap()).unwrap().unwrap();
                    let xo = self.wires.get(gate.wires.get(2).unwrap()).unwrap().unwrap();

                    let m = F::from_str(gate.parameters.get(0).unwrap()).unwrap_or(F::zero());
                    let l = F::from_str(gate.parameters.get(1).unwrap()).unwrap_or(F::zero());
                    let r = F::from_str(gate.parameters.get(2).unwrap()).unwrap_or(F::zero());
                    let o = F::from_str(gate.parameters.get(3).unwrap()).unwrap_or(F::zero());
                    let c = F::from_str(gate.parameters.get(4).unwrap()).unwrap_or(F::zero());

                    composer.poly_gate(xl, xr, xo, m, l, r, o, c, None);
                },
                // out = add x y
                "add" => {
                    let x = self.wires.get(gate.wires.get(0).unwrap()).unwrap().unwrap();
                    let y = self.wires.get(gate.wires.get(1).unwrap()).unwrap().unwrap();

                    let out = composer.arithmetic_gate(|gate| {
                        gate.witness(x, y, None).add(F::one(), F::one())
                    });

                    let out_wire = gate.out_wires.get(0).unwrap();
                    self.wires.insert(out_wire.clone(), Some(out));
                },
                // out = mul x y
                "mul" => {
                    let x = self.wires.get(gate.wires.get(0).unwrap()).unwrap().unwrap();
                    let y = self.wires.get(gate.wires.get(1).unwrap()).unwrap().unwrap();

                    let out = composer.arithmetic_gate(|gate| {
                        gate.witness(x, y, None).mul(F::one())
                    });

                    let out_wire = gate.out_wires.get(0).unwrap();
                    self.wires.insert(out_wire.clone(), Some(out));
                },
                // bit_range[n], where n is even
                "bit_range" => {
                    let x = self.wires.get(gate.wires.get(0).unwrap()).unwrap().unwrap();
                    let num_bits: usize = gate.parameters.get(0).unwrap().parse().unwrap();
                    composer.range_gate(x, num_bits);
                },
                "bool" => {
                    let x = self.wires.get(gate.wires.get(0).unwrap()).unwrap().unwrap();
                    composer.boolean_gate(x);
                },
                // z = xor[n] x y, where n is even
                "xor" => {
                    let x = self.wires.get(gate.wires.get(0).unwrap()).unwrap().unwrap();
                    let y = self.wires.get(gate.wires.get(1).unwrap()).unwrap().unwrap();

                    let num_bits: usize = gate.parameters.get(0).unwrap().parse().unwrap();
                    let out = composer.xor_gate(x, y, num_bits);
                    let out_wire = gate.out_wires.get(0).unwrap();
                    self.wires.insert(out_wire.clone(), Some(out));
                },
                // z = and[n] x y, where n is even
                "and" => {
                    let x = self.wires.get(gate.wires.get(0).unwrap()).unwrap().unwrap();
                    let y = self.wires.get(gate.wires.get(1).unwrap()).unwrap().unwrap();

                    let num_bits: usize = gate.parameters.get(0).unwrap().parse().unwrap();
                    let out = composer.and_gate(x, y, num_bits);
                    let out_wire = gate.out_wires.get(0).unwrap();
                    self.wires.insert(out_wire.clone(), Some(out));
                },
                // z = cselect bit x y, where n is even
                "cselect" => {
                    let bit = self.wires.get(gate.wires.get(0).unwrap()).unwrap().unwrap();
                    let opt_0 = self.wires.get(gate.wires.get(1).unwrap()).unwrap().unwrap();
                    let opt_1 = self.wires.get(gate.wires.get(2).unwrap()).unwrap().unwrap();

                    let out = composer.conditional_select(bit, opt_0, opt_1);
                    let out_wire = gate.out_wires.get(0).unwrap();
                    self.wires.insert(out_wire.clone(), Some(out));
                },
                _ => {

                }
            }
        });
        let pubout: HashMap<String, String> = HashMap::from_iter(self.out_wires.iter().filter_map(|wire| {
            if self.pub_wires.contains_key(wire) {
                let wire_var = self.wires.get(wire).unwrap().unwrap();
                let val = composer.variables.get(&wire_var).unwrap().to_string();
                Some((wire.clone(), val))
            } else { None }
        }));
        Ok(pubout)
    }

    pub fn compile_prover_and_verifier<PC>(
        &mut self,
        u_params: &PC::UniversalParams,
    ) -> Result<(ProverKey<F>, VerifierKey<F, PC>), Error>
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

        // Generate & save `VerifierKey` with some random values.
        let mut verifier = Verifier::new(b"test");
        self.synth_using_composer(verifier.mut_cs(), None, None)?;
        verifier.preprocess(&ck)?;

        Ok((prover
            .prover_key
            .expect("Unexpected error. Missing ProverKey in compilation"),
            verifier.
            verifier_key.
            expect("Unexpected error. Missing VerifierKey in compilation")
        ))
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
) -> Result<(Proof<F, PC>, HashMap<String, String>), Error>
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

    let pubout = circuit.synth_using_composer(prover.mut_cs(), Some(public_inputs), Some(witnesses))?;

    // prover.mut_cs()
    //         .check_circuit_satisfied();

    // Add ProverKey to Prover
    prover.prover_key = Some(prover_key);
    let proof = prover.prove(&ck)?;

    Ok((proof, pubout))
}

pub fn verify<F, P, PC> (
    u_params: &PC::UniversalParams,
    plonk_verifier_key: VerifierKey<F, PC>,
    circuit: &mut Synthesizer<F, P>,
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
            pi[*pos] = val
        }
    });

    // Build verifier key
    let mut verifier: Verifier<F, P, PC> = Verifier::new(b"test");
    verifier.verifier_key = Some(plonk_verifier_key);
    let (_ck, vk) = PC::trim(
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
pubout_poly_gate[0 1 0 0 0] y y y x
poly_gate[1 0 0 0 0] y y y y
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
    fn pubout_poly_gate() {
        // Generate CRS
        type PC = SonicKZG10::<Bls12_381,DensePolynomial<BlsScalar>>;
        let pp = PC::setup(1 << 12, None, &mut OsRng).unwrap();

        // Circuit
        let ast_circuit = ast::parse_circuit_from_string("
pub x
pubout_poly_gate[0 1 0 0 0] y y y x
");

        // Runtime inputs
        let public_inputs: HashMap<String, String> = HashMap::from([
                ("x".to_string(), "1".to_string()),
        ]);
        let witnesses: HashMap<String, String> = HashMap::from([
                ("y".to_string(), "52435875175126190479447740508185965837690552500527637822603658699938581184512".to_string())
                // y = -1
            ]);


        let mut circuit = synth::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit.from_ast(ast_circuit.clone());
        let (pk, vk) = circuit.compile_prover_and_verifier::<PC>(&pp).unwrap();

        // Prover POV
        let mut circuit_prover = synth::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit_prover.from_ast(ast_circuit.clone());
        let pk2 = circuit_prover.compile_prover::<PC>(&pp).unwrap();
        assert_eq!(pk, pk2);
        let (proof, _) = run_and_prove(&pp, pk, &mut circuit_prover, &public_inputs, &witnesses).unwrap();

        // Verifier POV
        let mut circuit_verifier = synth::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit_verifier.from_ast(ast_circuit.clone());
        let vk2 = circuit_verifier.compile_verifier::<PC>(&pp).unwrap();
        assert_eq!(vk, vk2);
        verify(&pp, vk, &mut circuit_verifier, &public_inputs, &proof).unwrap();
    }

    #[test]
    fn poly_gate() {
        // Generate CRS
        type PC = SonicKZG10::<Bls12_381,DensePolynomial<BlsScalar>>;
        let pp = PC::setup(1 << 12, None, &mut OsRng).unwrap();

        // Circuit
        let ast_circuit = ast::parse_circuit_from_string("
poly_gate[0 1 0 1 0] y y x
");

        // Runtime inputs
        let public_inputs: HashMap<String, String> = HashMap::from([]);
        let witnesses: HashMap<String, String> = HashMap::from([
                ("x".to_string(), "1".to_string()),
                ("y".to_string(), "52435875175126190479447740508185965837690552500527637822603658699938581184512".to_string())
                // y = -1
            ]);


        let mut circuit = synth::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit.from_ast(ast_circuit.clone());
        let (pk, vk) = circuit.compile_prover_and_verifier::<PC>(&pp).unwrap();

        // Prover POV
        let mut circuit_prover = synth::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit_prover.from_ast(ast_circuit.clone());
        let pk2 = circuit_prover.compile_prover::<PC>(&pp).unwrap();
        assert_eq!(pk, pk2);
        let (proof, _) = run_and_prove(&pp, pk, &mut circuit_prover, &public_inputs, &witnesses).unwrap();

        // Verifier POV
        let mut circuit_verifier = synth::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit_verifier.from_ast(ast_circuit.clone());
        let vk2 = circuit_verifier.compile_verifier::<PC>(&pp).unwrap();
        assert_eq!(vk, vk2);
        verify(&pp, vk, &mut circuit_verifier, &public_inputs, &proof).unwrap();
    }

    #[test]
    fn add_and_mul() {
        // Generate CRS
        type PC = SonicKZG10::<Bls12_381,DensePolynomial<BlsScalar>>;
        let pp = PC::setup(1 << 12, None, &mut OsRng).unwrap();

        // Circuit
        let ast_circuit = ast::parse_circuit_from_string("
pub z
a = add x y
b = mul x y
z = add a b
");

        // Runtime inputs
        let public_inputs: HashMap<String, String> = HashMap::from([]);
        let witnesses: HashMap<String, String> = HashMap::from([
                ("x".to_string(), "2".to_string()),
                ("y".to_string(), "3".to_string())
                // y = -1
            ]);


        let mut circuit = synth::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit.from_ast(ast_circuit.clone());
        let (pk, vk) = circuit.compile_prover_and_verifier::<PC>(&pp).unwrap();

        // Prover POV
        let mut circuit_prover = synth::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit_prover.from_ast(ast_circuit.clone());
        let pk2 = circuit_prover.compile_prover::<PC>(&pp).unwrap();
        assert_eq!(pk, pk2);
        let (proof, pubout) = run_and_prove(&pp, pk, &mut circuit_prover, &public_inputs, &witnesses).unwrap();

        // Verifier POV
        let mut circuit_verifier = synth::Synthesizer::<BlsScalar, JubJubParameters>::default();
        circuit_verifier.from_ast(ast_circuit.clone());
        let vk2 = circuit_verifier.compile_verifier::<PC>(&pp).unwrap();
        assert_eq!(vk, vk2);
        verify(&pp, vk, &mut circuit_verifier, &pubout, &proof).unwrap();
    }
}
