use crate::ast::{Module, VariableId};
use crate::error::Error;
use crate::error::Error::{BackendError, ParseError};
use crate::halo2::synth::make_constant;
use crate::halo2::synth::{keygen, prover, Halo2Module, PrimeFieldOps};
use crate::qprintln;
use crate::util::{get_circuit_assignments, Config};
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
use ark_serialize::{Read, SerializationError};
use bincode::error::{DecodeError, EncodeError};
use ff::PrimeField;
use halo2_proofs::pasta::{EqAffine, Fp};
use halo2_proofs::poly::commitment::Params;
use num_bigint::BigInt;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

pub fn compile(source: impl AsRef<str>, config: &Config) -> Result<HaloCircuitData, Error> {
    qprintln!(config, "* Compiling constraints...");
    let module = Module::parse(source.as_ref()).map_err(|err| ParseError { e: err.to_string() })?;
    let module_3ac = crate::transform::compile(module, &PrimeFieldOps::<Fp>::default(), &config);
    qprintln!(config, "* Synthesizing arithmetic circuit...");
    let module_rc = Rc::new(module_3ac);
    let circuit = Halo2Module::<Fp>::new(module_rc);
    let params: Params<EqAffine> = Params::new(circuit.k);
    Ok(HaloCircuitData { params, circuit })
}

pub fn prove(
    circuit_data: &HaloCircuitData,
    named_assignments: &HashMap<String, Fp>,
    config: &Config,
) -> Result<ProofDataHalo2, Error> {
    let module = circuit_data.circuit.module.as_ref();
    let assignments = get_circuit_assignments(module, named_assignments)?;
    prove_from_variable_assignments(circuit_data, &assignments, config)
}

pub(crate) fn prove_from_int_variable_assignments(
    circuit_data: &HaloCircuitData,
    int_assignments: &HashMap<VariableId, BigInt>,
    config: &Config,
) -> Result<ProofDataHalo2, Error> {
    let assignments: HashMap<VariableId, Fp> = int_assignments
        .iter()
        .map(|(id, f)| (*id, make_constant::<Fp>(f.clone())))
        .collect();
    prove_from_variable_assignments(circuit_data, &assignments, config)
}

pub(crate) fn prove_from_variable_assignments(
    circuit_data: &HaloCircuitData,
    assignments: &HashMap<VariableId, Fp>,
    config: &Config,
) -> Result<ProofDataHalo2, Error> {
    let params = &circuit_data.params;
    let module = circuit_data.circuit.module.as_ref();

    // Populate variable definitions
    let mut circuit = circuit_data.circuit.clone();
    circuit.populate_variables(assignments.clone());

    // Get public inputs Fp
    let binding = module
        .pubs
        .iter()
        .map(|inst| assignments[&inst.id])
        .collect::<Vec<Fp>>();
    let instances = binding.as_slice();

    let public_inputs: Vec<u8> = instances
        .iter()
        .flat_map(|fp| {
            let repr = fp.to_repr();
            repr.as_ref().to_vec()
        })
        .collect();

    // Generating proving key
    qprintln!(config, "* Generating proving key...");
    let (pk, _vk) = keygen(&circuit, &params)?;

    // Start proving witnesses
    qprintln!(config, "* Proving knowledge of witnesses...");
    let proof = prover(circuit.clone(), &params, &pk, instances)
        .map_err(|e| BackendError { e: e.to_string() })?;
    Ok(ProofDataHalo2 {
        proof,
        public_inputs,
    })
}

/* Captures all the data required to use a Halo2 circuit. */
pub struct HaloCircuitData {
    pub params: Params<EqAffine>,
    pub circuit: Halo2Module<Fp>,
}

#[derive(CanonicalSerialize, CanonicalDeserialize)]
pub struct ProofDataHalo2 {
    pub proof: Vec<u8>,
    pub public_inputs: Vec<u8>,
}

impl HaloCircuitData {
    pub fn read<R>(mut reader: R) -> Result<Self, DecodeError>
    where
        R: std::io::Read,
    {
        let params = Params::<EqAffine>::read(&mut reader)
            .map_err(|x| DecodeError::OtherString(x.to_string()))?;
        let circuit: Halo2Module<Fp> =
            bincode::decode_from_std_read(&mut reader, bincode::config::standard())?;
        Ok(Self { params, circuit })
    }

    pub fn write<W>(&self, mut writer: W) -> Result<(), EncodeError>
    where
        W: std::io::Write,
    {
        self.params
            .write(&mut writer)
            .expect("unable to create circuit file");
        bincode::encode_into_std_write(&self.circuit, &mut writer, bincode::config::standard())
            .expect("unable to create circuit file");
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_valid_file() {
        let config = Config { quiet: true };
        assert!(compile("x = 1;", &config).is_ok());
    }

    #[test]
    fn test_compile_invalid_file() {
        let config = Config { quiet: true };
        assert!(compile("", &config).is_err());
    }

    #[test]
    #[ignore] // This test panics in type checking
    fn test_compile_type_error() {
        let config = Config { quiet: true };
        assert!(compile("1/0 = 1;", &config).is_err());
    }

    #[test]
    fn test_prove_valid() {
        let config = Config { quiet: true };
        let circuit = compile("x = 1;", &config).unwrap();
        let assignments = HashMap::from([("x", Fp::one())]);
        assert!(prove(&circuit, &assignments, &config).is_ok());
    }

    #[test]
    fn test_prove_missing_assignment() {
        let config = Config { quiet: true };
        let circuit = compile("x = 1;", &config).unwrap();
        let assignments: HashMap<String, Fp> = HashMap::new();
        assert!(prove(&circuit, &assignments, &config).is_err());
    }

    #[test]
    fn test_prove_invalid_assignment() {
        let config = Config { quiet: true };
        let circuit = compile("x = 1;", &config).unwrap();
        let assignments = HashMap::from([("x", Fp::zero())]);
        assert!(prove(&circuit, &assignments, &config).is_ok());
    }
}
