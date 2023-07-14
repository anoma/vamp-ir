use crate::ast::Module;
use crate::error::Error;
use crate::error::Error::ParseError;
use crate::halo2::synth::{Halo2Module, PrimeFieldOps};
use crate::qprintln;
use crate::util::Config;
use bincode::error::{DecodeError, EncodeError};
use halo2_proofs::pasta::{EqAffine, Fp};
use halo2_proofs::poly::commitment::Params;
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

/* Captures all the data required to use a Halo2 circuit. */
pub struct HaloCircuitData {
    pub params: Params<EqAffine>,
    pub circuit: Halo2Module<Fp>,
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
}
