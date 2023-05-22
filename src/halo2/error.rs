use crate::error::Error;

impl From<halo2_proofs::plonk::Error> for Error {
    fn from(error: halo2_proofs::plonk::Error) -> Self {
        match error {
            halo2_proofs::plonk::Error::Opening =>
                Self::ProofVerificationFailure,
            halo2_proofs::plonk::Error::NotEnoughRowsAvailable { current_k: _ } => 
                Self::InsufficientParameters,
            _ => Self::BackendError { e:  error.to_string()},
        }
    }
}