use crate::error::Error;

impl From<plonk_core::error::Error> for Error {
    fn from(error: plonk_core::error::Error) -> Self {
        match error {
           plonk_core::prelude::Error::PCError { error } => 
                match error.as_ref() {
                    "Polynomial Commitment Error: TrimmingDegreeTooLarge" => Self::InsufficientParameters,
                    _ => Self::BackendError { e: error.to_string()},
                }
        _ => Self::BackendError { e:  error.to_string()},
        }
    }
}