use crate::error::Error;

impl From<plonk_core::error::Error> for Error {
    fn from(error: plonk_core::error::Error) -> Self {
        match error {
            plonk_core::prelude::Error::InvalidEvalDomainSize { log_size_of_group: _, adicity: _ } => todo!(),
            plonk_core::prelude::Error::ProofVerificationError => Self::ProofVerificationFailure,
            plonk_core::prelude::Error::CircuitInputsNotFound => todo!(),
            plonk_core::prelude::Error::UninitializedPIGenerator => todo!(),
            plonk_core::prelude::Error::InvalidPublicInputBytes => Self::MalformedPublicInput,
            plonk_core::prelude::Error::InvalidPublicInputValue => Self::MalformedPublicInput,
            plonk_core::prelude::Error::CircuitAlreadyPreprocessed => todo!(),
            plonk_core::prelude::Error::MismatchedPolyLen => todo!(),
            plonk_core::prelude::Error::PCError { error } => 
                match error.as_ref() {
                    "Polynomial Commitment Error: TrimmingDegreeTooLarge" => Self::InsufficientParameters,
                    _ => Self::MalformedParameters
                }
            plonk_core::prelude::Error::DegreeIsZero => todo!(),
            plonk_core::prelude::Error::TruncatedDegreeTooLarge => todo!(),
            plonk_core::prelude::Error::TruncatedDegreeIsZero => todo!(),
            plonk_core::prelude::Error::PolynomialDegreeTooLarge => todo!(),
            plonk_core::prelude::Error::PolynomialDegreeIsZero => todo!(),
            plonk_core::prelude::Error::PairingCheckFailure => todo!(),
            plonk_core::prelude::Error::NotEnoughBytes => todo!(),
            plonk_core::prelude::Error::PointMalformed => todo!(),
            plonk_core::prelude::Error::ScalarMalformed => todo!(),
            plonk_core::prelude::Error::ElementNotIndexed => todo!(),
            plonk_core::prelude::Error::TablePreProcessingError => todo!(),
        }
    }
}