use crate::ast::{TExpr, TPat};

#[derive(Debug)]
pub enum Error {
    // Parse errors

    // Compilation errors

    // cannot apply {} to {}
    ApplicationError { expr2: TExpr, expr1: TExpr },

    // enountered empty sequence
    EmptySequenceError,

    // variables are not permitted in expression exponents
    VariableExponentError,

    // only constant arguments to iter supported
    NonConstantIterArgumentError,

    // cannot statically match {} against {}
    StaticMatchError { expr: TExpr, pat: TPat },

    MatchError { expr: TExpr, pat: TPat },

    // only list arguments to fold supported
    NonListArgumentsInFoldError,

    // encountered unexpected expression: {}
    UnexpectedExpression { expr: TExpr },

    // encountered unexpected pattern: {}
    UnexpectedPattern { pat: TPat },

    // only variable patterns should be present at this stage
    UnexpectedNonVariablePattern,

    // unexpected parameters for fresh: {:?}
    UnexpectedFreshParameters { params: Vec<TPat> },

    // unexpected arguments to iter: {:?}
    UnexpectedIterArguments { params: Vec<TPat> },

    // unexpected arguments to fold: {:?}
    UnexpectedArgumentsInFold { params: Vec<TPat> },
    // Type errors
}

// pub enum ParseError {

// }

// pub enum TypeError {

// }

// pub enum CompileError {

// }

// pub enum ProofError {
//     TooFewParameters,
// }

// pub enum VerifyError {
//     ProofVerificationError,
// }
