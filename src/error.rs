use crate::{
    ast::{TExpr, TPat, Variable},
    typecheck::Type,
};

#[derive(Debug)]
pub enum Error {
    ParseError { e: String },

    // Compilation errors

    // cannot apply {} to {}
    ApplicationError { e2: TExpr, e1: TExpr },

    // encountered empty sequence
    EmptySequenceError,

    // variables are not permitted in expression exponents
    VariableExponentError,

    // only constant arguments to iter supported
    NonConstantIterArgumentError,

    // cannot statically match {} against {}
    StaticMatchError { e: TExpr, p: TPat },

    // cannot match {} to any pattern in {}
    MatchError { e1: TExpr, e2: TExpr },

    // only list arguments to fold supported
    NonListArgumentsInFoldError,

    // encountered unexpected expression: {}
    UnexpectedExpression { e: TExpr },

    // unexpected parameters for fresh: {:?}
    UnexpectedFreshParameters { params: Vec<TPat> },

    // unexpected arguments to iter: {:?}
    UnexpectedIterArguments { params: Vec<TPat> },

    // unexpected arguments to fold: {:?}
    UnexpectedArgumentsInFold { params: Vec<TPat> },

    // functions should have at least one parameter
    NoParameterInFunction,

    // Type errors

    // universally quantified types cannot be occurs-checked
    OccursCheckError,

    // unable to match {:?} with {}
    VariableTypeError { v: Variable, t: Type },

    // unable to match {} with {}
    TypeError { t1: Type, t2: Type },

    // pattern {} cannot match {}
    PatternMatchError { p: TPat, e: TExpr },

    // pattern cannot use the variable {} more than once
    DuplicatePatternVariable { v: Variable },

    // the global function {} is undefined
    UndefinedGlobalFunction { v: Variable },

    // unable to determine type of global variable {}
    UnableDetermineType { v: Variable },

    // expression {} cannot have type {}
    ImpossibleType { e: TExpr, t: Type },

    // the global list {} is undefined
    UndefinedGlobalList { v: Variable },

    // not enough parameters are available for this circuit
    InsufficientParameters,

    // general error from backend
    BackendError { e: String },

    // The user did not provide an assignment for a variable during proving
    MissingVariableAssignment { var_name: String },

    // A variable assignment has an invalid value
    InvalidVariableAssignmentValue { var_name: String },

    // proof fails to verify
    ProofVerificationFailure,

    // invalid field at repl
    InvalidField,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // cannot apply {} to {}
            Self::ApplicationError { e2, e1 } => write!(f, "Cannot apply {e2} to {e1}"),

            // encountered empty sequence
            Self::EmptySequenceError => write!(f, "Encountered empty sequence"),

            // variables are not permitted in expression exponents
            Self::VariableExponentError => {
                write!(f, "Variables are not permitted in expression exponents")
            }

            // only constant arguments to iter supported
            Self::NonConstantIterArgumentError => {
                write!(f, "Only constant arguments to iter supported")
            }

            // cannot statically match {} against {}
            Self::StaticMatchError { e, p } => {
                write!(f, "Cannot statically match {e} against {p}")
            }

            // cannot match {} to any pattern in {}
            Self::MatchError { e1, e2 } => {
                write!(f, "Cannot match {e1} to any pattern in {e2}")
            }

            // only list arguments to fold supported
            Self::NonListArgumentsInFoldError => write!(f, "Only list arguments to fold supported"),

            // encountered unexpected expression: {}
            Self::UnexpectedExpression { e } => {
                write!(f, "Encountered unexpected expression: {e}")
            }

            // unexpected parameters for fresh: {:?}
            Self::UnexpectedFreshParameters { params } => {
                write!(f, "unexpected parameters for fresh: {params:?}")
            }

            // unexpected arguments to iter: {:?}
            Self::UnexpectedIterArguments { params } => {
                write!(f, "Unexpected arguments to iter: {params:?}")
            }

            // unexpected arguments to fold: {:?}
            Self::UnexpectedArgumentsInFold { params } => {
                write!(f, "Unexpected arguments to fold: {params:?}")
            }

            // functions should have at least one parameter
            Self::NoParameterInFunction => {
                write!(f, "Functions should have at least one parameter")
            }

            // Type errors

            // universally quantified types cannot be occurs checked
            Self::OccursCheckError => {
                write!(f, "Universally quantified types cannot be occurs-checked")
            }

            // unable to match {:?} with {}
            Self::VariableTypeError { v, t } => write!(f, "Unable to match {v:?} with {t}"),

            // unable to match {} with {}
            Self::TypeError { t1, t2 } => write!(f, "Unable to match {t1} with {t2}"),

            // pattern {} cannot match {}
            Self::PatternMatchError { p, e } => write!(f, "Pattern {p} cannot match {e}"),

            // pattern cannot use the variable {} more than once
            Self::DuplicatePatternVariable { v } => {
                write!(f, "Pattern cannot use the variable {} more than once", v,)
            }

            // the global function {} is undefined
            Self::UndefinedGlobalFunction { v } => {
                write!(f, "The global function {v} is undefined")
            }

            // unable to determine type of global variable {}
            Self::UnableDetermineType { v } => write!(
                f,
                "Unable to determine type of global variable {v}: was this variable never used?"
            ),

            // expression {} cannot have type {}
            Self::ImpossibleType { e, t } => write!(f, "Expression {e} cannot have type {t}"),

            // the global list {} is undefined
            Self::UndefinedGlobalList { v } => write!(f, "The global list {v} is undefined"),

            // not enough parameters are available for this circuit
            Self::InsufficientParameters => write!(f, "Not enough parameters"),

            // general error from backend
            Self::BackendError { e } => write!(f, "Error in backend: {e}"),

            // proof fails to verify
            Self::ProofVerificationFailure => write!(f, "Proof failed to verify"),

            // invalid field at repl
            Self::InvalidField => write!(f, "Invalid field value"),

            Self::ParseError { e } => write!(f, "Error while parsing file: {e}"),

            Self::MissingVariableAssignment { var_name } => {
                write!(f, "Missing assignment for variable: {var_name}")
            }

            Self::InvalidVariableAssignmentValue { var_name } => write!(
                f,
                "The assignment for variable: {var_name} has an invalid value"
            ),
        }
    }
}
