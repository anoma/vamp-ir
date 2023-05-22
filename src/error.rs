use crate::{ast::{TExpr, TPat, Variable}, typecheck::Type};

#[derive(Debug)]
pub enum Error {
    // Compilation errors

    // cannot apply {} to {}
    ApplicationError { e2: TExpr, e1: TExpr },

    // enountered empty sequence
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
    VariableTypeError {v: Variable, t: Type},

    // unable to match {} with {}
    TypeError {t1: Type, t2: Type},

    // pattern {} cannot match {}
    PatternMatchError { p: TPat, e: TExpr},

    // the global function {} is undefined
    UndefinedGlobalFunction { v: Variable },

    // unable to determine type of global variable {}
    UnableDetermineType { v: Variable },

    // expression {} cannot have type {}
    ImpossibleType { e: TExpr, t: Type},

    // the global list {} is undefined
    UndefinedGlobalList { v: Variable },

    // Proof System errors
    InsufficientParameters,
    MalformedParameters,
    MalformedProof,
    MalformedProverKey,
    MalformedVerifierKey,
    MalformedWitness,
    MalformedPublicInput,
    ProofVerificationFailure,
}


impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // cannot apply {} to {}
            Self::ApplicationError { e2,e1 } => 
                write!(f, "cannot apply {} to {}", e2, e1),

            // enountered empty sequence
            Self::EmptySequenceError =>
                write!(f, "enountered empty sequence"),

            // variables are not permitted in expression exponents
            Self::VariableExponentError =>
                write!(f, "variables are not permitted in expression exponents"),

            // only constant arguments to iter supported
            Self::NonConstantIterArgumentError =>
                write!(f, "only constant arguments to iter supported"),

            // cannot statically match {} against {}
            Self::StaticMatchError { e, p } =>
                write!(f, "cannot statically match {} against {}", e, p),

            // cannot match {} to any pattern in {}
            Self::MatchError { e1, e2 } =>
                write!(f, "cannot match {} to any pattern in {}", e1, e2),

            // only list arguments to fold supported
            Self::NonListArgumentsInFoldError =>
                write!(f, "only list arguments to fold supported"),

            // encountered unexpected expression: {}
            Self::UnexpectedExpression { e } =>
                write!(f, "encountered unexpected expression: {}", e),

            // unexpected parameters for fresh: {:?}
            Self::UnexpectedFreshParameters { params } =>
                write!(f, "unexpected parameters for fresh: {:?}", params),

            // unexpected arguments to iter: {:?}
            Self::UnexpectedIterArguments { params} =>
                write!(f, "unexpected arguments to iter: {:?}", params),

            // unexpected arguments to fold: {:?}
            Self::UnexpectedArgumentsInFold { params} =>
                write!(f, "unexpected arguments to fold: {:?}", params),          

            // functions should have at least one parameter
            Self::NoParameterInFunction =>
                write!(f, "functions should have at least one parameter"),
                
            // Type errors

            // universally quantified types cannot be occurs checked
            Self::OccursCheckError =>
                write!(f, "universally quantified types cannot be occurs checked"),

            // unable to match {:?} with {}
            Self::VariableTypeError {v, t} =>
                write!(f, "unable to match {:?} with {}", v, t),
            
            // unable to match {} with {}
            Self::TypeError {t1, t2} =>
                write!(f, "unable to match {} with {}", t1, t2),
            
            // pattern {} cannot match {}
            Self::PatternMatchError { p, e} =>
                write!(f, "pattern {} cannot match {}", p, e),
            
            // the global function {} is undefined
            Self::UndefinedGlobalFunction { v } =>
                write!(f, "the global function {} is undefined", v),
            
            // unable to determine type of global variable {}
            Self::UnableDetermineType { v } =>
                write!(f, "unable to determine type of global variable {}", v),
            
            // expression {} cannot have type {}
            Self::ImpossibleType { e, t} =>
                write!(f, "expression {} cannot have type {}", e, t),
            
            // the global list {} is undefined
            Self::UndefinedGlobalList { v } =>
                write!(f, "the global list {} is undefined", v),
            
            // Proof System errors
            Self::InsufficientParameters =>
                write!(f, "not enough parameters"),
            
            Self::MalformedParameters =>
                write!(f, "malformed parameters"),
            
            Self::MalformedProof =>
                write!(f, "malformed proof"),
            
            Self::MalformedProverKey =>
                write!(f, "malformed prover key"),
            
            Self::MalformedVerifierKey =>
                write!(f, "malformed verifier key"),
            
            Self::MalformedWitness =>
                write!(f, "malformed witness"),

            Self::MalformedPublicInput =>
                write!(f, "malformed public input"),
            
            Self::ProofVerificationFailure =>
                write!(f, "proof verification failure"),
        }
    }
}