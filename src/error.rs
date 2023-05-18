use crate::{ast::{TExpr, TPat, Variable}, typecheck::Type};

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

    // cannot match {} to any pattern in {}
    MatchError { expr1: TExpr, expr2: TExpr },

    // only list arguments to fold supported
    NonListArgumentsInFoldError,

    // encountered unexpected expression: {}
    UnexpectedExpression { expr: TExpr },

    // unexpected parameters for fresh: {:?}
    UnexpectedFreshParameters { params: Vec<TPat> },

    // unexpected arguments to iter: {:?}
    UnexpectedIterArguments { params: Vec<TPat> },

    // unexpected arguments to fold: {:?}
    UnexpectedArgumentsInFold { params: Vec<TPat> },
    
    // functions should have at least one parameter
    NoParameterInFunction,

    // Type errors

    // universally quantified types cannot be occurs checked
    OccursCheckError,

    // unable to match {:?} with {}
    VariableTypeError {v: Variable, t: Type},

    // unable to match {} with {}
    TypeError {t1: Type, t2: Type},


    // pattern {} cannot match {}
    PatternMatchError { pat: TPat, expr: TExpr},

    // the global function {} is undefined
    UndefinedGlobalFunction { v: Variable },

    // unable to determine type of global variable {}
    UnableDetermineType { v: Variable },

    // expression {} cannot have type {}
    ImpossibleType { e: TExpr, t: Type},

    // the global list {} is undefined
    UndefinedGlobalList { v: Variable },
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
