use pest::{Parser, Span};
use from_pest::FromPest;

#[derive(Parser)]
#[grammar = "vampir.pest"]
struct VampirParser;

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::nonneg))]
pub struct Nonneg {
    #[pest_ast(outer(with(span_into_str)))]
    pub value: String,
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::integer))]
pub struct Integer {
    #[pest_ast(outer(with(span_into_str)))]
    pub value: String,
}

// #[derive(Debug, FromPest, PartialEq, Clone)]
// #[pest_ast(rule(Rule::identifier))]
// pub struct Identifier {
//     #[pest_ast(outer(with(span_into_str)))]
//     pub value: String,
// }

// #[derive(Debug, FromPest, PartialEq, Clone)]
// #[pest_ast(rule(Rule::wire))]
// pub struct Wire{
//     #[pest_ast(outer(rule(Rule::wire_type), with(span_into_str)))]
//     pub typ: String,
//     pub name: Identifier
// }


// #[derive(Debug, FromPest, PartialEq, Clone)]
// #[pest_ast(rule(Rule::statement))]
// pub enum Statement {
//     PubStatement(PubStatement),
//     AliasStatement(AliasStatement),
//     ConstraintStatement(ConstraintStatement)
// }

// #[derive(Debug, FromPest, PartialEq, Clone)]
// #[pest_ast(rule(Rule::pub_statement))]
// pub struct PubStatement {
//     pub wires: Vec<Identifier>
// }

// #[derive(Debug, FromPest, PartialEq, Clone)]
// #[pest_ast(rule(Rule::alias_statement))]
// pub struct AliasStatement {
//     pub name: Identifier,
//     pub inputs: Vec<Wire>,
//     pub _arrow: Option<AliasArrow>,
//     pub outputs: Vec<Wire>,
//     pub body: Vec<ConstraintStatement>,
// }

// #[derive(Debug, FromPest, PartialEq, Clone)]
// #[pest_ast(rule(Rule::alias_arrow))]
// pub struct AliasArrow;

// #[derive(Debug, FromPest, PartialEq, Clone)]
// #[pest_ast(rule(Rule::constraint_statement))]
// pub enum ConstraintStatement {
//     GateInvocationWithOutput(GateInvocationWithOutput),
//     EqualConstraint(Expression, Expression),
//     GateInvocation(GateInvocation)
// }

// #[derive(Debug, FromPest, PartialEq, Clone)]
// #[pest_ast(rule(Rule::expression))]
// pub enum Expression {
//     GateInvocation(GateInvocation),
//     PolyExpression(PolyExpression)
// }

// //TODO

// #[derive(Debug, FromPest, PartialEq, Clone)]
// #[pest_ast(rule(Rule::base))]
// pub enum Base {
//     Constant(Constant),
//     Wire(Wire),
//     GateInvocation(GateInvocation),
// }

// #[derive(Debug, FromPest, PartialEq, Clone)]
// #[pest_ast(rule(Rule::monomial))]
// pub struct Monomial {
//     pub base: Base,
//     pub _exponent: Option<Constant>,
// }

// #[derive(Debug, FromPest, PartialEq, Clone)]
// #[pest_ast(rule(Rule::term))]
// pub struct Term {
//     monomials: Vec<Monomial>,
// }

// #[derive(Debug, FromPest, PartialEq, Clone)]
// #[pest_ast(rule(Rule::signed_term))]
// pub struct SignedTerm {
//     pub _sign: Option<Op>,
//     pub term: Term,
// }

// #[derive(Debug, FromPest, PartialEq, Clone)]
// #[pest_ast(rule(Rule::op))]
// pub struct Op {
//     #[pest_ast(outer(with(span_into_str)))]
//     pub value: String,
// }

// #[derive(Debug, FromPest, PartialEq, Clone)]
// #[pest_ast(rule(Rule::poly))]
// pub struct PolyExpression {
//     terms: Vec<SignedTerm>,
// }

// #[derive(Debug, FromPest, PartialEq, Clone)]
// #[pest_ast(rule(Rule::gate_invocation))]
// pub struct GateInvocation {
//     pub name: Identifier,
//     pub parameters: Vec<Constant>,
//     // TODO: Support general expressions here
//     pub wires: Vec<Wire>
// }

// #[derive(Debug, FromPest, PartialEq, Clone)]
// #[pest_ast(rule(Rule::gate_invocation_with_output))]
// pub struct GateInvocationWithOutput {
//     pub out_wires: Vec<Wire>,
//     pub name: Identifier,
//     pub parameters: Vec<Constant>,
//     // TODO: Support general expressions here
//     pub wires: Vec<Wire>
// }


#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::constraint))]
pub struct Constraint {

}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::circuit))]
pub struct Circuit {
    pub constraints: Vec<Constraint>,
    //pub eoi: EOI
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::EOI))]
pub struct EOI;

impl Circuit {
    pub fn size(&self) -> usize {
        self.statements.len()
    }
}

pub fn parse_circuit_from_string(input: &str) -> Circuit {
    let mut pest_output = VampirParser::parse(Rule::circuit, input).unwrap();
    Circuit::from_pest(&mut pest_output).unwrap()
}

fn span_into_str(span: Span) -> String {
    span.as_str().to_string()
}


#[cfg(test)]
mod tests {
    use crate::ast;

    #[test]
    fn test_polynomial() {
        let poly = VampirParser::parse(Rule::poly, "x^3 + a*x + b - y*y").unwrap();
        println!("{:?}", poly);
    }
}
