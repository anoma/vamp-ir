use pest::{Parser, Span};
use from_pest::FromPest;

#[derive(Parser)]
#[grammar = "plonk_ir.pest"]
struct PlonkIRParser;

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::circuit))]
pub struct Circuit {
    pub statements: Vec<Statement>,
    pub eoi: EOI
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::statement))]
pub enum Statement {
    PubStatement(PubStatement),
    AliasStatement(AliasStatement),
    ConstraintStatement(ConstraintStatement)
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::pub_statement))]
pub struct PubStatement {
    pub wires: Vec<Wire>
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::alias_statement))]
pub struct AliasStatement {
    pub name: Identifier,
    pub inputs: WireIDList,
    pub outputs: WireIDList,
    pub body: Vec<ConstraintStatement>,
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::constraint_statement))]
pub enum ConstraintStatement {
    EqualConstraint(Expression, Expression),
    GateExpression(GateExpression)
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::expression))]
pub enum Expression {
    GateExpression(GateExpression),
    PolyExpression(PolyExpression)
}

//TODO
#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::poly))]
pub struct PolyExpression {
    // signed_terms: Vec<SignedTerm>
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::gate_expression))]
pub struct GateExpression {
    pub name: Identifier,
    pub expressions: Vec<Identifier>
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::wire_id_list))]
pub struct WireIDList {
    wires: Vec<Identifier>,
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::wire_id))]
pub struct Wire {
    #[pest_ast(outer(with(span_into_str)))]
    name: String,
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::identifier))]
pub struct Identifier {
    #[pest_ast(outer(with(span_into_str)))]
    pub value: String,
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::identifier))]
pub struct Constants {
    #[pest_ast(outer(with(span_into_str)))]
    pub value: String,
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::EOI))]
pub struct EOI;

fn span_into_str(span: Span) -> String {
    span.as_str().to_string()
}

impl Circuit {
    pub fn size(&self) -> usize {
        self.statements.len()
    }
}

pub fn parse_circuit_from_string(input: &str) -> Circuit {
    let mut pest_output = PlonkIRParser::parse(Rule::circuit, input).unwrap();
    Circuit::from_pest(&mut pest_output).unwrap()
}

#[cfg(test)]
mod tests {
    use crate::ast;

    #[test]
    fn test_circuit_no_expr() {
        ast::parse_circuit_from_string("def gate a -> b { gate a }
(a b) = (b c)
");
    }

    #[test]
    fn test_circuit_one() {
        ast::parse_circuit_from_string("pub c d f
range a 2^6
range b 2^5
c = a + b
d = a * b
f = (fixed_base_scalar_mul e)");
    }

    #[test]
    fn test_circuit_two() {
        ast::parse_circuit_from_string("c = d * (fi a b c) + b ^ 5");
    }
}
