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
    pub wires: Vec<Identifier>
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::alias_statement))]
pub struct AliasStatement {
    pub name: Identifier,
    pub inputs: Vec<Identifier>,
    pub _arrow: Option<AliasArrow>,
    pub outputs: Vec<Identifier>,
    pub body: Vec<ConstraintStatement>,
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::alias_arrow))]
pub struct AliasArrow;

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
    pub parameters: Vec<Constant>,
    pub expressions: Vec<Identifier>
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::identifier))]
pub struct Identifier {
    #[pest_ast(outer(with(span_into_str)))]
    pub value: String,
}

#[derive(Debug, FromPest, PartialEq, Clone)]
#[pest_ast(rule(Rule::constant))]
pub struct Constant {
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
    fn no_expr() {
        let circuit = ast::parse_circuit_from_string("def gate a -> b { gate a }
(a b) = (b c)
");
        assert_eq!(circuit.size(), 2);
    }

    #[test]
    fn expr() {
        ast::parse_circuit_from_string("c = d * (fi a b c) + b ^ 5");
    }

    #[test]
    fn test_circuit() {
        ast::parse_circuit_from_string("pub c d f
range a 2^6
range b 2^5
c = a + b
d = a * b
f = (fixed_base_scalar_mul e)");
    }

    #[test]
    fn built_in_gate() {
        ast::parse_circuit_from_string("pubout_poly_gate[0 1 0 0 0 0] y y y y x");
    }
}
