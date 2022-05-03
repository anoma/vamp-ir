use pest::{
    Parser,
    iterators::{Pair, Pairs},
    prec_climber::{Assoc, Operator, PrecClimber}
};

use pest_derive::Parser;

use crate::circuit::{Circuit, Node, Node::Wire, Node::Constant};

#[derive(Parser)]
#[grammar = "vampir.pest"]
pub struct VampirParser;


lazy_static! {
    static ref CLIMBER: PrecClimber<Rule> = PrecClimber::new(
        vec![
            Operator::new(Rule::plus, Assoc::Right) | Operator::new(Rule::minus, Assoc::Left),
            Operator::new(Rule::times, Assoc::Right),
        ]
    );
}

// folds two primaries according to operator precedence
fn infix(lhs: Node, op: Pair<Rule>, rhs: Node) -> Node {
    match op.as_rule() {
        Rule::plus => Node::Gate("add".to_string(), Box::new(lhs), Box::new(rhs)),
        Rule::minus => Node::Gate("sub".to_string(), Box::new(lhs), Box::new(rhs)),
        Rule::times => Node::Gate("mul".to_string(), Box::new(lhs), Box::new(rhs)),
        _ => unreachable!(),
    }
}

fn primary(pair: Pair<Rule>) -> Node {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::whole => Node::Constant(inner.as_str().to_string().parse::<u64>().unwrap()),
        Rule::wire => Node::Wire(inner.as_str().to_string()),
        Rule::exponential => build_exponential(inner),
        Rule::expression => CLIMBER.climb(inner.into_inner(), primary, infix),
        _ => unreachable!(),
    }
}

fn build_exponential(pair: Pair<Rule>) -> Node {
    let mut inner = pair.into_inner();
    let base = inner.next().unwrap();
    let exp = inner.next().unwrap().as_str().to_string().parse::<usize>().unwrap();
    match base.as_rule() {
        Rule::wire => Node::Exponential(Box::new(Wire(base.as_str().to_string())), exp),
        Rule::whole => Node::Exponential(Box::new(Constant(base.as_str().to_string().parse::<u64>().unwrap())), exp),
        _ => unreachable!(),
    }
}

fn build_node(pair: Pair<Rule>) -> Node {
    let rule = pair.as_rule();
    let mut inner = pair.into_inner();
    match rule {
        Rule::constraint => build_node(inner.next().unwrap()),
        Rule::expression => CLIMBER.climb(inner, primary, infix),
        Rule::equation => {
            Node::Gate(
                "sub".to_string(),
                Box::new(build_node(inner.next().unwrap())),
                Box::new(build_node(inner.next().unwrap()))
            )
        }
        Rule::EOI => Node::EndOfInput(),
        _ => unreachable!(),
    }
}

fn build_circuit(mut pairs: Pairs<Rule>) -> Circuit {
    Circuit(
        pairs
            .next()
            .unwrap()
            .into_inner()
            .map(|pair| build_node(pair))
            .collect::<Vec<_>>()
    )
}

#[cfg(test)]
mod tests {
    use crate::ast::*;

    #[test]
    pub(crate) fn test_circuit() {
        let test_circuit = "
            x*z*w - 3 = y - w + x
            x^3 + a*x + b - y^2
            ";
        build_circuit(VampirParser::parse(Rule::circuit, test_circuit).unwrap());
    }

    #[test]
    pub(crate) fn test_bracketing() {
        let test_circuit = "x - (w*(y - z - w)-x)*(w+z)";
        build_circuit(VampirParser::parse(Rule::circuit, test_circuit).unwrap());
    }
}
