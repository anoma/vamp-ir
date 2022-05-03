use pest::{
    Parser,
    iterators::{Pair, Pairs},
    prec_climber::{Assoc, Operator, PrecClimber}
};

use pest_derive::Parser;

use crate::circuit::{Circuit, Node, Node::Wire, Node::Constant};

#[derive(Parser)]
#[grammar = "vampir.pest"]
struct VampirParser;

// folds two primaries according to operator precedence
fn infix(lhs: Node, op: Pair<Rule>, rhs: Node) -> Node {
    match op.as_rule() {
        Rule::plus => Node::Gate("add".to_string(), Box::new(lhs), Box::new(rhs)),
        Rule::minus => Node::Gate("sub".to_string(), Box::new(lhs), Box::new(rhs)),
        Rule::times => Node::Gate("mul".to_string(), Box::new(lhs), Box::new(rhs)),
        _ => unreachable!(),
    }
}

// returns a closure that captures PrecClimber but has the required
// signature for the `primary` function required by PrecClimber
fn prepare_primary(climber: &PrecClimber<Rule>) -> impl Fn(Pair<Rule>) -> Node + '_ {
    move | pair: Pair<Rule> | {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::whole => Node::Constant(inner.as_str().to_string().parse::<u64>().unwrap()),
            Rule::wire => Node::Wire(inner.as_str().to_string()),
            Rule::exponential => build_exponential(inner),
            Rule::expression => climber.climb(inner.into_inner(), prepare_primary(climber), infix),
            _ => unreachable!(),
        }
    }
}  

fn build_exponential(pair: Pair<Rule>) -> Node {
    let mut inner = pair.into_inner();
    let base = inner.next().unwrap();
    let exp = inner.next().unwrap().as_str().to_string().parse::<usize>().unwrap();
    match base.as_rule() {
        Rule::whole => Node::Exponential(Box::new(Wire(base.as_str().to_string())), exp),
        Rule::wire => Node::Exponential(Box::new(Constant(base.as_str().to_string().parse::<u64>().unwrap())), exp),
        _ => unreachable!(),
    }
}

fn build_node(pair: Pair<Rule>, climber: &PrecClimber<Rule>) -> Node {
    let rule = pair.as_rule();
    let mut inner = pair.into_inner();
    match rule {
        Rule::constraint => build_node(inner.next().unwrap(), climber),
        Rule::expression => build_expression(inner, climber),
        Rule::equation => {
            Node::Gate(
                "sub".to_string(),
                Box::new(build_node(inner.next().unwrap(), &climber)),
                Box::new(build_node(inner.next().unwrap(), &climber))
            )
        }
        Rule::EOI => Node::EndOfInput(),
        _ => unreachable!(),
    }
}

fn build_circuit(mut pairs: Pairs<Rule>, climber: &PrecClimber<Rule>) -> Circuit {
    Circuit(
        pairs
            .next()
            .unwrap()
            .into_inner()
            .map(|pair| build_node(pair, climber))
            .collect::<Vec<_>>()
    )
}

pub fn parse(vampir: &str) -> Circuit {
    let climber = PrecClimber::new(
        vec![
            Operator::new(Rule::plus, Assoc::Left) | Operator::new(Rule::minus, Assoc::Left),
            Operator::new(Rule::times, Assoc::Left),
        ]
    );
    build_circuit(VampirParser::parse(Rule::circuit, vampir).unwrap(), &climber)
}

#[cfg(test)]
mod tests {
    use crate::ast::parse;

    #[test]
    pub(crate) fn test_circuit() {
        let test_circuit = "x*z*w - 3 = y - w + x";
        let circuit = parse(test_circuit);
    }

    #[test]
    pub(crate) fn test_bracketing() {
        let test_circuit = "x - (w*(y - z - w)-x)*(w+z)";
        let circuit = parse(test_circuit);
    }
}
