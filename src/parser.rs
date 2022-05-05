use pest::{
    iterators::{Pair, Pairs},
    prec_climber::{Assoc, Operator, PrecClimber},
    Parser,
};

use crate::ast::*;

#[derive(Parser)]
#[grammar = "vampir.pest"]
pub struct VampirParser;

lazy_static! {
    static ref CLIMBER: PrecClimber<Rule> = PrecClimber::new(vec![
        Operator::new(Rule::plus, Assoc::Left) | Operator::new(Rule::minus, Assoc::Left),
        Operator::new(Rule::times, Assoc::Left),
    ]);
}

impl From<Pair<'_, Rule>> for Identifier {
    fn from(pair: Pair<'_, Rule>) -> Self {
        Identifier::from(pair.as_str())
    }
}

impl From<Pair<'_, Rule>> for Node {
    fn from(pair: Pair<Rule>) -> Node {
        let rule = &pair.as_rule();
        let name = pair.as_str();
        let mut inner = pair.into_inner();
        match rule {
            Rule::constraint => Node::from(inner.next().unwrap()),
            Rule::expression => CLIMBER.climb(inner, primary, infix),
            Rule::equation => Node::Gate(Gate {
                name: Identifier::from("sub"),
                inputs: vec![Box::new(Node::from(inner.next().unwrap()))],
                outputs: Some(vec![Box::new(Node::from(inner.next().unwrap()))]),
                constant: None,
            }),
            Rule::wire => Node::Wire(Wire::from(name)),
            _ => unreachable!(),
        }
    }
}

impl From<Pair<'_, Rule>> for Wire {
    fn from(pair: Pair<'_, Rule>) -> Self {
        Self::from(pair.as_str())
    }
}

impl From<Pair<'_, Rule>> for Definition {
    fn from(pair: Pair<Rule>) -> Definition {
        let mut inner = pair.into_inner();
        Definition {
            name: Identifier::from(inner.next().unwrap()),
            inputs: inner
                .next()
                .unwrap()
                .into_inner()
                .map(|pair| Node::Wire(Wire::from(pair)))
                .collect::<Vec<_>>(),
            outputs: match inner.peek().unwrap().as_rule() {
                Rule::wire_list => Some(
                    inner
                        .next()
                        .unwrap()
                        .into_inner()
                        .map(|pair| Node::Wire(Wire::from(pair)))
                        .collect::<Vec<_>>(),
                ),
                Rule::circuit => None,
                _ => unreachable!(),
            },
            circuit: Circuit::from(inner),
        }
    }
}

impl From<Pairs<'_, Rule>> for Circuit {
    fn from(mut pairs: Pairs<'_, Rule>) -> Self {
        Circuit(
            pairs
                .next()
                .unwrap()
                .into_inner()
                .map(Node::from)
                .collect::<Vec<Node>>(),
        )
    }
}

impl From<&str> for Circuit {
    fn from(input: &str) -> Self {
        Circuit::from(VampirParser::parse(Rule::circuit, input).unwrap())
    }
}

impl From<&str> for Preamble {
    fn from(input: &str) -> Self {
        Preamble::from(VampirParser::parse(Rule::preamble, input).unwrap())
    }
}

impl From<Pairs<'_, Rule>> for Preamble {
    fn from(mut pairs: Pairs<'_, Rule>) -> Self {
        Preamble(
            pairs
                .next()
                .unwrap()
                .into_inner()
                .map(Definition::from)
                .collect::<Vec<Definition>>(),
        )
    }
}

// folds two primaries according to operator precedence
fn infix(lhs: Node, op: Pair<Rule>, rhs: Node) -> Node {
    match op.as_rule() {
        Rule::plus => Node::Gate(Gate {
            name: Identifier("add".to_string()),
            inputs: vec![Box::new(lhs)],
            outputs: Some(vec![Box::new(rhs)]),
            constant: None,
        }),
        Rule::minus => Node::Gate(Gate {
            name: Identifier("sub".to_string()),
            inputs: vec![Box::new(lhs)],
            outputs: Some(vec![Box::new(rhs)]),
            constant: None,
        }),
        Rule::times => Node::Gate(Gate {
            name: Identifier("mul".to_string()),
            inputs: vec![Box::new(lhs)],
            outputs: Some(vec![Box::new(rhs)]),
            constant: None,
        }),
        _ => unreachable!(),
    }
}

fn primary(pair: Pair<Rule>) -> Node {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::whole => Node::Constant(Constant(inner.as_str().to_string().parse::<u64>().unwrap())),
        Rule::wire => Node::Wire(Wire {
            name: Identifier(inner.as_str().to_string()),
        }),
        Rule::expression => CLIMBER.climb(inner.into_inner(), primary, infix),
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Circuit, Preamble};

    #[test]
    pub(crate) fn test_circuit() {
        // exponentials not handled currently
        let test_circuit = "
            x*z*w - 3 = y - w + x
            //x^3 + a*x + b - y^2
            ";
        let circuit = Circuit::from(test_circuit);
    }

    #[test]
    pub(crate) fn test_bracketing() {
        let test_circuit = "x - (w*(y - z - w)-x)*(w+z)";
        let circuit = Circuit::from(test_circuit);
    }

    #[test]
    pub(crate) fn test_definition() {
        let test_definition = "def dist x y -> z { x*x + y*y = z*z }";
        let preamble = Preamble::from(test_definition);
    }
}
