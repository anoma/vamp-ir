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
        Operator::new(Rule::equals, Assoc::Left),
        Operator::new(Rule::plus, Assoc::Left) | Operator::new(Rule::minus, Assoc::Left),
        Operator::new(Rule::times, Assoc::Left),
        Operator::new(Rule::power, Assoc::Right),
    ]);
}

impl From<Pair<'_, Rule>> for Node {
    fn from(pair: Pair<Rule>) -> Node {
        match pair.as_rule() {
            Rule::expression => CLIMBER.climb(pair.into_inner(), primary, infix),
            Rule::wire => Node::Wire(Wire::Named(String::from(pair.as_str()))),
            _ => unreachable!(),
        }
    }
}

impl From<&str> for Vampir {
    fn from(input: &str) -> Vampir {
        Vampir::from(
            VampirParser::parse(Rule::vampir, input)
                .unwrap()
                .next()
                .unwrap(),
        )
    }
}

impl From<Pair<'_, Rule>> for Circuit {
    fn from(pair: Pair<Rule>) -> Circuit {
        let inner = pair.into_inner();
        let parsed_nodes: Vec<Node> = inner.map(Node::from).collect();
        let mut wires = WireList::from(parsed_nodes.clone());
        let nodes: Vec<Node> = parsed_nodes
            .into_iter()
            .map(|node| remove_name(node, &mut wires))
            .collect();
        Circuit {
            nodes,
            wires,
            // make this Signature::new()
            signature: Signature {
                inputs: vec![],
                outputs: vec![],
            },
        }
    }
}

impl From<Pair<'_, Rule>> for Vampir {
    fn from(pair: Pair<Rule>) -> Vampir {
        let inner = pair.into_inner();
        let mut definitions = Definitions::new();
        let mut parsed_nodes: Vec<Node> = vec![];
        inner.for_each(|pair| match pair.as_rule() {
            Rule::alias_definition => {
                // call the From on the Pair
                let mut inner = pair.into_inner();
                let name = inner.next().unwrap().as_str().into();
                let signature = Signature::from(inner.next().unwrap());
                let mut circuit = Circuit::from(inner.next().unwrap());
                circuit.signature = signature;
                definitions.insert(name, circuit);
            }
            Rule::expression => parsed_nodes.push(Node::from(pair)),
            Rule::EOI => (),
            _ => unreachable!(),
        });

        let mut wires = WireList::from(parsed_nodes.clone());
        let nodes: Vec<Node> = parsed_nodes
            .into_iter()
            .map(|node| remove_name(node, &mut wires))
            .collect();
        let inputs: Vec<Wire> = nodes.iter().flat_map(|node| node.inputs()).collect();
        let circuit = Circuit {
            nodes,
            wires,
            signature: Signature {
                inputs,
                outputs: vec![],
            },
        };

        Vampir {
            definitions,
            circuit,
        }
    }
}

impl From<Pair<'_, Rule>> for Signature {
    fn from(pair: Pair<Rule>) -> Signature {
        let mut inner = pair.into_inner();
        let inputs = inner
            .next()
            .unwrap()
            .into_inner()
            .map(|pair| Wire::Named(String::from(pair.as_str())))
            .collect::<Vec<_>>();
        let outputs = match inner.peek() {
            Some(pair) => pair
                .into_inner()
                .map(|pair| Wire::Named(String::from(pair.as_str())))
                .collect::<Vec<_>>(),
            None => vec![],
        };

        Signature {
            inputs: (0..inputs.len()).map(Wire::Index).collect(),
            outputs: (inputs.len()..(inputs.len() + outputs.len()))
                .map(Wire::Index)
                .collect(),
        }
    }
}

fn remove_name(node: Node, wires: &mut WireList) -> Node {
    match node {
        Node::Wire(wire) => match wires.iter().position(|w| wire == *w) {
            Some(num) => Node::Wire(Wire::Index(num)),
            None => {
                wires.push(wire);
                Node::Wire(Wire::Index(wires.len() - 1))
            }
        },
        Node::Op(op) => Node::Op(op.same(remove_names(op.inputs(), wires))),
        Node::Invocation(inv) => Node::Invocation(Invocation {
            name: inv.name,
            inputs: remove_names(inv.inputs, wires),
        }),
    }
}
fn remove_names(nodes: Vec<Node>, wires: &mut WireList) -> Vec<Node> {
    nodes
        .into_iter()
        .map(|node| remove_name(node, wires))
        .collect()
}

impl From<&str> for Constant {
    fn from(input: &str) -> Self {
        Self(String::from(input).parse::<i64>().unwrap())
    }
}

impl From<Pair<'_, Rule>> for Constant {
    fn from(pair: Pair<'_, Rule>) -> Self {
        Self::from(pair.as_str())
    }
}

pub fn from_pairs(mut pairs: Pairs<Rule>) -> Vec<Node> {
    pairs
        .next()
        .unwrap()
        .into_inner()
        .map(Node::from)
        .collect::<Vec<Node>>()
}

// folds two primaries according to operator precedence
fn infix(lhs: Node, op: Pair<Rule>, rhs: Node) -> Node {
    match op.as_rule() {
        Rule::plus => Node::Op(Op::Add(Box::new(lhs), Box::new(rhs))),
        Rule::minus => Node::Op(Op::Sub(Box::new(lhs), Box::new(rhs))),
        Rule::times => Node::Op(Op::Mul(Box::new(lhs), Box::new(rhs))),
        Rule::power => Node::Op(Op::Pow(Box::new(lhs), Box::new(rhs))),
        Rule::equals => Node::Op(Op::Eq(Box::new(lhs), Box::new(rhs))),
        _ => unreachable!(),
    }
}

fn primary(pair: Pair<Rule>) -> Node {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::wire => Node::Wire(Wire::Named(String::from(inner.as_str()))),
        Rule::constant => Node::Wire(Wire::Constant(
            inner.as_str().to_string().parse::<i64>().unwrap(),
        )),
        Rule::expression => CLIMBER.climb(inner.into_inner(), primary, infix),
        Rule::alias_invocation => {
            let mut inner = inner.into_inner();
            Node::Invocation(Invocation {
                name: inner.next().unwrap().as_str().into(),
                inputs: inner.next().unwrap().into_inner().map(primary).collect(),
            })
        }
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Vampir;

    #[test]
    pub(crate) fn test_expressions() {
        let test_expressions = "
            x - 3 y - 5
            x*z*w - 3 = y - w + x
            x^3--10*x +7-y^2
        ";
        let expressions = Vampir::from(test_expressions);
    }

    #[test]
    pub(crate) fn test_bracketing() {
        let test_expressions = "x - (w*(y - z - w)-x)*(w+z)";
        let expressions = Vampir::from(test_expressions);
    }

    #[test]
    pub(crate) fn test_invocation_one_output() {
        let test_expressions = "x + -7*(ec_check y z)";
        let _expressions = Vampir::from(test_expressions);
    }

    #[test]
    pub(crate) fn test_definition() {
        let test_definition = "
        def dist x y -> z { 
            x*x + 
            y*y = z*z
        }
        euclidean_distance = (dist a b)
        taxicab_distance = a + b
        ";
        let _vampir = Vampir::from(test_definition);
        println!("{:?}", _vampir);
    }
}
