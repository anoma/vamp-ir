use pest::{
    iterators::{Pair, Pairs},
    prec_climber::{Assoc, Operator, PrecClimber},
};

use crate::pest::Parser;

use crate::preamble::{Definition, Preamble};
use crate::Circuit;
use std::fmt;

#[derive(Parser)]
#[grammar = "vampir.pest"]
pub struct VampirParser;

pub enum VampirObject {
    Circuit(Circuit),
    Node(Node),
    Definition,
    Identifier(Identifier),
    WireList,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier(String);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<&str> for Identifier {
    fn from(item: &str) -> Self {
        Self(item.to_string())
    }
}

impl From<Pair<'_, Rule>> for Identifier {
    fn from(pair: Pair<Rule>) -> Self {
        Self(pair.as_str().into())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Gate {
    name: Identifier,
    inputs: Vec<Box<Node>>,
    outputs: Option<Vec<Box<Node>>>,
    constant: Option<Constant>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Gate(Gate),
    Wire(Wire),
    Constant(Constant),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constant(u64);

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Wire {
    pub name: Identifier,
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Gate(gate) => match gate.name.0.as_str() {
                "sub" => {
                    write!(
                        f,
                        "({} - {})",
                        gate.inputs[0],
                        gate.outputs.as_ref().unwrap()[0]
                    )
                }
                "add" => {
                    write!(
                        f,
                        "({} + {})",
                        gate.inputs[0],
                        gate.outputs.as_ref().unwrap()[0]
                    )
                }
                "mul" => {
                    write!(
                        f,
                        "{}*{}",
                        gate.inputs[0],
                        gate.outputs.as_ref().unwrap()[0]
                    )
                }
                "exp" => {
                    write!(f, "{}^{}", gate.inputs[0], gate.constant.as_ref().unwrap())
                }
                _ => match &gate.outputs {
                    Some(outputs) => write!(
                        f,
                        "{}({} -> {})",
                        gate.name,
                        gate.inputs[0],
                        outputs[0]
                    ),
                    None => write!(f, "{}({})", gate.name, gate.inputs[0]),
                },
            },
            Node::Wire(wire) => write!(f, "{}", wire.name),
            Node::Constant(constant) => write!(f, "{}", constant.0),
        }
    }
}

lazy_static! {
    static ref CLIMBER: PrecClimber<Rule> = PrecClimber::new(vec![
        Operator::new(Rule::plus, Assoc::Right) | Operator::new(Rule::minus, Assoc::Left),
        Operator::new(Rule::times, Assoc::Right),
    ]);
}

pub fn build_node(pair: Pair<Rule>) -> Node {
    match pair.as_rule() {
        Rule::constraint => build_node(pair.into_inner().next().unwrap()),
        Rule::expression => CLIMBER.climb(pair.into_inner(), primary, infix),
        Rule::equation => Node::Gate(Gate {
            name: Identifier("sub".to_string()),
            inputs: vec![Box::new(build_node(
                pair.clone().into_inner().next().unwrap(),
            ))],
            outputs: Some(vec![Box::new(build_node(
                pair.into_inner().next().unwrap(),
            ))]),
            constant: None,
        }),
        _ => unreachable!(),
    }
}

pub fn build_definition(pair: Pair<Rule>) -> Definition {
    let mut inner = pair.into_inner();
    Definition {
        name: Identifier(inner.next().unwrap().as_str().to_string()),
        inputs: inner
            .next()
            .unwrap()
            .into_inner()
            .map(|pair| Node::Wire( Wire {
                name: Identifier(pair.as_str().to_string()),
            }))
            .collect::<Vec<_>>(),
        outputs: match inner.peek().unwrap().as_rule() {
            Rule::wire_list => Some(
                inner
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(|pair| Node::Wire( Wire {
                        name: Identifier(pair.as_str().to_string()),
                    }))
                    .collect::<Vec<_>>(),
            ),
            Rule::circuit => None,
            _ => unreachable!(),
        },
        circuit: VampirParser::build_circuit(inner),
    }
}

impl VampirParser {
    pub fn build_circuit(mut pairs: Pairs<Rule>) -> Circuit {
        Circuit(
            pairs
                .next()
                .unwrap()
                .into_inner()
                .map(build_node)
                .collect::<Vec<Node>>(),
        )
    }

    pub fn build_preamble(mut pairs: Pairs<Rule>) -> Preamble {
        Preamble(
            pairs
                .next()
                .unwrap()
                .into_inner()
                .map(build_definition)
                .collect::<Vec<Definition>>(),
        )
    }

    pub fn parse_circuit(input: &str) -> Circuit {
        VampirParser::build_circuit(VampirParser::parse(Rule::circuit, input).unwrap())
    }

    pub fn parse_preamble(input: &str) -> Preamble {
        VampirParser::build_preamble(VampirParser::parse(Rule::preamble, input).unwrap())
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
        //Rule::exponential => Node::Gate(Gate{name: inner.as_str(), inputs:  }),
        Rule::expression => CLIMBER.climb(inner.into_inner(), primary, infix),
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::VampirParser;

    #[test]
    pub(crate) fn test_circuit() {
        let test_circuit = "
            x*z*w - 3 = y - w + x
            //x^3 + a*x + b - y^2
            ";
        VampirParser::parse_circuit(test_circuit);
    }

    #[test]
    pub(crate) fn test_bracketing() {
        let test_circuit = "x - (w*(y - z - w)-x)*(w+z)";
        VampirParser::parse_circuit(test_circuit);
    }

    #[test]
    pub(crate) fn test_refactor() {
        let test_circuit = "x - (w*(y - z - w)-x)*(w+z)";
        VampirParser::parse_circuit(test_circuit);
    }

    #[test]
    pub(crate) fn test_definition() {
        let test_definition = "def dist x y -> z { x*x + y*y = z*z }";
        let preamble = VampirParser::parse_preamble(test_definition);
        println!("{}", preamble);
    }
}
