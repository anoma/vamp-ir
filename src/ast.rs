use pest::{
    iterators::{Pair, Pairs},
    prec_climber::{Assoc, Operator, PrecClimber},
};

use crate::Circuit;
//use crate::preamble::{Preamble, Definition};
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
    name: Identifier,
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
                        gate.outputs.as_ref().unwrap()[0]
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

pub fn build_circuit(pairs: Pairs<Rule>) -> Circuit {
    Circuit(pairs.next().unwrap().into_inner().map(build_node).collect::<Vec<Node>>())
}

pub fn build_node(pair: Pair<Rule>) -> Node {
    match pair.as_rule() {
        Rule::constraint => build_node(pair.into_inner().next().unwrap()),
        Rule::expression => CLIMBER.climb(pair.into_inner(), primary, infix),
        Rule::equation => Node::Gate(Gate {
            name: Identifier("sub".to_string()),
            inputs: vec![Box::new(build_node(
                pair.into_inner().next().unwrap(),
            ))],
            outputs: Some(vec![Box::new(build_node(
                pair.into_inner().next().unwrap(),
            ))]),
            constant: None,
        }),
        
        // Rule::definition => {
        //     VampirObject::Definition{

        //     }
        // }
    }
}

impl VampirParser {
    // pub fn parse_circuit(input: &str) -> Circuit {
    //     Circuit(
    //         VampirParser::parse(Rule::circuit, input).unwrap()
    //             .next()
    //             .unwrap()
    //             .into_inner()
    //             .map(VampirParser::build_node)
    //             .collect::<Vec<_>>()
    //     )
    // }

    // pub fn parse_preamble(input: &str) -> Preamble {
    //     Preamble(
    //         VampirParser::parse(Rule::preamble, input).unwrap()
    //             .next()
    //             .unwrap()
    //             .into_inner()
    //             .map(VampirParser::build_definition)
    //             .collect::<Vec<_>>()
    //     )
    // }

    // pub fn build_node(pair: Pair<Rule>) -> Node {
    //     let rule = pair.as_rule();
    //     let mut inner = pair.into_inner();
    //     match rule {
    //         Rule::constraint => VampirParser::build_node(inner.next().unwrap()),
    //         Rule::expression => CLIMBER.climb(inner, primary, infix),
    //         Rule::equation => {
    //             Node::Gate(
    //                 "sub".to_string(),
    //                 Box::new(VampirParser::build_node(inner.next().unwrap())),
    //                 Box::new(VampirParser::build_node(inner.next().unwrap()))
    //             )
    //         }
    //         //Rule::EOI => Node::EndOfInput(),
    //         _ => unreachable!(),
    //     }
    // }
    // pub fn build_exponential(pair: Pair<Rule>) -> Node {
    //     let mut inner = pair.into_inner();
    //     let base = inner.next().unwrap();
    //     let exp = inner.next().unwrap().as_str().to_string().parse::<usize>().unwrap();
    //     match base.as_rule() {
    //         Rule::wire => Node::Exponential(Box::new(Node::Wire(base.as_str().to_string())), exp),
    //         Rule::whole => Node::Exponential(Box::new(Node::Constant(base.as_str().to_string().parse::<u64>().unwrap())), exp),
    //         _ => unreachable!(),
    //     }
    // }

    // pub fn build_definition(pair: Pair<Rule>) -> Definition {
    //     // TODO: build a helper function that decomposes a parser subtree
    //     // I want a dictionary (?)
    //     let name_pair = &pair.into_inner().next().unwrap();
    //     let inputs_pair = &name_pair.into_inner().next().unwrap();
    //     let mut inputs_pair_inner = inputs_pair.into_inner();
    //     let outputs_pair = match inputs_pair_inner.peek().unwrap().as_rule() {Rule::wire_list => Some(inputs_pair_inner.next().unwrap()), _ => None};
    //     let constraints_pair = match outputs_pair { Some(pair) => pair.into_inner().next().unwrap(), None => inputs_pair_inner.next().unwrap()};
    //     Definition {
    //         name: name_pair.as_str().to_string(),
    //         inputs: inputs_pair.into_inner().map(|pair| Node::Wire(pair.as_str().to_string())).collect::<Vec<_>>(),
    //         outputs: match outputs_pair { Some(pair) => Some(pair.into_inner().map(|pair| Node::Wire(pair.as_str().to_string())).collect::<Vec<_>>()), None => None },
    //         constraints: constraints_pair.into_inner().map(|pair| VampirParser::build_node(pair)).collect::<Vec<_>>(),
    //     }
    // }
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
    use crate::ast::build_circuit;
    use pest::Parser;

    // #[test]
    // pub(crate) fn test_circuit() {
    //     let test_circuit = "
    //         x*z*w - 3 = y - w + x
    //         //x^3 + a*x + b - y^2
    //         ";
    //     VampirParser::parse_circuit(test_circuit);
    // }

    // #[test]
    // pub(crate) fn test_bracketing() {
    //     let test_circuit = "x - (w*(y - z - w)-x)*(w+z)";
    //     VampirParser::parse_circuit(test_circuit);
    // }

    #[test]
    pub(crate) fn test_refactor() {

        let test_circuit = "x - (w*(y - z - w)-x)*(w+z)";
        build_circuit(
            VampirParser::parse(Rule::circuit, test_circuit).unwrap()
        );
    }

    // #[test]
    // pub(crate) fn test_definition() {
    //     let test_definition = "def dist x y -> z { x*x + y*y = z*z }";
    //     VampirParser::parse_preamble(test_definition);
    // }
}
