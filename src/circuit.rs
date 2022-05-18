use std::collections::HashMap;

use crate::ast::{Node, Vampir};

#[derive(Debug)]
pub struct Circuit {
    inputs: HashMap<Wire, usize>,
    gates: Vec<Gate>,
}

#[derive(Debug)]
pub struct Gate {
    name: String,
    inputs: Vec<Wire>,
    // TODO maybe include outputs
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Wire(pub String);

// impl From<&Node> for Gate {
//     fn from(node: &Node) -> Self {
//         match node {
//             Node::AliasInvocation(name, inputs) => 
//                 Self {
//                     name: name.into(),
//                     inputs: inputs.to_vec(),
//                 },
//             _ => unreachable!(),
//         }
//     }
// }

impl From<Vec<Node>> for Circuit {
    fn from(nodes: Vec<Node>) -> Self {
        let inputs = construct_wire_map(&nodes);
        let gates = construct_gate_list(&nodes, &inputs);
        Self {
            inputs,
            gates,
        }
    }

}

impl From<Vampir> for Circuit {
    fn from(vampir: Vampir) -> Self {
        let inputs = construct_wire_map(&vampir.expressions);
        let gates = construct_gate_list(&vampir.expressions, &inputs);
        Self {
            inputs,
            gates,
        }
    }
}

// to do here:
// - alias invocations do not need their own struct--make them nodes
// - traverse the tree and create new wires with auto generated names 
// for the outputs of every alias invocation
// - flatmap over the the tree making a list of inputs for each node
// using the newly named outputs of each alias invocation

// i.e. add (mul a b) c becomes add d c, where d = mul a b

fn construct_wire_map(nodes: &Vec<Node>) -> HashMap<Wire, usize> {
    let mut wires = HashMap::<Wire, usize>::new();
    fn record_wires(node: &Node, wires: &mut HashMap<Wire, usize>) {
        match node {
            Node::Wire(string) => {
                let length = wires.len();
                wires.entry(Wire(string.into())).or_insert(length);
            }
            Node::Constant(_) => (),
            Node::Node(name, inputs) => {
                inputs.into_iter().for_each(|node| record_wires(node, wires));
            },
        }
    }
    nodes.into_iter().for_each(|node| {
        record_wires(node, &mut wires);
    });
    wires
}

fn construct_gate_list(nodes: &Vec<Node>, wire_map: &HashMap<Wire, usize>) -> Vec<Gate> {
    let mut gates = Vec::<Gate>::new();
    fn record_gates(node: &Node, gates: &mut Vec<Gate>, wire_map: &HashMap<Wire, usize>) {
        match node {
            Node::Node(name, inputs) => {
                inputs.into_iter().for_each(|node| record_gates(node, gates, wire_map));
            },
            _ => (),
        }
    }
    nodes.into_iter().for_each(|node| {
        record_gates(node, &mut gates, &wire_map);
    });
    gates
}

#[cfg(test)]
mod tests {
    use crate::{circuit::{Circuit, construct_wire_map, construct_gate_list}, parser::{from_str, pairs}, ast::Vampir};

    #[test]
    pub(crate) fn test_record_wires() {
        let test_str = "x * (ec_check y z) + (ec_check x y)";
        let vampir = Vampir::from(test_str);
        let wires = construct_wire_map(&vampir.expressions);
    }

    #[test]
    pub(crate) fn test_record_gates() {
        let test_str = "x * (ec_check y z) + (ec_check x y)";
        let vampir = Vampir::from(test_str);
        let wires = construct_wire_map(&vampir.expressions);
        let gates = construct_gate_list(&vampir.expressions, &wires);
    }

    #[test]
    pub(crate) fn test_circuit_construction() {
        let test_expressions = "
                def ec_check x y {
                    x^3 + 3 = y^2
                }
                x * (ec_check y z) + (ec_check x y)
            ";
        let vampir = Vampir::from(test_expressions);
        println!("{:?}", vampir);
        let circuit = Circuit::from(vampir);
        println!("{:?}", circuit);
    }
}
