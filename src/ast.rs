use core::fmt;
use std::{collections::HashMap};

#[derive(Debug, Clone, PartialEq)]
pub struct Constant(pub i64);

#[derive(Debug, Clone, PartialEq)]
pub struct Wire(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Gate {
    pub name: String,
    pub inputs: Vec<Node>,
    pub wires: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Node {
    Gate(Gate),
    Wire(String),
    Constant(i64),
    Index(usize),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Definition {
    pub inputs: Vec<Wire>,
    pub outputs: Vec<Wire>,
    pub wires: Vec<Wire>,
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Vampir {
    pub definitions: HashMap<String, Definition>,
    pub inputs: Vec<Node>,
    pub nodes: Vec<Node>,
}

impl Node {
    pub fn inputs(&self) -> Vec<Node> {
        match self {
            Node::Gate(gate) => gate.inputs.iter().flat_map(|node| node.inputs()).collect(),
            Node::Wire(_) => vec![self.clone()],
            Node::Constant(_) => vec![self.clone()],
            Node::Index(_) => vec![self.clone()],
        }
    }

    // traverses a tree and applies `f` to each Node, children first
    pub fn traverse(&self, f: &impl Fn(&Self) -> Node) -> Node {
        match self {
            Node::Gate(gate) => f(&Node::Gate(Gate {
                name: gate.name.clone(),
                inputs: gate
                    .inputs
                    .clone()
                    .into_iter()
                    .map(|n| n.traverse(f))
                    .collect(),
                wires: gate.wires.clone(),
            })),
            _ => f(&self.clone()),
        }
    }

    // traverses a tree and applies `f` to each Node, children first
    pub fn flat_traverse(&self, f: &impl Fn(&Self) -> Vec<Node>) -> Vec<Node> {
        match self {
            Node::Gate(gate) => vec![Node::Gate(Gate {
                name: gate.name.clone(),
                inputs: gate
                    .inputs
                    .clone()
                    .into_iter()
                    .flat_map(|n| n.flat_traverse(f))
                    .collect(),
                wires: gate.wires.clone(),
            })],
            _ => f(&self.clone()),
        }
    }

    // gives a list of the children of a node that aren't leaves of the expression tree
    pub fn child_roots(&self) -> Vec<Node> {
        match self {
            Node::Gate(gate) => gate
                .inputs
                .clone()
                .into_iter()
                .filter(|node| matches!(node, Node::Gate(_))).collect(),
            _ => vec![],
        }
    }

    // lookup an index in the current list of wires and replace the index with the wire
    fn rename_node(self, wires: &[Node]) -> Node {
        match self {
            Node::Index(i) => wires[i].clone(),
            Node::Gate(gate) => Node::Gate(Gate {
                name: gate.name,
                inputs: gate
                    .inputs
                    .into_iter()
                    .map(|node| node.rename_node(wires))
                    .collect(),
                wires: gate.wires,
            }),
            _ => self,
        }
    }

    pub fn expand(&self, definitions: &HashMap<String, Definition>) -> Vec<Self> {
        match self {
            Node::Gate(gate) => {
                let children = self.child_roots();
                let wires = [
                    self.outputs(definitions),
                    gate.inputs
                        .iter()
                        .flat_map(|node| node.outputs(definitions))
                        .collect(),
                ]
                .concat();
                let nodes: Vec<Node> = match definitions.get(&gate.name) {
                    Some(definition) => definition
                        .nodes
                        .clone()
                        .into_iter()
                        .map(|node| node.rename_node(&wires))
                        .collect(),
                    None => vec![self.clone()],
                };
                [nodes, children].concat()
            }
            _ => vec![self.clone()],
        }
    }

    // creates the canoncial outputs for a gate
    pub fn outputs(&self, definitions: &HashMap<String, Definition>) -> Vec<Node> {
        match self {
            Node::Gate(Gate { name, inputs, .. }) => {
                let number_of_child_wires: usize =
                    inputs.iter().map(|node| node.count_child_wires()).sum();
                (0..definitions[name].clone().wires.len())
                    .map(|i| Node::Wire(format!("w_{}", i + number_of_child_wires)))
                    .collect()
            }
            _ => {
                vec![self.clone()]
            }
        }
    }

    // counts the total number of wires for all children of a node--useful for creating the names of newly allocated wires
    pub fn count_child_wires(&self) -> usize {
        match self {
            Node::Gate(Gate { inputs, .. }) => {
                inputs.iter().map(|node| node.count_child_wires()).sum()
            }
            _ => 1,
        }
    }

    // changes an "eq" node into a "sub" node
    pub fn eq_to_sub(node: &Node) -> Node {
        match node {
            Node::Gate(gate) => {
                if gate.name.as_str() == "eq" {
                    Node::Gate(Gate {
                        name: "sub".into(),
                        inputs: gate.inputs.to_vec(),
                        wires: gate.wires.clone(),
                    })
                } else {
                    node.clone()
                }
            }
            _ => node.clone(),
        }
    }

    // changes a "pow" node into a series of "mul" nodes
    pub fn expand_powers(node: &Node) -> Node {
        match node {
            Node::Gate(gate) => {
                if gate.name.as_str() == "pow" {
                    let (left, right) = (&gate.inputs[0], &gate.inputs[1]);
                    match (left, right) {
                        (Node::Constant(base), Node::Constant(pow)) => {
                            Node::Constant(base.pow(*pow as u32))
                        }
                        (_, Node::Constant(pow)) => {
                            (0..(*pow as usize - 1)).fold(left.clone(), |acc, _| {
                                Node::Gate(Gate {
                                    name: "mul".into(),
                                    inputs: vec![acc, left.clone()],
                                    wires: gate.wires.clone(),
                                })
                            })
                        }
                        (_, Node::Gate(_)) => {
                            panic!("Gate nodes not allowed as powers")
                        }
                        _ => todo!(),
                    }
                } else {
                    node.clone()
                }
            }
            _ => node.clone(),
        }
    }
}

impl IntoIterator for Node {
    type Item = Node;
    type IntoIter =
        std::iter::Chain<std::vec::IntoIter<Self::Item>, std::vec::IntoIter<Self::Item>>;
    fn into_iter(self) -> Self::IntoIter {
        match &self {
            Node::Gate(gate) => vec![self.clone()].into_iter().chain(
                gate.inputs
                    .iter()
                    .flat_map(|node| node.clone().into_iter())
                    .collect::<Vec<Node>>()
                    .into_iter(),
            ),
            Node::Wire(_) => vec![self].into_iter().chain(vec![]),
            Node::Constant(_) => vec![self].into_iter().chain(vec![]),
            Node::Index(_) => vec![self].into_iter().chain(vec![]),
        }
    }
}

impl IntoIterator for &Node {
    type Item = Node;
    type IntoIter =
        std::iter::Chain<std::vec::IntoIter<Self::Item>, std::vec::IntoIter<Self::Item>>;
    fn into_iter(self) -> Self::IntoIter {
        self.clone().into_iter()
    }
}

impl fmt::Display for Vampir {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Write strictly the first element into the supplied output
        // stream: `f`. Returns `fmt::Result` which indicates whether the
        // operation succeeded or failed. Note that `write!` uses syntax which
        // is very similar to `println!`.
        write!(
            f,
            "Vampir\n\tDefinitions:\n\t\t{}\n\tInputs:\n\t\t{}\n\tNodes:\n\t\t{}",
            self.definitions
                .iter()
                .map(|def| format!("{:?}", def))
                .collect::<Vec<String>>()
                .join("\n\t\t"),
            self.inputs
                .iter()
                .map(|inp| format!("{:?}", inp))
                .collect::<Vec<String>>()
                .join("\n\t\t"),
            self.nodes
                .iter()
                .map(|node| format!("{:?}", node))
                .collect::<Vec<String>>()
                .join("\n\t\t"),
        )
    }
}
