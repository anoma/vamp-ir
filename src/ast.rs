use std::collections::HashMap;

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
    pub fn traverse(&self, f: fn(&Self) -> Self) -> Self {
        match self {
            Node::Gate(gate) => f(&Node::Gate(Gate{
                name: gate.name.clone(),
                inputs: gate.inputs.iter().map(|n| n.traverse(f)).collect(),
                wires: gate.wires.clone(),
            })),
            _ => f(self),
        }
    }

    // changes an "eq" node into a "sub" node
    pub fn eq_to_sub(node: &Node) -> Node {
        match node {
            Node::Gate(gate) => {
                if gate.name.as_str() == "eq" {
                    Node::Gate(Gate{name: "sub".into(), inputs: gate.inputs.to_vec(), wires: gate.wires.clone()})
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
                        (_, Node::Constant(pow)) => (0..(*pow as usize - 1))
                            .fold(left.clone(), |acc, _| {
                                Node::Gate(Gate{name: "mul".into(), inputs: vec![acc, left.clone()], wires: gate.wires.clone()})
                            }),
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
