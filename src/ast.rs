use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Constant(pub i64);

#[derive(Debug, Clone, PartialEq)]
pub struct Wire(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Gate {
    name: String,
    inputs: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Node {
    Gate(String, Vec<Node>),
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
    pub fn outputs(self) -> Vec<Node> {
        match self {
            Node::Gate(_, _) => todo!(),
            Node::Wire(_) => vec![self],
            Node::Constant(_) => vec![self],
            Node::Index(_) => vec![self],
        }
    }

    pub fn inputs(&self) -> Vec<Node> {
        match self {
            Node::Gate(_, inputs) => inputs.iter().flat_map(|node| node.inputs()).collect(),
            Node::Wire(_) => vec![self.clone()],
            Node::Constant(_) => vec![self.clone()],
            Node::Index(_) => vec![self.clone()],
        }
    }

    // traverses a tree and applies `f` to each Node
    pub fn traverse(&self, f: fn(&Self) -> Self) -> Self {
        match self {
            Node::Gate(name, inputs) => f(&Node::Gate(
                name.clone(),
                inputs.iter().map(|n| n.traverse(f)).collect(),
            )),
            _ => f(self),
        }
    }

    // changes an "eq" node into a "sub" node
    pub fn eq_to_sub(node: &Node) -> Node {
        match node {
            Node::Gate(name, inputs) => {
                if name.as_str() == "eq" {
                    Node::Gate("sub".into(), inputs.to_vec())
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
            Node::Gate(name, inputs) => {
                if name.as_str() == "pow" {
                    let (left, right) = (&inputs[0], &inputs[1]);
                    match (left, right) {
                        (Node::Constant(base), Node::Constant(pow)) => {
                            Node::Constant(base.pow(*pow as u32))
                        }
                        (_, Node::Constant(pow)) => (0..(*pow as usize - 1))
                            .fold(left.clone(), |acc, _| {
                                Node::Gate("mul".into(), vec![acc, left.clone()])
                            }),
                        (_, Node::Gate(_, _)) => {
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
            Node::Gate(_, inputs) => vec![self.clone()].into_iter().chain(
                inputs
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
