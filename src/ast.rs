use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Constant(pub i64);

#[derive(Debug, Clone, PartialEq)]
pub struct Wire(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Gate {
    name: String,
    inputs: Vec<Node>,
    outputs: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Node {
    Gate(String, Vec<Node>, Vec<Node>),
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

#[derive(Debug, Clone)]
pub struct Vampir {
    pub definitions: HashMap<String, Definition>,
    pub inputs: Vec<Node>,
    pub nodes: Vec<Node>,
}


impl Node {
    pub fn outputs(self) -> Vec<Node> {
        match self {
            Node::Gate(_, _, outputs) => outputs,
            Node::Wire(_) => vec![self],
            Node::Constant(_) => vec![self],
            Node::Index(_) => vec![self],
        }
    }

    pub fn inputs(&self) -> Vec<Node> {
        match self {
            Node::Gate(_, inputs, _) => inputs.iter().flat_map(|node| node.inputs()).collect(),
            Node::Wire(_) => vec![self.clone()],
            Node::Constant(_) => vec![self.clone()],
            Node::Index(_) => vec![self.clone()],
        }
    }
}