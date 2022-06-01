use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Constant(pub i64);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Wire(pub String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WireList{
    pub wires: Vec<Wire>,
    pub inputs: Vec<Wire>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Gate {
    pub name: String,
    pub inputs: Vec<Node>,
    pub wire_list: WireList,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Node {
    Gate(Gate),
    Wire(Wire),
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

pub struct WireIter {
    curr: Wire,
    next: Wire,
}

impl WireList {
    pub fn new() -> Self {
        WireList{inputs: vec![], wires: vec![]}
    }
    pub fn iter(&self) -> std::iter::Chain<std::slice::Iter<Wire>, std::slice::Iter<Wire>> {
        self.inputs.iter().chain(self.wires.iter())
    }

    pub fn push(&mut self, wire: Wire) {
        self.wires.push(wire);
    }

    pub fn len(&self) -> usize {
        self.inputs.len() + self.wires.len()
    }
}

impl Iterator for WireList {
    type Item = Wire;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter().next().cloned()
    }
}

impl Node {
    pub fn outputs(self) -> Vec<Wire> {
        match self {
            Node::Gate(Gate{wire_list, ..}) => wire_list.wires,
            Node::Wire(wire) => vec![wire],
            _ => todo!(),
        }
    }

    pub fn inputs(&self) -> Vec<Node> {
        match self {
            Node::Gate(Gate{inputs,..}) => inputs.iter().flat_map(|node| node.inputs()).collect(),
            Node::Wire(_) => vec![self.clone()],
            Node::Constant(_) => vec![self.clone()],
            Node::Index(_) => vec![self.clone()],
        }
    }
}

impl IntoIterator for Node {
    type Item = Node;
    type IntoIter =
        std::iter::Chain<std::vec::IntoIter<Self::Item>, std::vec::IntoIter<Self::Item>>;
    fn into_iter(self) -> Self::IntoIter {
        match &self {
            Node::Gate(Gate{inputs, ..}) => vec![self.clone()].into_iter().chain(
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
