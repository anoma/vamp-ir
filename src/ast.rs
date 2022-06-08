use std::{collections::HashMap, ops::Index};

#[derive(Debug, Clone, PartialEq)]
pub struct Constant(pub i64);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Wire {
    Named(String),
    Constant(i64),
    Index(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WireList(pub Vec<Wire>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Gate {
    pub name: String,
    pub inputs: Vec<Node>,
    pub outputs: Vec<Wire>,
    pub wires: WireList,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Op {
    Add(Vec<Node>),
    Sub(Vec<Node>),
    Mul(Vec<Node>),
    Pow(Vec<Node>),
    Eq(Vec<Node>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Node {
    Op(Op),
    Wire(Wire),
    Invocation(Invocation),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Circuit {
    pub inputs: Vec<Wire>,
    pub outputs: Vec<Wire>,
    pub wires: WireList,
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Invocation {
    pub name: String,
    pub inputs: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct Definitions(HashMap<String, Circuit>);

#[derive(Debug, Clone)]
pub struct Vampir {
    pub definitions: Definitions,
    pub circuit: Circuit,
}

impl Op {
    pub fn inputs(&self) -> Vec<Node> {
        match self {
            Op::Add(inp) => inp.to_vec(),
            Op::Mul(inp) => inp.to_vec(),
            Op::Sub(inp) => inp.to_vec(),
            Op::Pow(inp) => inp.to_vec(),
            Op::Eq(inp) => inp.to_vec(),
        }
    }

    pub fn same(&self, nodes: Vec<Node>) -> Op {
        match self {
            Op::Add(_) => Op::Add(nodes),
            Op::Mul(_) => Op::Mul(nodes),
            Op::Sub(_) => Op::Sub(nodes),
            Op::Pow(_) => Op::Pow(nodes),
            Op::Eq(_) => Op::Eq(nodes),
        }
    }
}

impl WireList {
    pub fn new() -> Self {
        WireList(vec![])
    }
    pub fn iter(&self) -> std::slice::Iter<Wire> {
        self.0.iter()
    }

    pub fn push(&mut self, wire: Wire) {
        self.0.push(wire);
    }

    pub fn concat(&mut self, another: &Self) {
        self.0.extend(another.0);
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<Vec<Node>> for WireList {
    fn from(nodes: Vec<Node>) -> Self {
        Self(nodes.iter().flat_map(|node| node.inputs()).collect())
    }
}

impl Default for WireList {
    fn default() -> Self {
        Self::new()
    }
}

impl Index<usize> for WireList {
    type Output = Wire;
    fn index(&self, idx: usize) -> &Self::Output {
        &self.0[idx]
    }
}

impl Iterator for WireList {
    type Item = Wire;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.get(0).cloned()
    }
}

impl Definitions {
    pub fn new() -> Self {
        Self(HashMap::<String, Circuit>::new())
    }

    pub fn insert(&mut self, name: String, circuit: Circuit) -> Option<Circuit> {
        self.0.insert(name, circuit)
    }

    pub fn get(&self, name: &str) -> Option<&Circuit> {
        self.0.get(name)
    }
}

impl Default for Definitions {
    fn default() -> Self {
        Self::new()
    }
}

impl Node {
    pub fn inputs(&self) -> Vec<Wire> {
        match self {
            Node::Op(op) => op.inputs().iter().flat_map(|node| node.inputs()).collect(),
            Node::Invocation(inv) => inv.inputs.iter().flat_map(|node| node.inputs()).collect(),
            Node::Wire(wire) => vec![wire.clone()],
        }
    }
}

impl IntoIterator for Node {
    type Item = Node;
    type IntoIter =
        std::iter::Chain<std::vec::IntoIter<Self::Item>, std::vec::IntoIter<Self::Item>>;
    fn into_iter(self) -> Self::IntoIter {
        match &self {
            Node::Op(op) => vec![self.clone()].into_iter().chain(
                op.inputs()
                    .iter()
                    .flat_map(|node| node.clone().into_iter())
                    .collect::<Vec<Node>>()
                    .into_iter(),
            ),
            _ => vec![self].into_iter().chain(vec![]),
        }
    }
}
