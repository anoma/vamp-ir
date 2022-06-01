use std::{collections::HashMap, ops::Index};

#[derive(Debug, Clone, PartialEq)]
pub struct Constant(pub i64);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Wire {
    Wire(String),
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
pub enum Node {
    Gate(Gate),
    Wire(Wire),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Definition {
    pub inputs: Vec<Node>,
    pub outputs: Vec<Wire>,
    pub wires: WireList,
    pub nodes: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct Definitions(HashMap<String, Definition>);


#[derive(Debug, Clone)]
pub struct Vampir {
    pub definitions: Definitions,
    pub inputs: Vec<Node>,
    pub nodes: Vec<Node>,
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

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
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
        Self(HashMap::<String, Definition>::new())
    }

    pub fn insert(&mut self, name: String, def: Definition) -> Option<Definition> {
        self.0.insert(name, def)
    }
}

impl Default for Definitions {
    fn default() -> Self {
        Self::new()
    }
}

impl Node {
    pub fn inputs(&self) -> Vec<Node> {
        match self {
            Node::Gate(Gate { inputs, .. }) => {
                inputs.iter().flat_map(|node| node.inputs()).collect()
            }
            Node::Wire(_) => vec![self.clone()],
        }
    }
}

impl IntoIterator for Node {
    type Item = Node;
    type IntoIter =
        std::iter::Chain<std::vec::IntoIter<Self::Item>, std::vec::IntoIter<Self::Item>>;
    fn into_iter(self) -> Self::IntoIter {
        match &self {
            Node::Gate(Gate { inputs, .. }) => vec![self.clone()].into_iter().chain(
                inputs
                    .iter()
                    .flat_map(|node| node.clone().into_iter())
                    .collect::<Vec<Node>>()
                    .into_iter(),
            ),
            Node::Wire(_) => vec![self].into_iter().chain(vec![]),
        }
    }
}
