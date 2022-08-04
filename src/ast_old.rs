use crate::circuit::{reindex_node, reindex_wire};
use std::{collections::HashMap, fmt::Display, ops::Index};

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
pub enum Op {
    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Pow(Box<Node>, Box<Node>),
    Eq(Box<Node>, Box<Node>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Node {
    Op(Op),
    Wire(Wire),
    Invocation(Invocation),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Gate {
    Input(Wire),
    //Add((usize, usize), (Constant, Constant)),
    //Mul((usize, usize), Constant),
    Op(Op),
    Invocation(Invocation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GateList(pub Vec<Gate>);

#[derive(Debug, PartialEq, Clone)]
pub struct Circuit {
    pub signature: Signature,
    pub wires: WireList,
    pub nodes: Vec<Node>,
    pub equalities: Vec<(Wire, Wire)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Invocation {
    pub name: String,
    pub inputs: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct Definitions(HashMap<String, Circuit>);

#[derive(Debug, PartialEq, Clone)]
pub struct Signature {
    pub inputs: Vec<Wire>,
    pub outputs: Vec<Wire>,
}

#[derive(Debug, Clone)]
pub struct Vampir {
    pub definitions: Definitions,
    pub circuit: Circuit,
}

impl Op {
    pub fn inputs(&self) -> Vec<Node> {
        match self {
            Op::Add(left, right) => vec![*left.clone(), *right.clone()],
            Op::Mul(left, right) => vec![*left.clone(), *right.clone()],
            Op::Sub(left, right) => vec![*left.clone(), *right.clone()],
            Op::Pow(left, right) => vec![*left.clone(), *right.clone()],
            Op::Eq(left, right) => vec![*left.clone(), *right.clone()],
        }
    }

    pub fn same(&self, nodes: Vec<Node>) -> Op {
        match self {
            Op::Add(_, _) => Op::Add(Box::new(nodes[0].clone()), Box::new(nodes[1].clone())),
            Op::Mul(_, _) => Op::Mul(Box::new(nodes[0].clone()), Box::new(nodes[1].clone())),
            Op::Sub(_, _) => Op::Sub(Box::new(nodes[0].clone()), Box::new(nodes[1].clone())),
            Op::Pow(_, _) => Op::Pow(Box::new(nodes[0].clone()), Box::new(nodes[1].clone())),
            Op::Eq(_, _) => Op::Eq(Box::new(nodes[0].clone()), Box::new(nodes[1].clone())),
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

    pub fn insert(&mut self, wire: &Wire) {
        if !self.0.contains(wire) {
            self.push(wire.clone());
        }
    }

    pub fn concat(&mut self, another: &Self) {
        self.0.extend(another.0.iter().cloned());
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn extend(&mut self, other: WireList) {
        self.0.extend(other.0)
    }
}

impl From<Vec<Node>> for WireList {
    fn from(nodes: Vec<Node>) -> Self {
        let mut res = Self::new();

        nodes
            .iter()
            .for_each(|node| node.inputs().iter().for_each(|wire| res.push(wire.clone())));
        res
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

impl From<Wire> for Node {
    fn from(wire: Wire) -> Node {
        Node::Wire(wire)
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

impl Gate {
    pub fn reindex(self, offset: usize) -> Gate {
        match self {
            Gate::Input(wire) => Gate::Input(reindex_wire(&wire, offset)),
            // Gate::Add((l, r),(c1,c2)) => Gate::Add((l+offset, r+offset),(c1,c2)),
            // Gate::Mul((l, r), c) => Gate::Mul((l+offset, r+offset),c),
            Gate::Op(op) => Gate::Op(
                op.same(
                    op.inputs()
                        .iter()
                        .map(|node| reindex_node(&node, offset))
                        .collect(),
                ),
            ),
            Gate::Invocation(inv) => Gate::Invocation(Invocation {
                name: inv.name,
                inputs: inv
                    .inputs
                    .iter()
                    .map(|node| reindex_node(&node, offset))
                    .collect(),
            }),
        }
    }
}

impl GateList {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn new() -> Self {
        GateList(vec![])
    }
    pub fn extend(&mut self, other: GateList) {
        self.0.extend(other.0);
    }
    pub fn push(&mut self, gate: Gate) {
        self.0.push(gate)
    }
    pub fn pprint(&self) {
        self.0.iter().for_each(|gate| println!("{:?}", gate));
    }
    pub fn reindex(&self, offset: usize) -> GateList {
        GateList(
            self.0
                .clone()
                .into_iter()
                .map(|gate| gate.reindex(offset))
                .collect(),
        )
    }
    pub fn concat(self, other: GateList) -> GateList {
        GateList([self.0.clone(), other.reindex(self.len()).0].concat())
    }
}

impl From<Node> for GateList {
    fn from(node: Node) -> GateList {
        match node {
            Node::Op(op) => {
                let gate_lists: Vec<GateList> = op
                    .clone()
                    .inputs()
                    .into_iter()
                    .map(GateList::from)
                    .collect();
                let new_inputs: Vec<Node> = gate_lists
                    .iter()
                    .map(|gate_list| Node::Wire(Wire::Index(gate_list.len())))
                    .collect();
                let new_op = Gate::Op(op.same(new_inputs));
                let mut gate_list: GateList = gate_lists
                    .into_iter()
                    .fold(GateList::new(), |state, gate_list| {
                        state.concat(gate_list)
                    });
                gate_list.push(new_op);
                gate_list
            }
            Node::Wire(wire) => GateList(vec![Gate::Input(wire)]),
            Node::Invocation(ref inv) => {
                let gate_list: GateList =
                    inv.clone().inputs.into_iter().flat_map(GateList::from).collect();
                //println!("gate lists {:?}", gate_lists);
                // let new_inputs: Vec<Node> = gate_lists
                //     .iter()
                //     .scan(0usize, |count, gate_list| {
                //         *count += gate_list.len();
                //         Some(Node::Wire(Wire::Index(*count - 1)))
                //     })
                //     .collect();
                let new_inputs: Vec<Node> = inv.inputs.clone().iter().map(|input| Node::Wire(Wire::Index(gate_list.into_iter().position(|gate| gate == input)))).collect();
                let new_invocation = Gate::Invocation(Invocation {
                    name: inv.name.clone(),
                    inputs: inv.clone().inputs,//new_inputs,
                });
                //let mut gate_list: GateList = gate_lists.into_iter().flatten().collect();
                gate_list.push(new_invocation);
                gate_list
            }
        }
    }
}

impl FromIterator<Gate> for GateList {
    fn from_iter<I: IntoIterator<Item = Gate>>(iter: I) -> Self {
        let mut v: Vec<Gate> = vec![];
        for i in iter {
            v.push(i);
        }
        GateList(v)
    }
}

impl IntoIterator for GateList {
    type Item = Gate;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
