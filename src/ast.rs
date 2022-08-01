use std::{collections::HashMap, fmt::Display, ops::Index};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constant(pub i64);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pub {
    pub name: Option<String>,
    pub index: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Priv {
    pub name: Option<String>,
    pub index: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Wire {
    Constant(Constant),
    Pub(Pub),
    Priv(Priv),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Op {
    Add(Box<Gate>, Box<Gate>),
    Sub(Box<Gate>, Box<Gate>),
    Mul(Box<Gate>, Box<Gate>),
    Pow(Box<Gate>, Box<Gate>),
    Eq(Box<Gate>, Box<Gate>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Input {
    Wire(Wire),
    Index(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Gate {
    Input(Input),
    Op(Op),
    Invocation(Invocation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GateList(pub Vec<Gate>);

#[derive(Debug, PartialEq, Clone)]
pub struct Circuit {
    pub signature: Signature,
    pub gates: GateList,
    pub equalities: Vec<(Wire, Wire)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Invocation {
    pub name: String,
    pub inputs: Vec<Gate>,
}

#[derive(Debug, Clone)]
pub struct Definitions(HashMap<String, Circuit>);

#[derive(Debug, PartialEq, Clone)]
pub struct Signature {
    pub inputs: Vec<Input>,
    pub outputs: Vec<Input>,
}

#[derive(Debug, Clone)]
pub struct Vampir {
    pub definitions: Definitions,
    pub circuit: Circuit,
}

impl From<usize> for Input {
    fn from(n: usize) -> Input {
        Input::Index(n)
    }
}

impl From<Gate> for Input {
    fn from(gate: Gate) -> Input {
        match gate {
            Gate::Input(inp) => inp,
            _ => unreachable!(),
        }
    }
}

impl Wire {
    pub fn reindex(&self, offset: usize) -> Wire {
        match self {
            Wire::Pub(inner) => Wire::Pub(Pub {
                name: inner.name.clone(),
                index: inner.index.map(|i| i + offset),
            }),
            Wire::Priv(inner) => Wire::Priv(Priv {
                name: inner.name.clone(),
                index: inner.index.map(|i| i + offset),
            }),
            Wire::Constant(_) => self.clone(),
        }
    }

    pub fn expand(&self) -> Wire {
        self.clone()
    }
}

impl From<Wire> for Input {
    fn from(wire: Wire) -> Input {
        Input::Wire(wire)
    }
}

impl Input {
    pub fn reindex(&self, offset: usize) -> Input {
        match self {
            Input::Wire(wire) => Input::Wire(wire.reindex(offset)),
            Input::Index(n) => Input::Index(n + offset), 
        }
    }
}

impl Op {
    pub fn inputs(&self) -> Vec<Gate> {
        match self {
            Op::Add(left, right) => vec![*left.clone(), *right.clone()],
            Op::Mul(left, right) => vec![*left.clone(), *right.clone()],
            Op::Sub(left, right) => vec![*left.clone(), *right.clone()],
            Op::Pow(left, right) => vec![*left.clone(), *right.clone()],
            Op::Eq(left, right) => vec![*left.clone(), *right.clone()],
        }
    }

    pub fn same(&self, gates: Vec<Gate>) -> Op {
        match self {
            Op::Add(_, _) => Op::Add(Box::new(gates[0].clone()), Box::new(gates[1].clone())),
            Op::Mul(_, _) => Op::Mul(Box::new(gates[0].clone()), Box::new(gates[1].clone())),
            Op::Sub(_, _) => Op::Sub(Box::new(gates[0].clone()), Box::new(gates[1].clone())),
            Op::Pow(_, _) => Op::Pow(Box::new(gates[0].clone()), Box::new(gates[1].clone())),
            Op::Eq(_, _) => Op::Eq(Box::new(gates[0].clone()), Box::new(gates[1].clone())),
        }
    }

    pub fn expand(&self) -> GateList {
        GateList::from(self)
    }
}

impl Invocation {
    pub fn outputs(&self, definitions: &Definitions) -> GateList {
        GateList(
            definitions
                .get(self)
                .unwrap()
                .signature
                .outputs
                .iter()
                .map(Gate::from)
                .collect(),
        )
    }

    pub fn gates(&self, definitions: &Definitions) -> GateList {
        definitions.get(self).unwrap().gates.clone()
    }

    pub fn expand(&self, definitions: &Definitions) -> GateList {
        let input_gates: GateList = self
            .clone()
            .inputs
            .into_iter()
            .flat_map(|gate| gate.expand(definitions))
            .collect();
        let internal_gates: GateList = self
            .clone()
            .gates(definitions)
            .into_iter()
            .flat_map(|gate| gate.expand(definitions))
            .collect();
        GateList::concat(input_gates, internal_gates)
    }
}

impl From<Input> for Gate {
    fn from(inp: Input) -> Gate {
        Gate::Input(inp)
    }
}

impl From<&Input> for Gate {
    fn from(inp: &Input) -> Gate {
        Gate::Input(inp.clone())
    }
}

impl IntoIterator for Gate {
    type Item = Gate;
    type IntoIter =
        std::iter::Chain<std::vec::IntoIter<Self::Item>, std::vec::IntoIter<Self::Item>>;
    fn into_iter(self) -> Self::IntoIter {
        match &self {
            Gate::Op(op) => vec![self.clone()].into_iter().chain(
                op.inputs()
                    .iter()
                    .flat_map(|gate| gate.clone().into_iter())
                    .collect::<Vec<Gate>>()
                    .into_iter(),
            ),
            _ => vec![self].into_iter().chain(vec![]),
        }
    }
}

impl Gate {
    pub fn inputs(&self) -> Vec<Input> {
        match self {
            Gate::Op(op) => op.inputs().iter().flat_map(|gate| gate.inputs()).collect(),
            Gate::Invocation(inv) => inv.inputs.iter().flat_map(|gate| gate.inputs()).collect(),
            Gate::Input(inp) => vec![inp.clone()],
        }
    }

    pub fn outputs(&self, definitions: &Definitions) -> GateList {
        match self {
            Gate::Op(_) => GateList(vec![self.clone()]),
            Gate::Input(_) => GateList(vec![self.clone()]),
            Gate::Invocation(inv) => inv.outputs(definitions),
        }
    }

    pub fn reindex(&self, offset: usize) -> Gate {
        match self {
            Gate::Input(inp) => Gate::Input(inp.reindex(offset)),
            Gate::Op(op) => Gate::Op(
                op.same(
                    op.inputs()
                        .iter()
                        .map(|gate| gate.reindex(offset))
                        .collect(),
                ),
            ),
            Gate::Invocation(inv) => Gate::Invocation(Invocation {
                name: inv.name.clone(),
                inputs: inv.inputs.iter().map(|gate| gate.reindex(offset)).collect(),
            }),
        }
    }

    pub fn expand(&self, definitions: &Definitions) -> GateList {
        match self {
            Gate::Input(wire) => GateList::from(self),
            Gate::Op(op) => op.expand(),
            Gate::Invocation(inv) => inv.expand(definitions),
        }
    }
}

impl Signature {
    pub fn reindex(&self, offset: usize) -> Signature {
        Signature {
            inputs: self
                .inputs
                .iter()
                .map(|wire| wire.reindex(offset))
                .collect(),
            outputs: self
                .outputs
                .iter()
                .map(|wire| wire.reindex(offset))
                .collect(),
        }
    }
}

impl Default for GateList {
    fn default() -> Self {
        Self::new()
    }
}

impl From<Wire> for GateList {
    fn from(wire: Wire) -> GateList {
        GateList(vec![Gate::Input(Input::Wire(wire))])
    }
}

impl From<Input> for GateList {
    fn from(inp: Input) -> GateList {
        GateList(vec![Gate::Input(inp)])
    }
}

impl From<Op> for GateList {
    fn from(op: Op) -> GateList {
        let mut gate_list: GateList = op
            .inputs()
            .into_iter()
            .flat_map(GateList::from)
            .collect();
        gate_list.push(Gate::Op(op));
        gate_list
    }
}

impl From<&Op> for GateList {
    fn from(op: &Op) -> GateList {
        GateList::from(op.clone())
    }
}

impl From<Invocation> for GateList {
    fn from(invocation: Invocation) -> GateList {
        let mut gate_list: GateList = invocation
            .clone()
            .inputs
            .into_iter()
            .flat_map(GateList::from)
            .collect();
        gate_list.push(Gate::Invocation(invocation));
        gate_list
    }
}

impl From<Gate> for GateList {
    fn from(gate: Gate) -> GateList {
        match gate {
            Gate::Op(op) => GateList::from(op),
            Gate::Input(inp) => GateList::from(inp),
            Gate::Invocation(invocation) => GateList::from(invocation),
        }
    }
}

impl From<&Gate> for GateList {
    fn from(gate: &Gate) -> GateList {
        GateList::from(gate.clone())
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

impl GateList {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
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

    pub fn remove_names(self) -> Self {
        self.iter()
            .map(|gate| match gate {
                Gate::Input(_) => gate.clone(),
                Gate::Op(op) => Gate::Op(
                    op.same(
                        op.inputs()
                            .iter()
                            .map(|gate| {
                                Gate::Input(Input::from(
                                    self.iter().position(|g| gate == g).unwrap(),
                                ))
                            })
                            .collect(),
                    ),
                ),
                Gate::Invocation(inv) => Gate::Invocation(Invocation {
                    name: inv.name.clone(),
                    inputs: inv
                        .inputs
                        .iter()
                        .map(|gate| {
                            Gate::Input(Input::from(self.iter().position(|g| gate == g).unwrap()))
                        })
                        .collect(),
                }),
            })
            .collect()
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

    pub fn iter(&self) -> std::slice::Iter<'_, Gate> {
        self.0.iter()
    }
}

impl Default for Definitions {
    fn default() -> Self {
        Self::new()
    }
}

impl Definitions {
    pub fn new() -> Self {
        Self(HashMap::<String, Circuit>::new())
    }

    pub fn insert(&mut self, name: String, circuit: Circuit) -> Option<Circuit> {
        self.0.insert(name, circuit)
    }

    pub fn get(&self, inv: &Invocation) -> Option<&Circuit> {
        self.0.get(&inv.name)
    }
}
