use std::{collections::HashMap, fmt};

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Invocation {
    pub name: String,
    pub inputs: Vec<Gate>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Signature {
    pub inputs: Vec<Input>,
    pub outputs: Vec<Input>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Circuit {
    pub signature: Signature,
    pub gates: GateList,
    pub equalities: Vec<(Wire, Wire)>,
}

#[derive(Debug, Clone)]
pub struct Definitions(HashMap<String, Circuit>);

#[derive(Debug, Clone)]
pub struct Vampir {
    pub definitions: Definitions,
    pub circuit: Circuit,
}

fn indent(string_vec: Vec<String>, n: usize) -> Vec<String> {
    let tab_string = (0..n).fold(String::new(), |s, _| format!("  {}", s));
    string_vec
        .iter()
        .map(|s| format!("\n{}{}", tab_string, s))
        .collect()
}

impl fmt::Display for Invocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({} {})",
            self.name,
            self.inputs
                .iter()
                .map(|gate| format!("{}", gate))
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let inputs_string = self
            .inputs
            .iter()
            .map(|gate| format!("{}", gate))
            .collect::<Vec<String>>()
            .join(" ");
        let outputs_string = match self.outputs.is_empty() {
            false => format!(
                " -> {}",
                self.outputs
                    .iter()
                    .map(|gate| format!("{}", gate))
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            true => String::new(),
        };
        write!(f, "{}{}", inputs_string, outputs_string)
    }
}

impl fmt::Display for Wire {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Wire::Priv(prv) => write!(f, "{}", prv.name.as_ref().unwrap()),
            Wire::Pub(pb) => write!(f, "pub {}", pb.name.as_ref().unwrap()),
            Wire::Constant(con) => write!(f, "{}", con.0),
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Op::Add(left, right) => write!(f, "{} + {}", left, right),
            Op::Sub(left, right) => write!(f, "{} - {}", left, right),
            Op::Mul(left, right) => write!(f, "{}*{}", left, right),
            Op::Pow(left, right) => write!(f, "{}^{}", left, right),
            Op::Eq(left, right) => write!(f, "{} = {}", left, right),
        }
    }
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

impl fmt::Display for Input {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Input::Wire(wire) => write!(f, "{}", wire),
            Input::Index(idx) => write!(f, "w{}", idx),
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

    pub fn index(&self, idx: usize) -> Wire {
        match self {
            Wire::Pub(inner) => Wire::Pub(Pub {
                name: inner.name.clone(),
                index: Some(idx),
            }),
            Wire::Priv(inner) => Wire::Priv(Priv {
                name: inner.name.clone(),
                index: Some(idx),
            }),
            Wire::Constant(_) => self.clone(),
        }
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

    pub fn index(&self, idx: usize) -> Input {
        match self {
            Input::Wire(wire) => Input::Wire(wire.index(idx)),
            Input::Index(_) => Input::Index(idx),
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

    pub fn index(&self, idx: usize) -> Op {
        self.same(
            self.inputs()
                .into_iter()
                .map(|gate| gate.index(idx))
                .collect(),
        )
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

impl fmt::Display for Gate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Gate::Input(inp) => write!(f, "{}", inp),
            Gate::Op(op) => write!(f, "{}", op),
            Gate::Invocation(inv) => write!(f, "{}", inv),
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

    pub fn index(&self, idx: usize) -> Gate {
        match self {
            Gate::Input(inp) => Gate::Input(inp.index(idx)),
            Gate::Op(op) => {
                Gate::Op(op.same(op.inputs().iter().map(|gate| gate.index(idx)).collect()))
            }
            Gate::Invocation(inv) => Gate::Invocation(Invocation {
                name: inv.name.clone(),
                inputs: inv.inputs.iter().map(|gate| gate.index(idx)).collect(),
            }),
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
            Gate::Input(_) => GateList::from(self),
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
        let mut gate_list: GateList = op.inputs().into_iter().flat_map(GateList::from).collect();
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

// flattens a gate tree into a GateList
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

impl fmt::Display for GateList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "gates:{}",
            indent(
                self.iter()
                    .enumerate()
                    .map(|(i, gate)| format!("{}: {}", i, gate))
                    .collect::<Vec<String>>(),
                1,
            )
            .join("")
        )
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

    pub fn flatten(&self) -> GateList {
        self.iter().flat_map(GateList::from).collect()
    }

    pub fn remove_names(&self) -> Self {
        self.iter()
            .map(|gate| match gate {
                Gate::Input(_) => gate.index(self.iter().position(|g| gate == g).unwrap()),
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

    pub fn index(&self) -> GateList {
        self.clone()
            .into_iter()
            .enumerate()
            .map(|(i, gate)| gate.index(i))
            .collect()
    }

    pub fn dedupe(&self) -> GateList {
        let mut res = GateList::new();
        self.iter()
            .for_each(|gate| match res.iter().find(|g| g == &gate) {
                Some(_) => (),
                None => res.push(gate.clone()),
            });
        res
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

impl Signature {
    pub fn dedupe(&self) -> Self {
        let mut inputs = vec![];
        self
            .inputs
            .iter()
            .for_each(|inp| match inputs.iter().find(|i| i == &inp) {
                Some(_) => (),
                None => inputs.push(inp.clone()),
            });
        Signature {
            inputs,
            outputs: self.outputs.clone(),
        }
    }
}

impl fmt::Display for Circuit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "  signature:\n    {}\n\n  gates: {{{}\n  }}\n\n  equalities:{}",
            self.signature,
            indent(
                self.gates
                    .iter()
                    .enumerate()
                    .map(|(i, gate)| format!("w{}: {}", i, gate))
                    .collect(),
                2
            )
            .join(""),
            indent(
                self.equalities
                    .iter()
                    .map(|(l, r)| format!("{} = {}", l, r))
                    .collect::<Vec<String>>(),
                2
            )
            .join(""),
        )
    }
}

impl Circuit {
    pub fn reindex(&self, offset: usize) -> Circuit {
        let signature = self.signature.reindex(offset);
        let gates = self.gates.iter().map(|gate| gate.reindex(offset)).collect();
        let equalities = self
            .equalities
            .iter()
            .map(|(left, right)| (left.reindex(offset), right.reindex(offset)))
            .collect();
        Circuit {
            signature,
            gates,
            equalities,
        }
    }

    pub fn expand(&self, definitions: &Definitions) -> Circuit {
        let signature = self.signature.clone();
        let equalities = self.equalities.clone();

        let gates = self
            .gates
            .clone()
            .into_iter()
            .map(|gate| gate.expand(definitions))
            .fold(GateList::new(), |state, gate_list| {
                GateList::concat(state, gate_list)
            });

        Circuit {
            signature,
            gates,
            equalities,
        }
    }

    pub fn dedupe(&self) -> Circuit {
        let signature = self.signature.dedupe();
        let equalities = self.equalities.clone();
        let gates = self.gates.dedupe();

        Circuit {
            signature,
            gates,
            equalities,
        }
    }

    pub fn remove_names(&self) -> Circuit {
        let signature = self.signature.clone();
        let equalities = self.equalities.clone();
        let gates = self.gates.remove_names();

        Circuit {
            signature,
            gates,
            equalities,
        }
    }

    pub fn to_expanded_anf(&self, definitions: &Definitions) -> Circuit {
        self.dedupe().expand(definitions).dedupe().remove_names()
    }

    pub fn to_anf(&self, definitions: &Definitions) -> Circuit {
        let gates = self.gates.flatten().dedupe().remove_names();
        Circuit {
            signature: self.signature.clone(),
            gates,
            equalities: self.equalities.clone(),
        }
    }
}

impl Default for Definitions {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for Definitions {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|(k, v)| format!(
                    "  def {} {} {{{}\n  }}\n",
                    k,
                    v.signature,
                    indent(v.gates.iter().map(|gate| format!("{}", gate)).collect(), 2).join("")
                ))
                .collect::<Vec<String>>()
                .join("\n")
        )
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

impl fmt::Display for Vampir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}\n{}", self.definitions, self.circuit)
    }
}

impl Vampir {
    pub fn expand(&self) -> Vampir {
        Vampir {
            definitions: self.definitions.clone(),
            circuit: self.circuit.expand(&self.definitions),
        }
    }

    pub fn to_anf(&self) -> Vampir {
        Vampir {
            definitions: self.definitions.clone(),
            circuit: self.circuit.to_anf(&self.definitions),
        }
    }
}
