use pest::{
    iterators::{Pair, Pairs},
    prec_climber::{Assoc, Operator, PrecClimber},
    Parser,
};

use crate::ast::*;

#[derive(Parser)]
#[grammar = "vampir.pest"]
pub struct VampirParser;

lazy_static! {
    static ref CLIMBER: PrecClimber<Rule> = PrecClimber::new(vec![
        Operator::new(Rule::equals, Assoc::Left),
        Operator::new(Rule::plus, Assoc::Left) | Operator::new(Rule::minus, Assoc::Left),
        Operator::new(Rule::times, Assoc::Left),
        Operator::new(Rule::power, Assoc::Right),
    ]);
}

impl From<Pair<'_, Rule>> for Wire {
    fn from(pair: Pair<Rule>) -> Wire {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::pub_wire => Wire::Pub(Pub {
                name: Some(inner.as_str().to_string()),
                index: None,
            }),
            Rule::priv_wire => Wire::Priv(Priv {
                name: Some(inner.as_str().to_string()),
                index: None,
            }),
            Rule::constant => Wire::Constant(Constant::from(inner)),
            _ => unreachable!(),
        }
    }
}

impl From<Pair<'_, Rule>> for Input {
    fn from(pair: Pair<Rule>) -> Input {
        Input::from(Wire::from(pair))
    }
}

impl From<Pair<'_, Rule>> for Gate {
    fn from(pair: Pair<Rule>) -> Gate {
        match pair.as_rule() {
            Rule::expression => CLIMBER.climb(pair.into_inner(), primary, infix),
            Rule::wire => Gate::Input(Input::Wire(Wire::from(pair))),
            _ => unreachable!(),
        }
    }
}

impl From<&str> for Vampir {
    fn from(input: &str) -> Vampir {
        Vampir::from(
            VampirParser::parse(Rule::vampir, input)
                .unwrap()
                .next()
                .unwrap(),
        )
    }
}

impl From<Pair<'_, Rule>> for GateList {
    fn from(pair: Pair<Rule>) -> GateList {
        GateList(pair.into_inner().map(Gate::from).collect::<Vec<Gate>>())
    }
}

// creates a signature-less circuit from a parser pair
impl From<Pair<'_, Rule>> for Circuit {
    fn from(pair: Pair<Rule>) -> Circuit {
        Circuit::from(GateList::from(pair))
    }
}

// creates a signature-less circuit from a list of gates
impl From<GateList> for Circuit {
    fn from(named_gates: GateList) -> Circuit {
        let gates = named_gates;
        let inputs: Vec<Input> = gates.iter().flat_map(|gate| gate.inputs()).collect();
        let signature = Signature {
            inputs,
            outputs: vec![],
        };
        let equalities = vec![];
        Circuit {
            gates,
            signature: signature.dedupe(),
            equalities,
        }
    }
}

impl From<Vec<Pair<'_, Rule>>> for Definitions {
    fn from(pairs: Vec<Pair<Rule>>) -> Definitions {
        let mut definitions = Definitions::new();
        pairs.into_iter().for_each(|pair| {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().into();
            let signature = Signature::from(inner.next().unwrap());
            let mut circuit = Circuit::from(inner.next().unwrap());
            circuit.signature = signature.dedupe();
            definitions.insert(name, circuit);
        });

        definitions
    }
}

impl From<Vec<Pair<'_, Rule>>> for Circuit {
    fn from(pairs: Vec<Pair<Rule>>) -> Circuit {
        Circuit::from(GateList(
            pairs.into_iter().map(Gate::from).collect::<Vec<Gate>>(),
        ))
    }
}

impl From<Pair<'_, Rule>> for Vampir {
    fn from(pair: Pair<Rule>) -> Vampir {
        let (def_pairs, exp_pairs): (Vec<_>, Vec<_>) = partition_pairs(pair);
        let definitions = Definitions::from(def_pairs);
        let circuit = Circuit::from(exp_pairs);

        Vampir {
            definitions,
            circuit,
        }
    }
}

impl From<Pair<'_, Rule>> for Signature {
    fn from(pair: Pair<Rule>) -> Signature {
        let mut inner = pair.into_inner();
        let inputs = inner
            .next()
            .unwrap()
            .into_inner()
            .map(Input::from)
            .collect::<Vec<_>>();
        let outputs = match inner.peek() {
            Some(pair) => pair.into_inner().map(Input::from).collect::<Vec<_>>(),
            None => vec![],
        };

        let sig = Signature { inputs, outputs };
        sig.dedupe()
    }
}

// impl Signature {
//     fn remove_names(self, wires: &mut WireList) -> Signature {
//         Self {
//             inputs: self
//                 .inputs
//                 .iter()
//                 .map(|wire| Wire::Index(wires.iter().position(|w| w == wire).unwrap()))
//                 .collect(),
//             outputs: self
//                 .inputs
//                 .iter()
//                 .map(|wire| Wire::Index(wires.iter().position(|w| w == wire).unwrap()))
//                 .collect(),
//         }
//     }
// }

// fn remove_name(gate: Gate, wires: &mut WireList) -> Gate {
//     match gate {
//         Gate::Input(wire) => {
//             let index = wires.iter().position(|w| w == &wire).unwrap_or({
//                 wires.push(wire);
//                 wires.len() - 1
//             });
//             Gate::Input(Wire::Index(index))
//         }
//         Gate::Op(op) => Gate::Op(op.same(remove_names(op.inputs(), wires))),
//         Gate::Invocation(inv) => Gate::Invocation(Invocation {
//             name: inv.name,
//             inputs: remove_names(inv.inputs, wires),
//         }),
//     }
// }

// fn remove_names(gates: Vec<Gate>, wires: &mut WireList) -> Vec<Gate> {
//     gates
//         .into_iter()
//         .map(|gate| remove_name(gate, wires))
//         .collect()
// }

// fn record_equalities(wires: &WireList) -> Vec<(Wire, Wire)> {
//     wires
//         .iter()
//         .enumerate()
//         .map(
//             |(idx, query)| match wires.iter().position(|wire| query == wire) {
//                 Some(num) => (Wire::Index(idx), Wire::Index(num)),
//                 None => (Wire::Index(idx), Wire::Index(idx)),
//             },
//         )
//         .collect()
// }

// paritions vampir input into Alias Definition pairs and Expression pairs
fn partition_pairs(pair: Pair<'_, Rule>) -> (Vec<Pair<Rule>>, Vec<Pair<Rule>>) {
    let pairs = pair.into_inner();
    let mut def_pairs = vec![];
    let mut exp_pairs = vec![];
    pairs.for_each(|pair| match pair.as_rule() {
        Rule::alias_definition => def_pairs.push(pair),
        Rule::expression => exp_pairs.push(pair),
        _ => (),
    });
    (def_pairs, exp_pairs)
}

impl From<&str> for Constant {
    fn from(input: &str) -> Self {
        Self(String::from(input).parse::<i64>().unwrap())
    }
}

impl From<Pair<'_, Rule>> for Constant {
    fn from(pair: Pair<'_, Rule>) -> Self {
        Self::from(pair.as_str())
    }
}

pub fn from_pairs(mut pairs: Pairs<Rule>) -> Vec<Gate> {
    pairs
        .next()
        .unwrap()
        .into_inner()
        .map(Gate::from)
        .collect::<Vec<Gate>>()
}

// folds two primaries according to operator precedence
fn infix(lhs: Gate, op: Pair<Rule>, rhs: Gate) -> Gate {
    match op.as_rule() {
        Rule::plus => Gate::Op(Op::Add(Box::new(lhs), Box::new(rhs))),
        Rule::minus => Gate::Op(Op::Sub(Box::new(lhs), Box::new(rhs))),
        Rule::times => Gate::Op(Op::Mul(Box::new(lhs), Box::new(rhs))),
        Rule::power => Gate::Op(Op::Pow(Box::new(lhs), Box::new(rhs))),
        Rule::equals => Gate::Op(Op::Eq(Box::new(lhs), Box::new(rhs))),
        _ => unreachable!(),
    }
}

fn primary(pair: Pair<Rule>) -> Gate {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::wire => Gate::Input(Input::from(inner)),
        Rule::constant => Gate::Input(Input::Wire(Wire::Constant(Constant(
            inner.as_str().to_string().parse::<i64>().unwrap(),
        )))),
        Rule::expression => CLIMBER.climb(inner.into_inner(), primary, infix),
        Rule::alias_invocation => {
            let mut inner = inner.into_inner();
            Gate::Invocation(Invocation {
                name: inner.next().unwrap().as_str().into(),
                inputs: inner.next().unwrap().into_inner().map(primary).collect(),
            })
        }
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Vampir;

    #[test]
    pub(crate) fn test_expressions() {
        let test_expressions = "
            x - 3 y - 5
            x*z*w - 3 = y - w + x
            x^3--10*x +7-y^2
        ";
        let expressions = Vampir::from(test_expressions);
    }

    #[test]
    pub(crate) fn test_bracketing() {
        let test_expressions = "x - (w*(y - z - w)-x)*(w+z)";
        let expressions = Vampir::from(test_expressions);
    }

    #[test]
    pub(crate) fn test_invocation_one_output() {
        let test_expressions = "x + -7*(ec_check y z)";
        let _expressions = Vampir::from(test_expressions);
    }

    #[test]
    pub(crate) fn test_definition() {
        let test_definition = "
        def dist x y -> z { 
            x*x + 
            y*y = z*z
        }
        euclidean_distance = (dist a b)
        taxicab_distance = a + b
        ";
        let _vampir = Vampir::from(test_definition);
        println!("{:?}", _vampir);
    }
}
