use std::collections::HashMap;

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

impl From<Pair<'_, Rule>> for Node {
    fn from(pair: Pair<Rule>) -> Node {
        match pair.as_rule() {
            Rule::expression => CLIMBER.climb(pair.into_inner(), primary, infix),
            Rule::wire => Node::Wire(Wire(String::from(pair.as_str()))),
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

impl From<Pair<'_, Rule>> for Vampir {
    fn from(pair: Pair<Rule>) -> Vampir {
        let inner = pair.into_inner();
        let mut definitions: HashMap<String, Definition> = HashMap::new();
        let mut nodes: Vec<Node> = vec![];
        inner.for_each(|pair| match pair.as_rule() {
            Rule::alias_definition => {
                let mut inner = pair.into_inner();
                let name = inner.next().unwrap().as_str().into();
                definitions.insert(name, Definition::from(inner));
            }
            Rule::expression => nodes.push(Node::from(pair)),
            Rule::EOI => (),
            _ => unreachable!(),
        });

        let inputs = nodes.iter().flat_map(|node| node.inputs()).collect();
        Vampir {
            definitions,
            inputs,
            nodes,
        }
    }
}

impl From<Pairs<'_, Rule>> for Definition {
    fn from(mut pairs: Pairs<Rule>) -> Definition {
        let inputs = pairs
            .next()
            .unwrap()
            .into_inner()
            .map(|pair| Wire(String::from(pair.as_str())))
            .collect::<Vec<_>>();
        let outputs = match pairs.peek().unwrap().as_rule() {
            Rule::definition_outputs => pairs
                .next()
                .unwrap()
                .into_inner()
                .map(|pair| Wire(String::from(pair.as_str())))
                .collect::<Vec<_>>(),
            _ => vec![],
        };

        let mut wire_list = WireList{inputs: inputs.clone(), wires: outputs.clone()};
        let nodes = pairs
            .map(|pair| remove_name(Node::from(pair), &mut wire_list))
            .collect();

        Definition {
            inputs,
            outputs,
            nodes,
        }
    }
}

fn remove_name(node: Node, wire_list: &mut WireList) -> Node {
    match node {
        Node::Wire(wire) => match wire_list.iter().position(|w| wire == *w) {
            Some(num) => Node::Index(num),
            None => {
                wire_list.push(wire);
                Node::Index(wire_list.len() - 1)
            }
        },
        Node::Gate(Gate{name, inputs, mut wire_list}) => Node::Gate(Gate{name, inputs: remove_names(inputs, &mut wire_list), wire_list}),
        _ => node,
    }
}

fn remove_names(nodes: Vec<Node>, wire_list: &mut WireList) -> Vec<Node> {
    nodes
        .into_iter()
        .map(|node| remove_name(node, wire_list))
        .collect()
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

pub fn from_pairs(mut pairs: Pairs<Rule>) -> Vec<Node> {
    pairs
        .next()
        .unwrap()
        .into_inner()
        .map(Node::from)
        .collect::<Vec<Node>>()
}

// folds two primaries according to operator precedence
fn infix(lhs: Node, op: Pair<Rule>, rhs: Node) -> Node {
    match op.as_rule() {
        Rule::plus => Node::Gate(Gate{name: String::from("add"), inputs: vec![lhs, rhs], wire_list: WireList::new()}),
        Rule::minus => Node::Gate(Gate{name: String::from("sub"), inputs: vec![lhs, rhs], wire_list: WireList::new()}),
        Rule::times => Node::Gate(Gate{name: String::from("mul"), inputs: vec![lhs, rhs], wire_list: WireList::new()}),
        Rule::power => Node::Gate(Gate{name: String::from("pow"), inputs: vec![lhs, rhs], wire_list: WireList::new()}),
        Rule::equals => Node::Gate(Gate{name: String::from("eq"), inputs: vec![lhs, rhs], wire_list: WireList::new()}),
        _ => unreachable!(),
    }
}

fn primary(pair: Pair<Rule>) -> Node {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::wire => Node::Wire(Wire(String::from(inner.as_str()))),
        Rule::constant => Node::Constant(inner.as_str().to_string().parse::<i64>().unwrap()),
        Rule::expression => CLIMBER.climb(inner.into_inner(), primary, infix),
        Rule::alias_invocation => {
            let mut inner = inner.into_inner();
            Node::Gate(Gate{
                name: inner.next().unwrap().as_str().into(),
                inputs: inner
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(primary)
                    .collect(),
                wire_list: WireList::new(),
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
