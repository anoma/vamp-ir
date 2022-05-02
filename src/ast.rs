use pest::Parser;

use pest::iterators::{Pair, Pairs};
use pest::prec_climber::PrecClimber;
use pest::prec_climber::{Assoc, Operator};

use std::fmt;

#[derive(Parser)]
#[grammar = "vampir.pest"]
struct VampirParser;

#[derive(Debug, PartialEq)]
pub struct Circuit(Vec<Node>);

impl fmt::Display for Circuit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Circuit\n\t{}",
            self.0
                .iter()
                .map(|constraint| format!("{}", constraint))
                .collect::<Vec<_>>()
                .join("\n\t")
        )
    }
}

pub enum CircuitObject {
    Circuit,
    //Constraint,
    //Expression,
    Equation(Node, Node),
    Node,
}

pub enum Wire {
    Input(String),
    Internal(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Gate(String, Box<Node>, Box<Node>),
    Wire(String),
    Constant(u64),
    Exponential(Box<Node>, usize),
    EndOfInput(),
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Gate(name, left, right) => match name.as_str() {
                "sub" => {
                    write!(f, "({} - {})", left, right)
                }
                "add" => {
                    write!(f, "{} + {}", left, right)
                }
                "mul" => {
                    write!(f, "{}*{}", left, right)
                }
                _ => {
                    write!(f, "{}({} {})", name, left, right)
                }
            },
            Node::Exponential(node, power) => write!(f, "{}^{}", node, power),
            Node::Wire(name) => write!(f, "{}", name),
            Node::Constant(num) => write!(f, "{}", num),
            Node::EndOfInput() => write!(f, ""),
        }
    }
}

fn build_base(pair: Pair<Rule>) -> Node {
    let inner = pair.into_inner().next().unwrap();
    println!(
        "in build base:\n\t{:?}\n\t{:?}",
        inner.as_rule(),
        inner.as_str()
    );
    match inner.as_rule() {
        Rule::whole => Node::Constant(inner.as_str().to_string().parse::<u64>().unwrap()),
        Rule::wire => Node::Wire(inner.as_str().to_string()),
        _ => unreachable!(),
    }
}

fn expand_exponential(pair: Pair<Rule>) -> Node {
    let mut exp_sequence = pair.clone().into_inner().flatten();

    let base_pair = exp_sequence.next().unwrap();
    let exp = exp_sequence
        .last()
        .unwrap()
        .as_str()
        .to_string()
        .parse::<usize>()
        .unwrap();

    let mut res = build_base(base_pair.clone());
    let base = build_base(base_pair);
    for i in 1..exp {
        res = Node::Gate(
            "mul".to_string(),
            Box::new(base.clone()),
            Box::new(res.clone()),
        )
    }
    res
}

fn build_exponential(pair: Pair<Rule>) -> Node {
    let mut exp_sequence = pair.clone().into_inner().flatten();
    let base_pair = exp_sequence.next().unwrap();
    let exp = exp_sequence
        .last()
        .unwrap()
        .as_str()
        .to_string()
        .parse::<usize>()
        .unwrap();
    Node::Exponential(Box::new(build_base(base_pair)), exp)
}

fn build_expression(pair: Pair<Rule>) -> Node {
    // primaries are monomials, which can be either a base or an exponential
    let inner_pair = pair.into_inner().next().unwrap();
    println!(
        "in build expression:\n\t{:?}\n\t{:?}",
        inner_pair.as_rule(),
        inner_pair.as_str()
    );
    match inner_pair.as_rule() {
        Rule::base => build_base(inner_pair),
        Rule::exponential => build_exponential(inner_pair),
        Rule::expression => build_expression(inner_pair.into_inner().next().unwrap()),
        _ => unreachable!(),
    }
}

fn build_equation(pair: Pair<Rule>) -> Node {
    // primaries are monomials, which can be either a base or an exponential
    let inner_pair = pair.into_inner().next().unwrap();
    println!(
        "in build expression:\n\t{:?}\n\t{:?}",
        inner_pair.as_rule(),
        inner_pair.as_str()
    );
    match inner_pair.as_rule() {
        Rule::base => build_base(inner_pair),
        Rule::exponential => build_exponential(inner_pair),
        Rule::expression => build_expression(inner_pair.into_inner().next().unwrap()),
        _ => unreachable!(),
    }
}

fn infix(lhs: Node, op: Pair<Rule>, rhs: Node) -> Node {
    match op.as_rule() {
        Rule::plus => Node::Gate("add".to_string(), Box::new(lhs), Box::new(rhs)),
        Rule::minus => Node::Gate("sub".to_string(), Box::new(lhs), Box::new(rhs)),
        Rule::times => Node::Gate("mul".to_string(), Box::new(lhs), Box::new(rhs)),
        //Rule::equals => Node::Gate("sub".to_string(), Box::new(lhs), Box::new(rhs)),
        _ => unreachable!(),
    }
}

pub fn build_circuit(mut pairs: Pairs<Rule>) -> Circuit {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::plus, Assoc::Left)
            //| Operator::new(Rule::equals, Assoc::Left)
            | Operator::new(Rule::minus, Assoc::Left),
        Operator::new(Rule::times, Assoc::Left),

    ]);

    fn build_node(pair: Pair<Rule>, climber: &PrecClimber<Rule>) -> Node {
        println!(
            "in parse_circuit:\n\t{:?}\n\t{:?}",
            pair.as_rule(),
            pair.as_str()
        );
        match pair.as_rule() {
            Rule::constraint => build_node(pair.into_inner().next().unwrap(), climber),
            Rule::expression => climber.climb(pair.into_inner(), build_expression, infix),
            Rule::equation => build_equation(pair.into_inner().next().unwrap()),
            Rule::EOI => Node::EndOfInput(),
            _ => unreachable!(),
        }
    }
    Circuit(
        pairs
            .next()
            .unwrap()
            .into_inner()
            .map(|pair| build_node(pair, &climber))
            .collect::<Vec<_>>(),
    )
}

pub fn parse(vampir: &str) -> Circuit {
    build_circuit(VampirParser::parse(Rule::circuit, vampir).unwrap())
}

#[cfg(test)]
mod tests {
use crate::ast::{build_circuit, parse};

    #[test]
    pub(crate) fn test_circuit() {
        let test_circuit = "
            x^6 - 4*y*y + 3*x  + 7
            4 - z - other_var^3
        ";
        let circuit = parse(test_circuit);
    }
}
