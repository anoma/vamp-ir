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

fn build_expression(mut pairs: Pairs<Rule>, climber: &PrecClimber<Rule>) -> Node {
    println!("in build expression:\n\t{:?}", pairs);
    climber.climb(pairs, build_primary, infix)
}

fn build_primary(pair: Pair<Rule>) -> Node {
    // primaries are monomials, which can be either a base, an exponential
    let inner = pair.into_inner().next().unwrap();
    println!(
        "in build primary:\n\t{:?}\n\t{:?}",
        inner.as_rule(),
        inner.as_str()
    );
    match inner.as_rule() {
        Rule::base => build_base(inner),
        Rule::exponential => build_exponential(inner),
        _ => unreachable!(),
    }
}

fn build_equation(mut pairs: Pairs<Rule>, climber: &PrecClimber<Rule>) -> Node {
    println!("in build equation:\n\t{:?}", pairs);
    let lhs = build_expression(pairs.next().unwrap().into_inner(), climber);
    let rhs = build_expression(pairs.next().unwrap().into_inner(), climber);
    Node::Gate("sub".to_string(), Box::new(lhs), Box::new(rhs))
}

fn build_constraint(mut pairs: Pairs<Rule>, climber: &PrecClimber<Rule>) -> Node {
    let inner = pairs.next().unwrap();
    match inner.as_rule() {
        Rule::expression => build_expression(inner.into_inner(), climber),
        Rule::equation => build_equation(inner.into_inner(), climber),
        _ => unreachable!(),
    }
}

fn infix(lhs: Node, op: Pair<Rule>, rhs: Node) -> Node {
    match op.as_rule() {
        Rule::plus => Node::Gate("add".to_string(), Box::new(lhs), Box::new(rhs)),
        Rule::minus => Node::Gate("sub".to_string(), Box::new(lhs), Box::new(rhs)),
        Rule::times => Node::Gate("mul".to_string(), Box::new(lhs), Box::new(rhs)),
        _ => unreachable!(),
    }
}

pub fn build_circuit(mut pairs: Pairs<Rule>) -> Circuit {
    let climber = PrecClimber::new(vec![
        Operator::new(Rule::plus, Assoc::Left) | Operator::new(Rule::minus, Assoc::Left),
        Operator::new(Rule::times, Assoc::Left),
    ]);

    fn build_node(pair: Pair<Rule>, climber: &PrecClimber<Rule>) -> Node {
        println!(
            "in parse_circuit:\n\t{:?}\n\t{:?}",
            pair.as_rule(),
            pair.as_str()
        );
        let rule = pair.as_rule();
        let mut inner = pair.into_inner();
        match rule {
            Rule::constraint => build_constraint(inner, climber),
            Rule::expression => build_expression(inner, climber),
            Rule::equation => build_equation(inner, climber),
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
        let test_circuit = "x*z*w - 3 = y - (w + x)";
        let circuit = parse(test_circuit);
    }

    // #[test]
    // pub(crate) fn test_equation() {
    //     let eq_circuit = "x*z*w - 3 = y - z";
    //     let sub_circuit = "x*z*w - 3 = y - z";
    //     let circuit = parse(test_circuit);
    // }
}
