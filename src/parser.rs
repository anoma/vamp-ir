use std::collections::HashMap;

use pest::{
    iterators::{Pair, Pairs},
    prec_climber::{Assoc, Operator, PrecClimber},
    Parser,
};

use crate::{ast::*, circuit::Wire};

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
            Rule::wire => Node::Wire(String::from(pair.as_str())),
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
        let mut expressions: Vec<Box<Node>> = vec![];
        inner.for_each(|pair| match pair.as_rule() {
            Rule::alias_definition => {
                let mut inner = pair.into_inner();
                let name = inner.next().unwrap().as_str().into();
                definitions.insert(name, Definition::from(inner));
            }
            Rule::expression => expressions.push(Box::new(Node::from(pair))),
            Rule::EOI => (),
            _ => unreachable!(),
        });
        Vampir {
            definitions,
            expressions,
        }
    }
}

impl From<Pairs<'_, Rule>> for Definition {
    fn from(mut pairs: Pairs<Rule>) -> Definition {
        Definition {
            inputs: pairs
                .next()
                .unwrap()
                .into_inner()
                .map(|pair| Node::Wire(String::from(pair.as_str())))
                .collect::<Vec<_>>(),
            outputs: match pairs.peek().unwrap().as_rule() {
                Rule::outputs => pairs
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(|pair| Node::Wire(String::from(pair.as_str())))
                    .collect::<Vec<_>>(),
                _ => vec![],
            },
            nodes: pairs.map(Node::from).collect::<Vec<Node>>(),
        }
    }
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

pub fn from_str(input: &str) -> Vec<Node> {
    from_pairs(VampirParser::parse(Rule::vampir, input).unwrap())
}

pub fn pairs(input: &str) -> Pairs<Rule> {
    VampirParser::parse(Rule::vampir, input).unwrap()
}

// folds two primaries according to operator precedence
fn infix(lhs: Node, op: Pair<Rule>, rhs: Node) -> Node {
    match op.as_rule() {
        Rule::plus => Node::Node(String::from("add"), vec![Box::new(lhs), Box::new(rhs)]),
        Rule::minus => Node::Node(String::from("sub"), vec![Box::new(lhs), Box::new(rhs)]),
        Rule::times => Node::Node(String::from("mul"), vec![Box::new(lhs), Box::new(rhs)]),
        Rule::power => Node::Node(String::from("exp"), vec![Box::new(lhs), Box::new(rhs)]),
        Rule::equals => Node::Node(String::from("eq"), vec![Box::new(lhs), Box::new(rhs)]),
        _ => unreachable!(),
    }
}

fn primary(pair: Pair<Rule>) -> Node {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::wire => Node::Wire(String::from(inner.as_str())),
        Rule::constant => Node::Constant(inner.as_str().to_string().parse::<i64>().unwrap()),
        Rule::expression => CLIMBER.climb(inner.into_inner(), primary, infix),
        Rule::alias_invocation => {
            let mut inner = inner.into_inner();
            Node::Node(
                inner.next().unwrap().as_str().into(),
                inner
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(|pair| Box::new(Node::Wire(String::from(pair.as_str()))))
                    .collect(),
            )
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
