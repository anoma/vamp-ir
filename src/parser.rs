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

// impl From<Pair<'_, Rule>> for Wire {
//     fn from(pair: Pair<'_, Rule>) -> Self {
//         Self::from(pair.as_str())
//     }
// }

// impl From<Pair<'_, Rule>> for Definition {
//     fn from(pair: Pair<Rule>) -> Definition {
//         let mut inner = pair.into_inner();
//         Definition {
//             name: inner.next().unwrap().as_str().into(),
//             inputs: inner
//                 .next()
//                 .unwrap()
//                 .into_inner()
//                 .map(|pair| Node::Wire(String::from(pair.as_str()))
//                 .collect::<Vec<_>>(),
//             outputs: match inner.peek().unwrap().as_rule() {
//                 Rule::wire_list => Some(
//                     inner
//                         .next()
//                         .unwrap()
//                         .into_inner()
//                         .map(|pair| Node::Wire(String::from(pair.as_str()))
//                         .collect::<Vec<_>>(),
//                 ),
//                 Rule::expression_list => None,
//                 _ => unreachable!(),
//             },
//             nodes: inner.map(Node::from).collect::<Vec<Node>>(),
//         }
//     }
// }

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

// impl From<Pair<'_, Rule>> for AliasInvocation {
//     fn from(pair: Pair<'_, Rule>) -> Self {
//         let mut inner = pair.into_inner();
//         Self {
//             name: inner.next().unwrap().as_str().into(),
//             inputs: inner.next().unwrap().into_inner().map(|pair|String::from(pair.as_str())).collect::<Vec<_>>(),
//         }
//     }
// }

pub fn from_pairs(mut pairs: Pairs<Rule>) -> Vec<Node> {
    pairs
        .next()
        .unwrap()
        .into_inner()
        .map(Node::from)
        .collect::<Vec<Node>>()
}

pub fn from_str(input: &str) -> Vec<Node> {
    from_pairs(VampirParser::parse(Rule::expression_list, input).unwrap())
}

pub fn pairs(input: &str) -> Pairs<Rule> {
    VampirParser::parse(Rule::expression_list, input).unwrap()
}

// impl From<&str> for Preamble {
//     fn from(input: &str) -> Self {
//         Preamble::from(VampirParser::parse(Rule::preamble, input).unwrap())
//     }
// }

// impl From<Pairs<'_, Rule>> for Preamble {
//     fn from(mut pairs: Pairs<'_, Rule>) -> Self {
//         Preamble(
//             pairs
//                 .next()
//                 .unwrap()
//                 .into_inner()
//                 .map(Definition::from)
//                 .collect::<Vec<Definition>>(),
//         )
//     }
// }

// folds two primaries according to operator precedence
fn infix(lhs: Node, op: Pair<Rule>, rhs: Node) -> Node {
    match op.as_rule() {
        Rule::plus => Node::Node(String::from("add"), Box::new(lhs), Box::new(rhs)),
        Rule::minus => Node::Node(String::from("sub"), Box::new(lhs), Box::new(rhs)),
        Rule::times => Node::Node(String::from("mul"), Box::new(lhs), Box::new(rhs)),
        Rule::power => Node::Node(String::from("exp"), Box::new(lhs), Box::new(rhs)),
        Rule::equals => Node::Node(String::from("eq"), Box::new(lhs), Box::new(rhs)),
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
            Node::AliasInvocation(
                inner.next().unwrap().as_str().into(),
                inner.next().unwrap().into_inner().map(|pair| Wire(String::from(pair.as_str()))).collect(),
            )
        }
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use pest::iterators::Pairs;

    use crate::ast::Preamble;
    use crate::parser::{from_str, pairs, VampirParser};
    use crate::pest::Parser;

    #[test]
    pub(crate) fn test_expressions() {
        let test_expressions = "
            x - 3 y - 5
            x*z*w - 3 = y - w + x
            x^3--10*x +7-y^2
        ";
        let expressions = from_str(test_expressions);
        println!("{:?}", expressions);
    }

    #[test]
    pub(crate) fn test_bracketing() {
        let test_expressions = "x - (w*(y - z - w)-x)*(w+z)";
        let expressions = from_str(test_expressions);
    }

    #[test]
    pub(crate) fn test_invocation_one_output() {
        let test_expressions = "x + -7*(ec_check y z)";
        let pairs = pairs(test_expressions);
        let expressions = from_str(test_expressions);
        println!("{:?}\n{:?}", pairs, expressions);
    }

    // #[test]
    // pub(crate) fn test_definition() {
    //     let test_definition = "def dist x y -> z { x*x + y*y = z*z }";
    //     let preamble = Preamble::from(test_definition);
    // }
}
