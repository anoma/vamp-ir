use std::fmt;
use crate::VampirParser;

#[derive(Debug, PartialEq)]
pub struct Circuit(pub Vec<Node>);

impl From<&str> for Circuit {
    fn from(item: &str) -> Self {
        VampirParser::parse_circuit(item)
    }
}

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
                    write!(f, "({} + {})", left, right)
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

impl Circuit {
    // fn expand_exponentials(self) -> Node {  

    // }
}