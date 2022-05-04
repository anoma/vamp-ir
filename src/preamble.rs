use crate::ast::VampirParser;
use crate::ast::{Identifier, Node, Wire};
use crate::Circuit;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Preamble(pub Vec<Definition>);

impl fmt::Display for Preamble {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0.iter().map(|def| format!("{}", def)).collect::<Vec<_>>().join("\n\t\t"))
    }
}

impl From<&str> for Preamble {
    fn from(item: &str) -> Self {
        VampirParser::parse_preamble(item)
    }
}

#[derive(Debug, PartialEq)]
pub struct Definition {
    pub name: Identifier,
    pub inputs: Vec<Node>,
    pub outputs: Option<Vec<Node>>,
    pub circuit: Circuit,
}

impl fmt::Display for Definition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let list_to_str = |v: &Vec<Node> | v.iter().map(|n| format!("{}", n)).collect::<Vec<_>>().join(" ");
        let input_str = list_to_str(&self.inputs);
        let output_str = match &self.outputs {
            Some(outputs) => format!("-> {}", list_to_str(&outputs.to_vec())),
            None => format!(""),
        };
        write!(
            f,
            "def {} {}{} {{\n\t{}\n}}",
            self.name,
            input_str,
            output_str,
            self.circuit,
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct WireList(Vec<Wire>);
