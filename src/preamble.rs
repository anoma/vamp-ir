use crate::VampirParser;
use crate::ast::{Node, Identifier, VampirObject};

#[derive(Debug, PartialEq)]
pub struct Preamble(pub Vec<Definition>);

// impl From<&str> for Preamble {
//     fn from(item: &str) -> Self {
//         VampirParser::parse_preamble(item)
//     }
// }

#[derive(Debug, PartialEq)]
pub struct Definition {
    pub name: Identifier,
    pub inputs: WireList,
    pub outputs: Option<WireList>,
    pub constraints: Vec<Node>,
}

#[derive(Debug, PartialEq)]
pub struct WireList(Vec<Node::Wire>);