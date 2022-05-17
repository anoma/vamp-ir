use std::fmt;

use crate::circuit::Wire;

#[derive(Debug, Clone, PartialEq)]
pub struct Constant(pub i64);

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Node(String, Box<Node>, Box<Node>),
    AliasInvocation(String, Vec<Wire>),
    Wire(String),
    Constant(i64),
}

#[derive(Debug, PartialEq)]
pub struct Definition {
    pub name: String,
    pub inputs: Vec<Node>,
    pub outputs: Option<Vec<Node>>,
    pub nodes: Vec<Node>,
}

#[derive(Debug, PartialEq)]
pub struct Vampir {
    pub definitions: Vec<Definition>,
    pub expressions: Vec<Node>,
}

// impl From<&str> for Wire {
//     fn from(item: &str) -> Self {
//         Self(String::from(item))
//     }
// }

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// impl fmt::Display for Gate {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             (Box<Node>, Box<Node>) =>
//             Node::Gate(gate) => match gate.name.as_str() {
//                 "add" => {
//                     write!(
//                         f,
//                         "{} + {}",
//                         gate.inputs[0],
//                         gate.outputs.as_ref().unwrap()[0]
//                     )
//                 }
//                 "sub" => {
//                     write!(
//                         f,
//                         "{} - {}",
//                         gate.inputs[0],
//                         gate.outputs.as_ref().unwrap()[0]
//                     )
//                 }
//                 "mul" => {
//                     write!(
//                         f,
//                         "{}*{}",
//                         gate.inputs[0],
//                         gate.outputs.as_ref().unwrap()[0]
//                     )
//                 }
//                 "exp" => {
//                     write!(
//                         f,
//                         "{}^{}",
//                         gate.inputs[0],
//                         gate.outputs.as_ref().unwrap()[0],
//                     )
//                 }
//                 "eq" => {
//                     write!(
//                         f,
//                         "{} = {}",
//                         gate.inputs[0],
//                         gate.outputs.as_ref().unwrap()[0],
//                     )
//                 }
//                 _ => match &gate.outputs {
//                     Some(outputs) => {
//                         write!(f, "{}({} -> {})", gate.name, gate.inputs[0], outputs[0])
//                     }
//                     None => write!(f, "{}({})", gate.name, gate.inputs[0]),
//                 },
//             },
//             Node::Wire(wire) => write!(f, "{}", wire.0),
//         }
//     }
// }

// impl fmt::Display for Definition {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         let list_to_str = |v: &Vec<Node>| {
//             v.iter()
//                 .map(|n| format!("{}", n))
//                 .collect::<Vec<_>>()
//                 .join(" ")
//         };
//         let input_str = list_to_str(&self.inputs);
//         let output_str = match &self.outputs {
//             Some(outputs) => format!("-> {}", list_to_str(outputs)),
//             None => String::new(),
//         };
//         write!(
//             f,
//             "def {} {}{} {{\n\t{}\n}}",
//             self.name, input_str, output_str, self.nodes,
//         )
//     }
// }

// impl fmt::Display for Preamble {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         write!(
//             f,
//             "{}",
//             self.0
//                 .iter()
//                 .map(|def| format!("{}", def))
//                 .collect::<Vec<_>>()
//                 .join("\n\t\t")
//         )
//     }
// }
