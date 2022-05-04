use crate::ast::{Node};
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Circuit(pub Vec<Node>);

// impl From<&str> for Circuit {
//     fn from(item: &str) -> Self {
//         VampirParser::parse_circuit(item)
//     }
// }

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

impl Circuit {
    // fn expand_exponentials(self) -> Node {

    // }
}
