use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Gate {
    pub name: String,
    pub inputs: Vec<Box<Node>>,
    pub outputs: Option<Vec<Box<Node>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Wire {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constant(pub i64);

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Gate(Gate),
    Wire(Wire),
}

#[derive(Debug, PartialEq)]
pub struct Circuit(pub Vec<Node>);

#[derive(Debug, PartialEq)]
pub struct Definition {
    pub name: String,
    pub inputs: Vec<Node>,
    pub outputs: Option<Vec<Node>>,
    pub circuit: Circuit,
}

#[derive(Debug, PartialEq)]
pub struct Preamble(pub Vec<Definition>);

#[derive(Debug, PartialEq)]
pub struct Vampir {
    pub preamble: Preamble,
    pub circuit: Circuit,
}

impl From<&str> for Wire {
    fn from(item: &str) -> Self {
        Self {
            name: String::from(item),
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Gate(gate) => match gate.name.as_str() {
                "add" => {
                    write!(
                        f,
                        "{} + {}",
                        gate.inputs[0],
                        gate.outputs.as_ref().unwrap()[0]
                    )
                }
                "sub" => {
                    write!(
                        f,
                        "{} - {}",
                        gate.inputs[0],
                        gate.outputs.as_ref().unwrap()[0]
                    )
                }
                "mul" => {
                    write!(
                        f,
                        "{}*{}",
                        gate.inputs[0],
                        gate.outputs.as_ref().unwrap()[0]
                    )
                }
                "exp" => {
                    write!(
                        f,
                        "{}^{}",
                        gate.inputs[0],
                        gate.outputs.as_ref().unwrap()[0],
                    )
                }
                "eq" => {
                    write!(
                        f,
                        "{} = {}",
                        gate.inputs[0],
                        gate.outputs.as_ref().unwrap()[0],
                    )
                }
                _ => match &gate.outputs {
                    Some(outputs) => {
                        write!(f, "{}({} -> {})", gate.name, gate.inputs[0], outputs[0])
                    }
                    None => write!(f, "{}({})", gate.name, gate.inputs[0]),
                },
            },
            Node::Wire(wire) => write!(f, "{}", wire.name),
        }
    }
}

impl fmt::Display for Circuit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "\t{}",
            self.0
                .iter()
                .map(|constraint| format!("{}", constraint))
                .collect::<Vec<_>>()
                .join("\n\t")
        )
    }
}

impl fmt::Display for Definition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let list_to_str = |v: &Vec<Node>| {
            v.iter()
                .map(|n| format!("{}", n))
                .collect::<Vec<_>>()
                .join(" ")
        };
        let input_str = list_to_str(&self.inputs);
        let output_str = match &self.outputs {
            Some(outputs) => format!("-> {}", list_to_str(outputs)),
            None => String::new(),
        };
        write!(
            f,
            "def {} {}{} {{\n\t{}\n}}",
            self.name, input_str, output_str, self.circuit,
        )
    }
}

impl fmt::Display for Preamble {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|def| format!("{}", def))
                .collect::<Vec<_>>()
                .join("\n\t\t")
        )
    }
}
