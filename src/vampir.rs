use crate::ast;
use std::collections::HashSet;

pub struct Gate {
    name: String,
    wires: Vec<String>
}

pub struct Circuit {
    wires: HashSet<String>,
    pub_wires: HashSet<String>,
    gates: Vec<Gate>,
}

impl Circuit {
    pub fn new(ast_circuit: ast::Circuit) -> Self {
        let mut pub_wires: HashSet<String> = HashSet::new();
        let mut wires: HashSet<String> = HashSet::new();
        let gates: Vec<Gate> = ast_circuit.statements.iter().filter_map(|statement| {
            match statement {
                ast::Statement::PubStatement(st) => {
                    for id in st.wires.iter() {
                        pub_wires.insert(id.value.clone());
                    }
                    None
                },
                ast::Statement::AliasStatement(_) => {
                    None
                },
                ast::Statement::ConstraintStatement(st) => {
                    match st {
                        ast::ConstraintStatement::GateExpression(gate) => {
                            let mut w = vec![];
                            for id in &gate.expressions {
                                wires.insert(id.value.clone());
                                w.push(id.value.clone());
                            }
                            let gate = Gate {name: gate.name.value.clone(), wires: w };
                            Some(gate)
                        }
                        _ => None
                    }
                }
            }
        }).collect();
        Circuit { wires, pub_wires, gates }
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast, vampir};

    #[test]
    fn test_circuit_pub_and_gate() {
        let ast_circuit = ast::parse_circuit_from_string("
pub a
gate a b
gate b c
");
        let circuit = vampir::Circuit::new(ast_circuit);
        assert_eq!(circuit.pub_wires.len(), 1);
        assert_eq!(circuit.wires.len(), 3);
        assert_eq!(circuit.gates.len(), 2);
    }

}
