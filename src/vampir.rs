use crate::ast;
use std::collections::HashSet;

#[derive(Default)]
pub struct CircuitEnv {
    wires: HashSet<String>,
    pub_wires: HashSet<String>,
    out_wires: HashSet<String>,
    gates: Vec<ast::GateExpression>,
}

impl CircuitEnv {
    pub fn default() -> Self {
        let wires = HashSet::new();
        let pub_wires = HashSet::new();
        let out_wires = HashSet::new();
        let gates = Vec::new();
        CircuitEnv {wires, pub_wires, out_wires, gates}
    }
    pub fn synth(&mut self, ast_circuit: ast::Circuit) -> () {
        ast_circuit.statements.iter().for_each(|statement| {
            match statement {
                ast::Statement::PubStatement(st) => {
                    st.wires.iter().for_each(|id| {
                        self.wires.insert(id.value.clone());
                        self.pub_wires.insert(id.value.clone());
                    });
                },
                ast::Statement::AliasStatement(_) => {
                },
                ast::Statement::ConstraintStatement(st) => {
                    match st {
                        ast::ConstraintStatement::GateExpression(gate) => {
                            gate.expressions.iter().for_each(|id| {
                                self.wires.insert(id.value.clone());
                            });
                            self.gates.push(gate.clone());
                        },
                        _ => {}
                    }
                }
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast, vampir};

    #[test]
    fn test_circuit_pub_and_gate() {
        let ast_circuit = ast::parse_circuit_from_string("
pub a d
gate a b
gate b c
");
        let mut circuit = vampir::CircuitEnv::default();
        circuit.synth(ast_circuit);
        assert_eq!(circuit.pub_wires.len(), 2);
        assert_eq!(circuit.wires.len(), 4);
        assert_eq!(circuit.gates.len(), 2);
    }

}
