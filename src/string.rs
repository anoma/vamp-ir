use crate::ast::*;
use crate::error::Error;
use crate::transform::{collect_module_variables, compile, FieldOps};
use std::collections::{HashMap, HashSet};
use num_bigint::BigInt;

// Define the address type
pub type Address = usize;
pub type PortIndex = usize;

// Define a port as a pair of node address and a list index
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Port(Address, PortIndex);

// Define different types of nodes
#[derive(Clone, Debug)]
pub enum Node {
    Addition(Vec<Port>), // The first port is the sum of the remaining ports
    Multiplication(Vec<Port>), // The first port is the product of the remaining ports
    Equality(Vec<Variable>, Vec<Port>), // All ports are equal
    AddConstant(BigInt, Port, Port), // First port equals the constant plus the second port
    MultiplyConstant(BigInt, Port, Port), // First port equals the constant times the second port
    ExponentiateConstant(BigInt, Port, Port), // First port equals the second port to the power of the constant
    Unrestricted(Port), // The port is an unrestricted natural number
    Constant(BigInt, Port), // The port is equal to the constant
}

// Define the String Diagram
#[derive(Clone, Debug)]
pub struct StringDiagram {
    pub nodes: HashMap<Address, Node>,
    pub next_address: Address,
}

impl StringDiagram {
    fn new() -> Self {
        StringDiagram {
            nodes: HashMap::new(),
            next_address: 0,
        }
    }

    fn add_node(&mut self, node: Node) -> Address {
        let address = self.next_address;
        self.nodes.insert(address, node);
        self.next_address += 1;
        address
    }

    // Check if the diagram is well-formed, i.e., every port is connected to a port that points back to itself.
    pub fn is_well_formed(&self) -> bool {
        for (address, node) in &self.nodes {
            match node {
                Node::Addition(ports)
                | Node::Multiplication(ports)
                | Node::Equality(_, ports) => {
                    for (port_index, port) in ports.iter().enumerate() {
                        if !self.is_connected_back(port, (*address, port_index)) {
                            return false;
                        }
                    }
                }
                Node::AddConstant(_, port1, port2)
                | Node::MultiplyConstant(_, port1, port2)
                | Node::ExponentiateConstant(_, port1, port2) => {
                    if !self.is_connected_back(port1, (*address, 0)) || !self.is_connected_back(port2, (*address, 1)) {
                        return false;
                    }
                }
                Node::Unrestricted(port) | Node::Constant(_, port) => {
                    if !self.is_connected_back(port, (*address, 0)) {
                        return false;
                    }
                }
            }
        }
        true
    }

    // Helper function to check if the port is connected back to the original (address, port_index).
    fn is_connected_back(&self, port: &Port, original: (Address, PortIndex)) -> bool {
        let Port(target_address, target_port_index) = port;
        if let Some(target_node) = self.nodes.get(target_address) {
            match target_node {
                Node::Addition(ports)
                | Node::Multiplication(ports)
                | Node::Equality(_, ports) => {
                    if let Some(Port(back_address, back_port_index)) = ports.get(*target_port_index) {
                        return *back_address == original.0 && *back_port_index == original.1;
                    }
                }
                Node::AddConstant(_, port1, port2)
                | Node::MultiplyConstant(_, port1, port2)
                | Node::ExponentiateConstant(_, port1, port2) => {
                    let back_port = if *target_port_index == 0 { port1 } else { port2 };
                    return back_port.0 == original.0 && back_port.1 == original.1;
                }
                Node::Unrestricted(port) | Node::Constant(_, port) => {
                    return port.0 == original.0 && port.1 == original.1;
                }
            }
        }
        false
    }

    fn get_target_port(&self, port: &Port) -> Port {
        match &self.nodes[&port.0] {
            Node::Equality(_, ports) | Node::Addition(ports) | Node::Multiplication(ports) => ports[port.1].clone(),
            Node::AddConstant(_, p1, p2) | Node::MultiplyConstant(_, p1, p2) | Node::ExponentiateConstant(_, p1, p2) => {
                if port.1 == 0 {
                    p1.clone()
                } else {
                    p2.clone()
                }
            }
            Node::Unrestricted(p) => p.clone(),
            Node::Constant(_, p) => p.clone(),
        }
    }
}

fn get_or_create_equality_node(variable: &Variable, pointing_port: Port, diagram: &mut StringDiagram, variable_addresses: &mut HashMap<VariableId, Address>, input_ids: &HashSet<u32>) -> (Address, usize) {
    // Does this variable already have an address? If not, make one.
    let equality_address = *variable_addresses.entry(variable.id).or_insert_with(|| {
        // If we're looking at an input variable, store it.
        let input_vec: Vec<Variable>;
        if input_ids.contains(&variable.id) {
            input_vec = vec![variable.clone()];
        } else {
            input_vec = vec![];
        }

        diagram.add_node(Node::Equality(input_vec, Vec::new()))
    });

    // Find the largest index in that equality node. Place the pointing port there.
    let next_idx;
    if let Node::Equality(_, ports) = diagram.nodes.get_mut(&equality_address).unwrap() {
        next_idx = ports.len();
        ports.push(pointing_port);
    } else {
        panic!("Expected an equality node");
    }
    //println!("Ports at {} now {:?}.", equality_address, diagram.nodes.get_mut(&equality_address).unwrap().clone());

    (equality_address, next_idx)
}

// Given a list of 3ac equations, generate a semantically equivalent string diagram.
pub fn build_string_diagram(equations: Vec<TExpr>, input_ids: &HashSet<u32>) -> StringDiagram {
    let mut diagram = StringDiagram::new();
    let mut variable_addresses: HashMap<VariableId, Address> = HashMap::new();

    for eq in equations {
        //println!("Equation {eq:?}");
        if let Expr::Infix(InfixOp::Equal, left, right) = eq.v {
            match (&left.v, &right.v) {
                (Expr::Variable(var), Expr::Constant(c)) => {
                    // TODO: There's probably a better way to do this.
                    let equality_address = variable_addresses.get(&var.id).cloned();
                    let was_newly_created = equality_address.is_none();
                    
                    let next_addr = diagram.next_address;
                    let target_address = if was_newly_created { next_addr + 1 } else { next_addr };
                    
                    let (equality_address, new_port_index) = get_or_create_equality_node(&var, Port(target_address, 0), &mut diagram, &mut variable_addresses, input_ids);

                    //println!("0: Adding {:?} at {}", Node::Constant(c.clone(), Port(equality_address, new_port_index)), diagram.next_address);
                    diagram.add_node(Node::Constant(c.clone(), Port(equality_address, new_port_index)));
                }
                (Expr::Variable(var1), Expr::Variable(var2)) => {
                    let eq_address1 = variable_addresses.get(&var1.id).cloned();
                    let eq_address2 = variable_addresses.get(&var2.id).cloned();
                    let was_newly_created1 = eq_address1.is_none();
                    let was_newly_created2 = eq_address2.is_none();

                    let next_id = diagram.next_address;
                    let target_address = match (was_newly_created1, was_newly_created2) {
                        (false, false) => next_id,
                        (true, true) => {
                            if var1.id == var2.id {
                                next_id + 1
                            } else {
                                next_id + 2
                            }
                        },
                        _ => next_id + 1,
                    };

                    let (equality_address1, new_port_index1) = get_or_create_equality_node(&var1, Port(target_address, 0), &mut diagram, &mut variable_addresses, input_ids);
                    let (equality_address2, new_port_index2) = get_or_create_equality_node(&var2, Port(target_address, 1), &mut diagram, &mut variable_addresses, input_ids);

                    // Connect these nodes by adding an equality node that points to both.
                    //println!("1: Adding {:?} at {}", Node::Equality(None, vec![Port(equality_address1, new_port_index1), Port(equality_address2, new_port_index2)]), diagram.next_address);
                    diagram.add_node(Node::Equality(vec![], vec![Port(equality_address1, new_port_index1), Port(equality_address2, new_port_index2)]));
                }
                (Expr::Variable(var1), Expr::Negate(var2p)) => {
                    if let Expr::Variable(var2) = &var2p.v {
                        let eq_address1 = variable_addresses.get(&var1.id).cloned();
                        let eq_address2 = variable_addresses.get(&var2.id).cloned();
                        let was_newly_created1 = eq_address1.is_none();
                        let was_newly_created2 = eq_address2.is_none();
    
                        let next_id = diagram.next_address;
                        let target_address = match (was_newly_created1, was_newly_created2) {
                            (false, false) => next_id,
                            (true, true) => {
                                if var1.id == var2.id {
                                    next_id + 1
                                } else {
                                    next_id + 2
                                }
                            },
                            _ => next_id + 1,
                        };
    
                        let (equality_address1, new_port_index1) = get_or_create_equality_node(&var1, Port(target_address, 0), &mut diagram, &mut variable_addresses, input_ids);
                        let (equality_address2, new_port_index2) = get_or_create_equality_node(&var2, Port(target_address, 1), &mut diagram, &mut variable_addresses, input_ids);
    
                        //println!("2: Adding {:?} at {}", Node::MultiplyConstant((-1).into(), Port(equality_address1, new_port_index1), Port(equality_address2, new_port_index2)), diagram.next_address);
                        diagram.add_node(Node::MultiplyConstant((-1).into(), Port(equality_address1, new_port_index1), Port(equality_address2, new_port_index2)));

                    }
                }
                (Expr::Variable(var), Expr::Infix(op, t1, t2)) => {
                    let mut new_var_ids = HashSet::new();
                    if !variable_addresses.contains_key(&var.id) {
                        new_var_ids.insert(var.id);
                    }

                    let ports;

                    let target_address: usize;

                    match (&t1.v, &t2.v) {
                        (Expr::Variable(term_var1), Expr::Variable(term_var2)) => {
                            if !variable_addresses.contains_key(&term_var1.id) {
                                new_var_ids.insert(term_var1.id);
                            }
                            if !variable_addresses.contains_key(&term_var2.id) {
                                new_var_ids.insert(term_var2.id);
                            }

                            target_address = diagram.next_address + new_var_ids.len();

                            if matches!(op, InfixOp::Subtract) || matches!(op, InfixOp::Divide) {
                                let (address0, port_index0) = get_or_create_equality_node(&var, Port(target_address, 1), &mut diagram, &mut variable_addresses, input_ids);
                                let (address1, port_index1) = get_or_create_equality_node(term_var1, Port(target_address, 0), &mut diagram, &mut variable_addresses, input_ids);
                                let (address2, port_index2) = get_or_create_equality_node(term_var2, Port(target_address, 2), &mut diagram, &mut variable_addresses, input_ids);
                                ports = vec![Port(address1, port_index1), Port(address0, port_index0), Port(address2, port_index2)];
                            } else {
                                let (address0, port_index0) = get_or_create_equality_node(&var, Port(target_address, 0), &mut diagram, &mut variable_addresses, input_ids);
                                let (address1, port_index1) = get_or_create_equality_node(term_var1, Port(target_address, 1), &mut diagram, &mut variable_addresses, input_ids);
                                let (address2, port_index2) = get_or_create_equality_node(term_var2, Port(target_address, 2), &mut diagram, &mut variable_addresses, input_ids);
                                ports = vec![Port(address0, port_index0), Port(address1, port_index1), Port(address2, port_index2)];
                            }
                        },
                        (Expr::Constant(const1), Expr::Variable(term_var2)) => {
                            if !variable_addresses.contains_key(&term_var2.id) {
                                new_var_ids.insert(term_var2.id);
                            }

                            target_address = diagram.next_address + new_var_ids.len()+1;

                            if matches!(op, InfixOp::Subtract) || matches!(op, InfixOp::Divide) {
                                let (address0, port_index0) = get_or_create_equality_node(&var, Port(target_address, 1), &mut diagram, &mut variable_addresses, input_ids);
                                //println!("5: Adding {:?} at {}", Node::Constant(const1.clone(), Port(target_address, 0)), diagram.next_address);
                                let address1 = diagram.add_node(Node::Constant(const1.clone(), Port(target_address, 0)));
                                let (address2, port_index2) = get_or_create_equality_node(term_var2, Port(target_address, 2), &mut diagram, &mut variable_addresses, input_ids);
                                ports = vec![Port(address1, 0), Port(address0, port_index0), Port(address2, port_index2)];
                            } else {
                                let (address0, port_index0) = get_or_create_equality_node(&var, Port(target_address, 0), &mut diagram, &mut variable_addresses, input_ids);
                                //println!("6: Adding {:?} at {}", Node::Constant(const1.clone(), Port(target_address, 1)), diagram.next_address);
                                let address1 = diagram.add_node(Node::Constant(const1.clone(), Port(target_address, 1)));
                                let (address2, port_index2) = get_or_create_equality_node(term_var2, Port(target_address, 2), &mut diagram, &mut variable_addresses, input_ids);
                                ports = vec![Port(address0, port_index0), Port(address1, 0), Port(address2, port_index2)];
                            }

                        },
                        (Expr::Variable(term_var1), Expr::Constant(const2)) => {
                            if !variable_addresses.contains_key(&term_var1.id) {
                                new_var_ids.insert(term_var1.id);
                            }

                            target_address = diagram.next_address + new_var_ids.len()+1;

                            if matches!(op, InfixOp::Subtract) || matches!(op, InfixOp::Divide) {
                                let (address0, port_index0) = get_or_create_equality_node(&var, Port(target_address, 1), &mut diagram, &mut variable_addresses, input_ids);
                                let (address1, port_index1) = get_or_create_equality_node(term_var1, Port(target_address, 0), &mut diagram, &mut variable_addresses, input_ids);
                                //println!("7: Adding {:?} at {}", Node::Constant(const2.clone(), Port(target_address, 2)), diagram.next_address);
                                let address2 = diagram.add_node(Node::Constant(const2.clone(), Port(target_address, 2)));
                                ports = vec![Port(address1, port_index1), Port(address0, port_index0), Port(address2, 0)];
                            } else {
                                let (address0, port_index0) = get_or_create_equality_node(&var, Port(target_address, 0), &mut diagram, &mut variable_addresses, input_ids);
                                let (address1, port_index1) = get_or_create_equality_node(term_var1, Port(target_address, 1), &mut diagram, &mut variable_addresses, input_ids);
                                //println!("8: Adding {:?} at {}", Node::Constant(const2.clone(), Port(target_address, 2)), diagram.next_address);
                                let address2 = diagram.add_node(Node::Constant(const2.clone(), Port(target_address, 2)));
                                ports = vec![Port(address0, port_index0), Port(address1, port_index1), Port(address2, 0)];
                            }

                        },
                        (Expr::Constant(const1), Expr::Constant(const2)) => {
                            target_address = diagram.next_address + new_var_ids.len()+2;

                            if matches!(op, InfixOp::Subtract) || matches!(op, InfixOp::Divide) {
                                let (address0, port_index0) = get_or_create_equality_node(&var, Port(target_address, 1), &mut diagram, &mut variable_addresses, input_ids);
                                //println!("9: Adding {:?} at {}", Node::Constant(const1.clone(), Port(target_address, 0)), diagram.next_address);
                                let address1 = diagram.add_node(Node::Constant(const1.clone(), Port(target_address, 0)));
                                //println!("10: Adding {:?} at {}", Node::Constant(const2.clone(), Port(target_address, 2)), diagram.next_address);
                                let address2 = diagram.add_node(Node::Constant(const2.clone(), Port(target_address, 2)));
                                ports = vec![Port(address1, 0), Port(address0, port_index0), Port(address2, 0)];
                            } else {
                                let (address0, port_index0) = get_or_create_equality_node(&var, Port(target_address, 0), &mut diagram, &mut variable_addresses, input_ids);
                                //println!("11: Adding {:?} at {}", Node::Constant(const1.clone(), Port(target_address, 1)), diagram.next_address);
                                let address1 = diagram.add_node(Node::Constant(const1.clone(), Port(target_address, 1)));
                                //println!("12: Adding {:?} at {}", Node::Constant(const2.clone(), Port(target_address, 2)), diagram.next_address);
                                let address2 = diagram.add_node(Node::Constant(const2.clone(), Port(target_address, 2)));
                                ports = vec![Port(address0, port_index0), Port(address1, 0), Port(address2, 0)];
                            }
                        },
                        _ => panic!("Impossible")
                    }

                    if matches!(op, InfixOp::Add) || matches!(op, InfixOp::Subtract) {
                        //println!("3: Adding {:?} at {}", Node::Addition(ports.clone()), diagram.next_address);
                        diagram.add_node(Node::Addition(ports));
                    } else if matches!(op, InfixOp::Multiply) || matches!(op, InfixOp::Divide) {
                        //println!("4: Adding {:?} at {}", Node::Multiplication(ports.clone()), diagram.next_address);
                        diagram.add_node(Node::Multiplication(ports));
                    }
                },
                _ => {
                    // Unsupported case
                }
            }
        }
    }

    diagram
}



pub fn get_variable_ids_in_equality_nodes(diagram: &StringDiagram) -> HashSet<u32> {
    let mut variable_ids = HashSet::new();
    
    for node in diagram.nodes.values() {
        if let Node::Equality(variables, _) = node {
            for variable in variables {
                variable_ids.insert(variable.id);
            }
        }
    }
    
    variable_ids
}

// Assign variables to all the ports
fn construct_port_vars(diagram: &StringDiagram) -> HashMap<Port, Variable> {
    let named_var_ids = get_variable_ids_in_equality_nodes(diagram);

    let mut port_vars: HashMap<Port, Variable> = HashMap::new();
    let mut variable_id_counter = 0;
    while named_var_ids.contains(&variable_id_counter) {
        variable_id_counter += 1;
    }
    
    // Assign names to ports from equality nodes that already know who they are
    for (address, node) in &diagram.nodes {
        if let Node::Equality(vars, ports) = node {
            if let Some(var) = vars.first() {
                for (index, target_port) in ports.iter().enumerate() {
                    let current_port = Port(*address, index);
                    
                    port_vars.insert(current_port, var.clone());
                    port_vars.insert(target_port.clone(), var.clone());
                }
            } else {
                let var = Variable { name: None, id: variable_id_counter };
                variable_id_counter += 1;
                while named_var_ids.contains(&variable_id_counter) {
                    variable_id_counter += 1;
                }

                for (index, target_port) in ports.iter().enumerate() {
                    let current_port = Port(*address, index);
                    
                    port_vars.insert(current_port, var.clone());
                    port_vars.insert(target_port.clone(), var.clone());
                }
            }
        }
    }
    
    // Assign variables to the remaining ports if they don't have one
    for (address, node) in &diagram.nodes {
        let ports = match node {
            Node::Equality(_, ports) => ports.clone(),
            Node::Addition(ports) => ports.clone(),
            Node::Multiplication(ports) => ports.clone(),
            Node::AddConstant(_, p1, p2) => vec![p1.clone(), p2.clone()],
            Node::MultiplyConstant(_, p1, p2) => vec![p1.clone(), p2.clone()],
            Node::ExponentiateConstant(_, p1, p2) => vec![p1.clone(), p2.clone()],
            Node::Unrestricted(p) => vec![p.clone()],
            Node::Constant(_, p) => vec![p.clone()],
        };
    
        for (index, target_port) in ports.iter().enumerate() {
            let current_port = Port(*address, index);
            if !port_vars.contains_key(&current_port) {
                let variable = Variable {
                    name: None,
                    id: variable_id_counter,
                };
                port_vars.insert(current_port, variable.clone());
                port_vars.insert(target_port.clone(), variable);

                variable_id_counter += 1;
                while named_var_ids.contains(&variable_id_counter) {
                    variable_id_counter += 1;
                }
            }
        }
    }
    
    port_vars
}

// Converts a string diagram which has already been prepared into a list of 3AC constraints.
pub fn convert_to_3ac(diagram: &StringDiagram) -> Vec<TExpr> {
    let port_vars = construct_port_vars(diagram);
    let mut expressions = Vec::new();

    for (_, node) in &diagram.nodes {
        match node {
            Node::Addition(ports) => {
                // Additionns should already be split up, and trivial removed
                if ports.len() == 3 {
                    let first_port = &ports[0];
                    let second_port = &ports[1];
                    let third_port = &ports[2];

                    expressions.push(TExpr {
                        v: Expr::Infix(
                            InfixOp::Equal,
                            Box::new(TExpr {
                                v: Expr::Variable(port_vars[first_port].clone()),
                                t: None,
                            }),
                            Box::new(TExpr {
                                v: Expr::Infix(
                                    InfixOp::Add,
                                    Box::new(TExpr {
                                        v: Expr::Variable(port_vars[second_port].clone()),
                                        t: None,
                                    }),
                                    Box::new(TExpr {
                                        v: Expr::Variable(port_vars[third_port].clone()),
                                        t: None,
                                    }),
                                ),
                                t: None,
                            }),
                        ),
                        t: None,
                    });
                }
            }
            Node::Multiplication(ports) => {
                // Multiplication should already be split up, and trivial removed
                if ports.len() == 3 {
                    let first_port = &ports[0];
                    let second_port = &ports[1];
                    let third_port = &ports[2];

                    expressions.push(TExpr {
                        v: Expr::Infix(
                            InfixOp::Equal,
                            Box::new(TExpr {
                                v: Expr::Variable(port_vars[first_port].clone()),
                                t: None,
                            }),
                            Box::new(TExpr {
                                v: Expr::Infix(
                                    InfixOp::Multiply,
                                    Box::new(TExpr {
                                        v: Expr::Variable(port_vars[second_port].clone()),
                                        t: None,
                                    }),
                                    Box::new(TExpr {
                                        v: Expr::Variable(port_vars[third_port].clone()),
                                        t: None,
                                    }),
                                ),
                                t: None,
                            }),
                        ),
                        t: None,
                    });
                }
            }
            Node::Equality(variables, _) => {
                // Equality nodes should only generate equations between stored variables
                if variables.len() > 1 {
                    let first_variable = &variables[0];
                    for variable in variables.iter().skip(1) {
                        expressions.push(TExpr {
                            v: Expr::Infix(
                                InfixOp::Equal,
                                Box::new(TExpr {
                                    v: Expr::Variable(first_variable.clone()),
                                    t: None,
                                }),
                                Box::new(TExpr {
                                    v: Expr::Variable(variable.clone()),
                                    t: None,
                                }),
                            ),
                            t: None,
                        });
                    }
                }
            }
            Node::AddConstant(value, p1, p2) => {
                expressions.push(TExpr {
                    v: Expr::Infix(
                        InfixOp::Equal,
                        Box::new(TExpr {
                            v: Expr::Variable(port_vars[p1].clone()),
                            t: None,
                        }),
                        Box::new(TExpr {
                            v: Expr::Infix(
                                InfixOp::Add,
                                Box::new(TExpr {
                                    v: Expr::Constant(value.clone()),
                                    t: None,
                                }),
                                Box::new(TExpr {
                                    v: Expr::Variable(port_vars[p2].clone()),
                                    t: None,
                                }),
                            ),
                            t: None,
                        }),
                    ),
                    t: None,
                });
            }
            Node::MultiplyConstant(value, p1, p2) => {
                expressions.push(TExpr {
                    v: Expr::Infix(
                        InfixOp::Equal,
                        Box::new(TExpr {
                            v: Expr::Variable(port_vars[p1].clone()),
                            t: None,
                        }),
                        Box::new(TExpr {
                            v: Expr::Infix(
                                InfixOp::Multiply,
                                Box::new(TExpr {
                                    v: Expr::Constant(value.clone()),
                                    t: None,
                                }),
                                Box::new(TExpr {
                                    v: Expr::Variable(port_vars[p2].clone()),
                                    t: None,
                                }),
                            ),
                            t: None,
                        }),
                    ),
                    t: None,
                });
            }
            Node::ExponentiateConstant(_, _, _) => {
                // Exponents should already be split into multiplications.
            }
            Node::Unrestricted(_) => {
                // Unrepresented as it doesn't map directly to an equation.
            }
            Node::Constant(value, p) => {
                expressions.push(TExpr {
                    v: Expr::Infix(
                        InfixOp::Equal,
                        Box::new(TExpr {
                            v: Expr::Variable(port_vars[p].clone()),
                            t: None,
                        }),
                        Box::new(TExpr {
                            v: Expr::Constant(value.clone()),
                            t: None,
                        }),
                    ),
                    t: None,
                });
            }
        }
    }

    expressions
}


// pub fn printEx() {
//     let mut diagram = StringDiagram::new();
    
//     // Example: Adding an Addition Node
//     diagram.add_node(1, Node::Addition(vec![
//         Port(2, 0), // Address 2, Index 0
//         Port(3, 1), // Address 3, Index 1
//         Port(4, 0), // Address 4, Index 0
//     ]));

//     // Example: Adding a MultiplyConstant Node
//     diagram.add_node(2, Node::MultiplyConstant(
//         BigInt::from(5),
//         Port(5, 0), // Address 5, Index 0
//         Port(6, 1), // Address 6, Index 1
//     ));
    
//     // ... add more nodes as needed

//     //println!("{diagram:#?}");
// }