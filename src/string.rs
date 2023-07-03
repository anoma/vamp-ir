use crate::ast::*;
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
    Contradiction, // Shouldn't exist in a coherent diagram
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
                Node::Contradiction => { }
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
                Node::Contradiction => { }
            }
        }
        false
    }

    pub fn replace_port(&mut self, target_port: &Port, new_port: &Port) {
        let Port(target_address, target_index) = target_port;
    
        if let Some(node) = self.nodes.get_mut(target_address) {
            match node {
                Node::Equality(_, ports) | Node::Addition(ports) | Node::Multiplication(ports) => {
                    if let Some(port) = ports.get_mut(*target_index) {
                        *port = new_port.clone();
                    }
                }
                Node::AddConstant(_, port1, port2)
                | Node::MultiplyConstant(_, port1, port2)
                | Node::ExponentiateConstant(_, port1, port2) => {
                    if *target_index == 0 {
                        *port1 = new_port.clone();
                    } else if *target_index == 1 {
                        *port2 = new_port.clone();
                    }
                }
                Node::Unrestricted(port) | Node::Constant(_, port) => {
                    if *target_index == 0 {
                        *port = new_port.clone();
                    }
                }
                Node::Contradiction => { }
            }
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
            Node::Contradiction => vec![],
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
                // Unrepresented as it doesn't map to an equation.
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
            Node::Contradiction => {
                expressions.push(TExpr {
                    v: Expr::Infix(
                        InfixOp::Equal,
                        Box::new(TExpr {
                            v: Expr::Constant(BigInt::from(0)),
                            t: None,
                        }),
                        Box::new(TExpr {
                            v: Expr::Constant(BigInt::from(1)),
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


pub fn split_addition_node(diagram: &mut StringDiagram, address: Address) {
    // Extract information about the node at the given address
    let (first_two_ports, mut remaining_ports) = if let Some(node) = diagram.nodes.get(&address) {
        if let Node::Addition(ports) = node {
            if ports.len() > 3 {
                (vec![ports[0].clone(), ports[1].clone()], ports[2..].to_vec())
            } else {
                return;
            }
        } else {
            return;
        }
    } else {
        return;
    };

    // Create a new addition node with the first two ports of the original node
    let new_node_ports = vec![first_two_ports[0].clone(), first_two_ports[1].clone(), Port(address, 0)];
    let new_address = diagram.add_node(Node::Addition(new_node_ports));

    // Replace the first two ports from the original node with a port pointing to the second arg of the new node.
    remaining_ports.insert(0, Port(new_address, 2));

    // Replace the node in the diagram with the updated node
    if let Some(node) = diagram.nodes.get_mut(&address) {
        if let Node::Addition(ports) = node {
            ports.clear();
            ports.extend_from_slice(&remaining_ports);
        }
    }

    // Update the output and first inpuit link to point at the new node
    diagram.replace_port(&first_two_ports[0], &Port(new_address, 0));
    diagram.replace_port(&first_two_ports[1], &Port(new_address, 1));

    // Update the port links for the remaining ports in the original node
    for (idx, port) in remaining_ports.iter().enumerate() {
        diagram.replace_port(port, &Port(address, idx));
    }
}

// Completely decompose addition node at address so it only has three arguments
pub fn decompose_addition_node(diagram: &mut StringDiagram, address: Address) {
    while let Some(Node::Addition(ports)) = diagram.nodes.get(&address) {
        if ports.len() > 3 {
            split_addition_node(diagram, address);
        } else {
            break; // Decomposition is complete
        }
    }
}

pub fn split_multiplication_node(diagram: &mut StringDiagram, address: Address) {
    // Extract information about the node at the given address
    let (first_two_ports, mut remaining_ports) = if let Some(node) = diagram.nodes.get(&address) {
        if let Node::Multiplication(ports) = node {
            if ports.len() > 3 {
                (vec![ports[0].clone(), ports[1].clone()], ports[2..].to_vec())
            } else {
                return;
            }
        } else {
            return;
        }
    } else {
        return;
    };

    // Create a new multiplication node with the first two ports of the original node
    let new_node_ports = vec![first_two_ports[0].clone(), first_two_ports[1].clone(), Port(address, 0)];
    let new_address = diagram.add_node(Node::Multiplication(new_node_ports));

    // Replace the first two ports from the original node with a port pointing to the second arg of the new node.
    remaining_ports.insert(0, Port(new_address, 2));

    // Replace the node in the diagram with the updated node
    if let Some(node) = diagram.nodes.get_mut(&address) {
        if let Node::Multiplication(ports) = node {
            ports.clear();
            ports.extend_from_slice(&remaining_ports);
        }
    }

    // Update the output and first inpuit link to point at the new node
    diagram.replace_port(&first_two_ports[0], &Port(new_address, 0));
    diagram.replace_port(&first_two_ports[1], &Port(new_address, 1));

    // Update the port links for the remaining ports in the original node
    for (idx, port) in remaining_ports.iter().enumerate() {
        diagram.replace_port(port, &Port(address, idx));
    }
}

// Completely decompose multiplication node at address so it only has three arguments
pub fn decompose_multiplication_node(diagram: &mut StringDiagram, address: Address) {
    while let Some(Node::Multiplication(ports)) = diagram.nodes.get(&address) {
        if ports.len() > 3 {
            split_multiplication_node(diagram, address);
        } else {
            break; // Decomposition is complete
        }
    }
}

pub fn split_exponentiation_node(diagram: &mut StringDiagram, address: Address) {
    // Get the data needed from the node at the address
    let (exp, p1, p2) = if let Some(Node::ExponentiateConstant(exp, p1, p2)) = diagram.nodes.get(&address) {
        (exp.clone(), p1.clone(), p2.clone())
    } else {
        return;
    };

    // Convert bigint into usize
    let exp_str = exp.to_str_radix(10);
    let exp_value = if let Ok(exp_value) = exp_str.parse::<usize>() {
        exp_value
    } else {
        // The exponentiation is too large to handle
        return;
    };

    // Create a new equality node with as many ports as the size of the exponent plus one.
    let mut equality_ports = Vec::new();
    equality_ports.push(p2.clone()); // The first port of the equality node should be the second port of the original exponentiation node
    for i in 0..exp_value {
        // Point the equality ports to the multiplication ports
        equality_ports.push(Port(address, i+1));
    }
    let equality_address = diagram.add_node(Node::Equality(vec![], equality_ports));

    // Create a new multiplication node
    let mut multiplication_ports = Vec::new();
    multiplication_ports.push(p1.clone()); // Keep the first argument the same
    for i in 0..exp_value {
        // Point the multiplication ports to the equality ports
        multiplication_ports.push(Port(equality_address, i+1));
    }

    // Replace the exponentiation node with the multiplication node
    diagram.nodes.insert(address, Node::Multiplication(multiplication_ports));

    // The second link of the original exponent node should now point to the first port of the new equality node.
    diagram.replace_port(&p2, &Port(equality_address, 0));
}

pub fn fuse_equality_nodes(diagram: &mut StringDiagram, prime_node_address: Address, port_index: PortIndex) {
    // Extract information about the first node
    let (second_node_address, second_node_port_index, mut first_node_vars, first_node_ports) = {
        if let Some(Node::Equality(vars, ports)) = diagram.nodes.get_mut(&prime_node_address) {
            if let Some(target_port) = ports.get(port_index) {
                (target_port.0, target_port.1, vars.clone(), ports.clone())
            } else {
                return; // Invalid port index
            }
        } else {
            return; // Not an equality node or doesn't exist
        }
    };

    // Extract information about the second node
    let (second_node_vars, second_node_ports) = {
        if let Some(Node::Equality(vars, ports)) = diagram.nodes.get_mut(&second_node_address) {
            (vars.clone(), ports.clone())
        } else {
            return; // Not an equality node or doesn't exist
        }
    };

    // Concatenate the variables from the second node into the first node
    first_node_vars.extend(second_node_vars);

    // Get the ports excluding the target port in the second equality node
    let mut new_ports: Vec<Port> = second_node_ports.iter()
        .enumerate()
        .filter(|(i, _)| *i != second_node_port_index)
        .map(|(_, port)| port.clone())
        .collect();

    // Remove the port at port_index in the first equality node
    new_ports.extend_from_slice(&first_node_ports[0..port_index]);
    new_ports.extend_from_slice(&first_node_ports[port_index + 1..]);

    // Update the first node in the diagram
    {
        if let Some(Node::Equality(vars, ports)) = diagram.nodes.get_mut(&prime_node_address) {
            *vars = first_node_vars;
            *ports = new_ports.clone();
        }
    }

    // Remove the second equality node from the diagram
    diagram.nodes.remove(&second_node_address);

    // Update the target ports in the diagram
    for (index, port) in new_ports.iter().enumerate() {
        diagram.replace_port(port, &Port(prime_node_address, index));
    }
}

pub fn fuse_addition_nodes(diagram: &mut StringDiagram, prime_node_address: Address, port_index: PortIndex) {
    // Extract information about the first node
    let (second_node_address, first_node_ports) = {
        if let Some(Node::Addition(ports)) = diagram.nodes.get_mut(&prime_node_address) {
            if let Some(target_port) = ports.get(port_index) {
                if target_port.1 == 0 {
                    (target_port.0, ports.clone())
                } else {
                    return; // The target port is not the 0th port
                }
            } else {
                return; // Invalid port index
            }
        } else {
            return; // Not an addition node or doesn't exist
        }
    };

    // Extract information about the second node
    let second_node_ports = {
        if let Some(Node::Addition(ports)) = diagram.nodes.get_mut(&second_node_address) {
            ports.clone()
        } else {
            return; // Not an addition node or doesn't exist
        }
    };

    // Get the ports excluding the 0th port in the second addition node
    let mut new_ports: Vec<Port> = second_node_ports.iter()
        .skip(1) // Skip the 0th port
        .cloned()
        .collect();

    // Add the ports from the first addition node (excluding the one at port_index)
    new_ports.extend_from_slice(&first_node_ports[0..port_index]);
    new_ports.extend_from_slice(&first_node_ports[port_index + 1..]);

    // Update the first node in the diagram
    {
        if let Some(Node::Addition(ports)) = diagram.nodes.get_mut(&prime_node_address) {
            *ports = new_ports.clone();
        }
    }

    // Remove the second addition node from the diagram
    diagram.nodes.remove(&second_node_address);

    // Update the target ports in the diagram
    for (index, port) in new_ports.iter().enumerate() {
        diagram.replace_port(port, &Port(prime_node_address, index));
    }
}

pub fn fuse_multiplication_nodes(diagram: &mut StringDiagram, prime_node_address: Address, port_index: PortIndex) {
    // Extract information about the first node
    let (second_node_address, first_node_ports) = {
        if let Some(Node::Multiplication(ports)) = diagram.nodes.get_mut(&prime_node_address) {
            if let Some(target_port) = ports.get(port_index) {
                if target_port.1 == 0 {
                    (target_port.0, ports.clone())
                } else {
                    return; // The target port is not the 0th port
                }
            } else {
                return; // Invalid port index
            }
        } else {
            return; // Not an multiplication node or doesn't exist
        }
    };

    // Extract information about the second node
    let second_node_ports = {
        if let Some(Node::Multiplication(ports)) = diagram.nodes.get_mut(&second_node_address) {
            ports.clone()
        } else {
            return; // Not an multiplication node or doesn't exist
        }
    };

    // Get the ports excluding the 0th port in the second multiplication node
    let mut new_ports: Vec<Port> = second_node_ports.iter()
        .skip(1) // Skip the 0th port
        .cloned()
        .collect();

    // Add the ports from the first multiplication node (excluding the one at port_index)
    new_ports.extend_from_slice(&first_node_ports[0..port_index]);
    new_ports.extend_from_slice(&first_node_ports[port_index + 1..]);

    // Update the first node in the diagram
    {
        if let Some(Node::Multiplication(ports)) = diagram.nodes.get_mut(&prime_node_address) {
            *ports = new_ports.clone();
        }
    }

    // Remove the second multiplication node from the diagram
    diagram.nodes.remove(&second_node_address);

    // Update the target ports in the diagram
    for (index, port) in new_ports.iter().enumerate() {
        diagram.replace_port(port, &Port(prime_node_address, index));
    }
}

pub fn constant_constant_simplification(diagram: &mut StringDiagram, address: Address) {
    if let Some(Node::Constant(value, target_port)) = diagram.nodes.get(&address) {
        let target_value;
        let target_address = target_port.0;
        // Check if the target port is also a constant node
        if let Some(Node::Constant(target_const, _)) = diagram.nodes.get(&target_address) {
            target_value = target_const.clone();
        } else {
            return; // Target node is not a constant
        }

        if value == &target_value {
            // If both constants are equal, remove both nodes
            diagram.nodes.remove(&address);
            diagram.nodes.remove(&target_address);
        } else {
            // If constants are not equal, remove the second constant node
            // and replace the first with Contradiction
            diagram.nodes.remove(&target_address);
            diagram.nodes.insert(address, Node::Contradiction);
        }
    }
}







pub fn simplify_string_diagram(diagram: &mut StringDiagram) {
    let mut changed = true;
    
    while changed {
        changed = false;
        
        // iterate through a snapshot of the nodes in the diagram
        let current_nodes = diagram.nodes.clone();
        for (prime_node_address, node) in current_nodes.iter() {
            match node {
                Node::Equality(_, ports) => {
                    for (port_index, target_port) in ports.iter().enumerate() {
                        if let Some(Node::Equality(_, _)) = diagram.nodes.get(&target_port.0) {
                            // Fuse equality nodes
                            fuse_equality_nodes(diagram, *prime_node_address, port_index);
                            changed = true;
                            break;
                        }
                    }
                },
                Node::Addition(ports) => {
                    for (port_index, target_port) in ports.iter().enumerate() {
                        if port_index > 0 {
                            if let Some(Node::Addition(_)) = diagram.nodes.get(&target_port.0) {
                                if target_port.1 == 0 {
                                    // Fuse addition nodes
                                    fuse_addition_nodes(diagram, *prime_node_address, port_index);
                                    changed = true;
                                    break;
                                }
                            }
                        }
                    }
                },
                Node::Multiplication(ports) => {
                    for (port_index, target_port) in ports.iter().enumerate() {
                        if port_index > 0 {
                            if let Some(Node::Multiplication(_)) = diagram.nodes.get(&target_port.0) {
                                if target_port.1 == 0 {
                                    // Fuse multiplication nodes
                                    fuse_multiplication_nodes(diagram, *prime_node_address, port_index);
                                    changed = true;
                                    break;
                                }
                            }
                        }
                    }
                },
                Node::Constant(_, port) => {
                    if let Some(Node::Constant(_, _)) = diagram.nodes.get(&port.0) {
                        constant_constant_simplification(diagram, *prime_node_address);
                        changed = true;
                        break;
                    }
                }
                _ => {}
            }
            
            // If there was a change, break from the inner loop to restart the outer loop
            if changed {
                break;
            }
        }
    }
}

// Ensure there aren't any additions/multiplications with more than 2 inputs
pub fn prep_for_3ac(diagram: &mut StringDiagram) {
    let mut changed = true;

    while changed {
        changed = false;

        // iterate through a snapshot of the nodes in the diagram
        let current_nodes = diagram.nodes.clone();
        for (address, node) in current_nodes.iter() {
            match node {
                Node::Addition(ports) => {
                    // If there are more than three ports in the addition node, decompose it
                    if ports.len() > 3 {
                        decompose_addition_node(diagram, *address);
                        changed = true;
                        break;
                    }
                },
                Node::Multiplication(ports) => {
                    // If there are more than three ports in the multiplication node, decompose it
                    if ports.len() > 3 {
                        decompose_multiplication_node(diagram, *address);
                        changed = true;
                        break;
                    }
                },
                _ => {}
            }
        }
    }
}

pub fn simplify_3ac(equations: Vec<TExpr>, input_ids: &HashSet<u32>) -> Vec<TExpr> {
    let mut diag = build_string_diagram(equations, input_ids);
    simplify_string_diagram(&mut diag);
    prep_for_3ac(&mut diag);
    convert_to_3ac(&diag)
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_translate_3ac_to_diagram() {
        let test_expr = vec![
            TExpr { v: Expr::Infix(InfixOp::Equal, 
                Box::new(TExpr { v: Expr::Variable(Variable { name: None, id: 56 }), t: None }), 
                Box::new(TExpr { v: Expr::Infix(InfixOp::Add, 
                    Box::new(TExpr { v: Expr::Variable(Variable { name: Some("x".to_string()), id: 11 }), t: None }), 
                    Box::new(TExpr { v: Expr::Constant(BigInt::from(1)), t: None })), t: None })), t: None }, 
            TExpr { v: Expr::Infix(InfixOp::Equal, 
                Box::new(TExpr { v: Expr::Variable(Variable { name: None, id: 56 }), t: None }), 
                Box::new(TExpr { v: Expr::Constant(BigInt::from(3345387)), t: None })), t: None }, 
            TExpr { v: Expr::Infix(InfixOp::Equal, Box::new(TExpr { v: Expr::Variable(Variable { name: Some("c".to_string()), id: 12 }), t: None }), 
            Box::new(TExpr { v: Expr::Constant(BigInt::from(-32)), t: None })), t: None }, 
            TExpr { v: Expr::Infix(InfixOp::Equal, Box::new(TExpr { v: Expr::Variable(Variable { name: Some("f".to_string()), id: 13 }), t: None }), 
            Box::new(TExpr { v: Expr::Constant(BigInt::from(-68)), t: None })), t: None }, 
            TExpr { v: Expr::Infix(InfixOp::Equal, Box::new(TExpr { v: Expr::Variable(Variable { name: Some("r".to_string()), id: 14 }), t: None }), 
            Box::new(TExpr { v: Expr::Variable(Variable { name: None, id: 58 }), t: None })), t: None }];

        let mut input_ids = HashSet::new();
        input_ids.insert(11);
        input_ids.insert(12);
        input_ids.insert(13);
        input_ids.insert(14);

        let str_diag = build_string_diagram(test_expr, &input_ids);

        assert!(str_diag.is_well_formed(), "Test that the translated diagram is well formed");
    }

    #[test]
    fn test_fuse_equality_nodes_basic() {
        // Create a simple diagram with two equality nodes and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 =  diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i2 =  diagram.add_node(Node::Unrestricted(Port(4, 2)));
        let i3 =  diagram.add_node(Node::Unrestricted(Port(5, 0)));
        let i4 =  diagram.add_node(Node::Unrestricted(Port(5, 2)));
        let addr1 = diagram.add_node(Node::Equality(vec![Variable::new(1)], vec![Port(i1, 0), Port(5, 1), Port(i2, 0)]));
        let addr2 = diagram.add_node(Node::Equality(vec![Variable::new(2)], vec![Port(i3, 0), Port(4, 1), Port(i4, 0)]));

        assert!(diagram.is_well_formed(), "Check that the starting diagram makes sense");

        let node_count = diagram.nodes.len();

        // Fuse the nodes together
        fuse_equality_nodes(&mut diagram, addr1, 1);

        assert!(diagram.is_well_formed(), "Check that the modified diagram still makes sense");

        assert!(diagram.nodes.get(&addr2).is_none(), "Check that the second node is gone");
        assert_eq!(node_count, diagram.nodes.len()+1, "There should be one less node after fusion");

        if let Some(Node::Equality(vars, ports)) = diagram.nodes.get(&addr1) {
            assert_eq!(vars.len(), 2, "Check that the first node has updated variables and ports");

            assert_eq!(ports.len(), 4, "The total ports in the remaining node should be two less than the sum of the original");
        } else {
            panic!("Expected Equality node!");
        }
    }

    #[test]
    fn test_fuse_addition_nodes_basic() {
        // Create a simple diagram with two equality nodes and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 =  diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i2 =  diagram.add_node(Node::Unrestricted(Port(4, 2)));
        let i4 =  diagram.add_node(Node::Unrestricted(Port(5, 1)));
        let i5 =  diagram.add_node(Node::Unrestricted(Port(5, 2)));
        let addr1 = diagram.add_node(Node::Addition(vec![Port(i1, 0), Port(5, 0), Port(i2, 0)]));
        let addr2 = diagram.add_node(Node::Addition(vec![Port(addr1, 1), Port(i4, 0), Port(i5, 0)]));

        assert!(diagram.is_well_formed(), "Check that the starting diagram makes sense");

        let node_count = diagram.nodes.len();

        // Fuse the nodes together
        fuse_addition_nodes(&mut diagram, addr1, 1);

        assert!(diagram.is_well_formed(), "Check that the modified diagram still makes sense");

        assert!(diagram.nodes.get(&addr2).is_none(), "Check that the second node is gone");
        assert_eq!(node_count, diagram.nodes.len()+1, "There should be one less node after fusion");

        if let Some(Node::Addition(ports)) = diagram.nodes.get(&addr1) {
            assert_eq!(ports.len(), 4, "The total ports in the remaining node should be two less than the sum of the original");
        } else {
            panic!("Expected Equality node!");
        }
    }

    #[test]
    fn test_fuse_multiplication_nodes_basic() {
        // Create a simple diagram with two equality nodes and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 =  diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i2 =  diagram.add_node(Node::Unrestricted(Port(4, 2)));
        let i4 =  diagram.add_node(Node::Unrestricted(Port(5, 1)));
        let i5 =  diagram.add_node(Node::Unrestricted(Port(5, 2)));
        let addr1 = diagram.add_node(Node::Multiplication(vec![Port(i1, 0), Port(5, 0), Port(i2, 0)]));
        let addr2 = diagram.add_node(Node::Multiplication(vec![Port(addr1, 1), Port(i4, 0), Port(i5, 0)]));

        assert!(diagram.is_well_formed(), "Check that the starting diagram makes sense");

        let node_count = diagram.nodes.len();

        // Fuse the nodes together
        fuse_multiplication_nodes(&mut diagram, addr1, 1);

        assert!(diagram.is_well_formed(), "Check that the modified diagram still makes sense");

        assert!(diagram.nodes.get(&addr2).is_none(), "Check that the second node is gone");
        assert_eq!(node_count, diagram.nodes.len()+1, "There should be one less node after fusion");

        if let Some(Node::Multiplication(ports)) = diagram.nodes.get(&addr1) {
            assert_eq!(ports.len(), 4, "The total ports in the remaining node should be two less than the sum of the original");
        } else {
            panic!("Expected Equality node!");
        }
    }

    #[test]
    fn test_split_addition_nodes_basic() {
        // Create a simple diagram with a single addition node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 =  diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i2 =  diagram.add_node(Node::Unrestricted(Port(4, 1)));
        let i3 =  diagram.add_node(Node::Unrestricted(Port(4, 2)));
        let i4 =  diagram.add_node(Node::Unrestricted(Port(4, 3)));
        let addr1 = diagram.add_node(Node::Addition(vec![Port(i1, 0), Port(i2, 0), Port(i3, 0), Port(i4, 0)]));
        
        assert!(diagram.is_well_formed(), "Check that the starting diagram makes sense");

        let node_count = diagram.nodes.len();

        // Fuse the nodes together
        split_addition_node(&mut diagram, addr1);

        assert!(diagram.is_well_formed(), "Check that the modified diagram still makes sense");

        // Assert that the original node is modified
        let new_address = addr1 + 1;
        if let Some(Node::Addition(ports)) = diagram.nodes.get(&addr1) {
            assert_eq!(ports.len(), 3, "Modified node should only have three ports");
            assert_eq!(ports[0], Port(new_address, 2), "Modified node head should point to the new node");
        } else {
            panic!("Node not properly modified");
        }

        // Assert that a new node has been added
        if let Some(Node::Addition(ports)) = diagram.nodes.get(&new_address) {
            assert_eq!(ports.len(), 3, "New node should only have three ports");
            assert_eq!(ports[0], Port(i1, 0), "New node should have same head as original node");
        } else {
            panic!("New node not properly formed");
        }

        assert_eq!(node_count + 1, diagram.nodes.len(), "There should be one more node after splitting");
    }

    #[test]
    fn test_split_multiplication_nodes_basic() {
        // Create a simple diagram with a single multiplication node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 =  diagram.add_node(Node::Unrestricted(Port(4, 0)));
        let i2 =  diagram.add_node(Node::Unrestricted(Port(4, 1)));
        let i3 =  diagram.add_node(Node::Unrestricted(Port(4, 2)));
        let i4 =  diagram.add_node(Node::Unrestricted(Port(4, 3)));
        let addr1 = diagram.add_node(Node::Multiplication(vec![Port(i1, 0), Port(i2, 0), Port(i3, 0), Port(i4, 0)]));
        
        assert!(diagram.is_well_formed(), "Check that the starting diagram makes sense");

        let node_count = diagram.nodes.len();

        // Fuse the nodes together
        split_multiplication_node(&mut diagram, addr1);

        assert!(diagram.is_well_formed(), "Check that the modified diagram still makes sense");

        // Assert that the original node is modified
        let new_address = addr1 + 1;
        if let Some(Node::Multiplication(ports)) = diagram.nodes.get(&addr1) {
            assert_eq!(ports.len(), 3, "Modified node should only have three ports");
            assert_eq!(ports[0], Port(new_address, 2), "Modified node head should point to the new node");
        } else {
            panic!("Node not properly modified");
        }

        // Assert that a new node has been added
        if let Some(Node::Multiplication(ports)) = diagram.nodes.get(&new_address) {
            assert_eq!(ports.len(), 3, "New node should only have three ports");
            assert_eq!(ports[0], Port(i1, 0), "New node should have same head as original node");
        } else {
            panic!("New node not properly formed");
        }

        assert_eq!(node_count + 1, diagram.nodes.len(), "There should be one more node after splitting");
    }

    #[test]
    fn test_constant_constant_eq() {
        // Create a simple diagram with a single multiplication node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 =  diagram.add_node(Node::Unrestricted(Port(1, 0)));
        diagram.add_node(Node::Unrestricted(Port(i1, 0)));
        let i3 =  diagram.add_node(Node::Constant(BigInt::from(55), Port(3, 0)));
        let i4 =  diagram.add_node(Node::Constant(BigInt::from(55), Port(i3, 0)));

        assert!(diagram.is_well_formed(), "Check that the starting diagram makes sense");

        let node_count = diagram.nodes.len();

        // Fuse the nodes together
        constant_constant_simplification(&mut diagram, i3);

        assert!(diagram.is_well_formed(), "Check that the modified diagram still makes sense");

        assert!(diagram.nodes.get(&i3).is_none(), "Check that the first node is gone");
        assert!(diagram.nodes.get(&i4).is_none(), "Check that the second node is gone");

        assert_eq!(diagram.nodes.len() + 2, node_count, "Nodecount should have decreased by 2");
    }

    #[test]
    fn test_constant_constant_neq() {
        // Create a simple diagram with a single multiplication node and a few ancillary nodes.
        let mut diagram = StringDiagram::new();
        let i1 =  diagram.add_node(Node::Unrestricted(Port(1, 0)));
        diagram.add_node(Node::Unrestricted(Port(i1, 0)));
        let i3 =  diagram.add_node(Node::Constant(BigInt::from(56), Port(3, 0)));
        let i4 =  diagram.add_node(Node::Constant(BigInt::from(55), Port(i3, 0)));

        assert!(diagram.is_well_formed(), "Check that the starting diagram makes sense");

        let node_count = diagram.nodes.len();

        // Fuse the nodes together
        constant_constant_simplification(&mut diagram, i3);

        assert!(diagram.is_well_formed(), "Check that the modified diagram still makes sense");

        assert!(diagram.nodes.get(&i4).is_none(), "Check that the second node is gone");


        if let Some(Node::Contradiction) = diagram.nodes.get(&i3) {
            
        } else {
            panic!("Old node should be contradiction");
        }

        assert_eq!(diagram.nodes.len() + 1, node_count, "Nodecount should have decreased by 1");
    }
}