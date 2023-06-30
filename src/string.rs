use crate::ast::*;
use crate::error::Error;
use crate::transform::{collect_module_variables, compile, FieldOps};
use std::collections::{HashMap, HashSet};
use num_bigint::BigInt;

// Define the address type
pub type Address = usize;
pub type PortIndex = usize;

// Define a port as a pair of node address and a list index
#[derive(Clone, Debug)]
pub struct Port(Address, PortIndex);

// Define different types of nodes
#[derive(Clone, Debug)]
pub enum Node {
    Addition(Vec<Port>), // The first port is the sum of the remaining ports
    Multiplication(Vec<Port>), // The first port is the product of the remaining ports
    Equality(Option<HashSet<String>>, Vec<Port>), // All ports are equal
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
}

fn get_or_create_equality_node(variable: &Variable, pointing_port: Port, diagram: &mut StringDiagram, variable_addresses: &mut HashMap<VariableId, Address>) -> (Address, usize) {
    // Does this variable already have an address? If not, make one.
    let equality_address = *variable_addresses.entry(variable.id).or_insert_with(|| {
        // If we're looking at a named variable, store that name.
        let mut name_set = None;
        if let Some(name) = &variable.name {
            let mut set = HashSet::new();
            set.insert(name.clone());
            name_set = Some(set);
        }
        //println!("EE: Adding {:?} at {}", Node::Equality(name_set.clone(), Vec::new()), diagram.next_address);
        diagram.add_node(Node::Equality(name_set, Vec::new()))
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

pub fn build_string_diagram(equations: Vec<Expr>) -> StringDiagram {
    let mut diagram = StringDiagram::new();
    let mut variable_addresses: HashMap<VariableId, Address> = HashMap::new();

    for eq in equations {
        //println!("Equation {eq:?}");
        if let Expr::Infix(InfixOp::Equal, left, right) = eq {
            match (&left.v, &right.v) {
                (Expr::Variable(var), Expr::Constant(c)) => {
                    // TODO: There's probably a better way to do this.
                    let equality_address = variable_addresses.get(&var.id).cloned();
                    let was_newly_created = equality_address.is_none();
                    
                    let next_addr = diagram.next_address;
                    let target_address = if was_newly_created { next_addr + 1 } else { next_addr };
                    
                    let (equality_address, new_port_index) = get_or_create_equality_node(&var, Port(target_address, 0), &mut diagram, &mut variable_addresses);

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

                    let (equality_address1, new_port_index1) = get_or_create_equality_node(&var1, Port(target_address, 0), &mut diagram, &mut variable_addresses);
                    let (equality_address2, new_port_index2) = get_or_create_equality_node(&var2, Port(target_address, 1), &mut diagram, &mut variable_addresses);

                    // Connect these nodes by adding an equality node that points to both.
                    //println!("1: Adding {:?} at {}", Node::Equality(None, vec![Port(equality_address1, new_port_index1), Port(equality_address2, new_port_index2)]), diagram.next_address);
                    diagram.add_node(Node::Equality(None, vec![Port(equality_address1, new_port_index1), Port(equality_address2, new_port_index2)]));
                },
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
    
                        let (equality_address1, new_port_index1) = get_or_create_equality_node(&var1, Port(target_address, 0), &mut diagram, &mut variable_addresses);
                        let (equality_address2, new_port_index2) = get_or_create_equality_node(&var2, Port(target_address, 1), &mut diagram, &mut variable_addresses);
    
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
                                let (address0, port_index0) = get_or_create_equality_node(&var, Port(target_address, 1), &mut diagram, &mut variable_addresses);
                                let (address1, port_index1) = get_or_create_equality_node(term_var1, Port(target_address, 0), &mut diagram, &mut variable_addresses);
                                let (address2, port_index2) = get_or_create_equality_node(term_var2, Port(target_address, 2), &mut diagram, &mut variable_addresses);
                                ports = vec![Port(address1, port_index1), Port(address0, port_index0), Port(address2, port_index2)];
                            } else {
                                let (address0, port_index0) = get_or_create_equality_node(&var, Port(target_address, 0), &mut diagram, &mut variable_addresses);
                                let (address1, port_index1) = get_or_create_equality_node(term_var1, Port(target_address, 1), &mut diagram, &mut variable_addresses);
                                let (address2, port_index2) = get_or_create_equality_node(term_var2, Port(target_address, 2), &mut diagram, &mut variable_addresses);
                                ports = vec![Port(address0, port_index0), Port(address1, port_index1), Port(address2, port_index2)];
                            }
                        },
                        (Expr::Constant(const1), Expr::Variable(term_var2)) => {
                            if !variable_addresses.contains_key(&term_var2.id) {
                                new_var_ids.insert(term_var2.id);
                            }

                            target_address = diagram.next_address + new_var_ids.len()+1;

                            if matches!(op, InfixOp::Subtract) || matches!(op, InfixOp::Divide) {
                                let (address0, port_index0) = get_or_create_equality_node(&var, Port(target_address, 1), &mut diagram, &mut variable_addresses);
                                //println!("5: Adding {:?} at {}", Node::Constant(const1.clone(), Port(target_address, 0)), diagram.next_address);
                                let address1 = diagram.add_node(Node::Constant(const1.clone(), Port(target_address, 0)));
                                let (address2, port_index2) = get_or_create_equality_node(term_var2, Port(target_address, 2), &mut diagram, &mut variable_addresses);
                                ports = vec![Port(address1, 0), Port(address0, port_index0), Port(address2, port_index2)];
                            } else {
                                let (address0, port_index0) = get_or_create_equality_node(&var, Port(target_address, 0), &mut diagram, &mut variable_addresses);
                                //println!("6: Adding {:?} at {}", Node::Constant(const1.clone(), Port(target_address, 1)), diagram.next_address);
                                let address1 = diagram.add_node(Node::Constant(const1.clone(), Port(target_address, 1)));
                                let (address2, port_index2) = get_or_create_equality_node(term_var2, Port(target_address, 2), &mut diagram, &mut variable_addresses);
                                ports = vec![Port(address0, port_index0), Port(address1, 0), Port(address2, port_index2)];
                            }

                        },
                        (Expr::Variable(term_var1), Expr::Constant(const2)) => {
                            if !variable_addresses.contains_key(&term_var1.id) {
                                new_var_ids.insert(term_var1.id);
                            }

                            target_address = diagram.next_address + new_var_ids.len()+1;

                            if matches!(op, InfixOp::Subtract) || matches!(op, InfixOp::Divide) {
                                let (address0, port_index0) = get_or_create_equality_node(&var, Port(target_address, 1), &mut diagram, &mut variable_addresses);
                                let (address1, port_index1) = get_or_create_equality_node(term_var1, Port(target_address, 0), &mut diagram, &mut variable_addresses);
                                //println!("7: Adding {:?} at {}", Node::Constant(const2.clone(), Port(target_address, 2)), diagram.next_address);
                                let address2 = diagram.add_node(Node::Constant(const2.clone(), Port(target_address, 2)));
                                ports = vec![Port(address1, port_index1), Port(address0, port_index0), Port(address2, 0)];
                            } else {
                                let (address0, port_index0) = get_or_create_equality_node(&var, Port(target_address, 0), &mut diagram, &mut variable_addresses);
                                let (address1, port_index1) = get_or_create_equality_node(term_var1, Port(target_address, 1), &mut diagram, &mut variable_addresses);
                                //println!("8: Adding {:?} at {}", Node::Constant(const2.clone(), Port(target_address, 2)), diagram.next_address);
                                let address2 = diagram.add_node(Node::Constant(const2.clone(), Port(target_address, 2)));
                                ports = vec![Port(address0, port_index0), Port(address1, port_index1), Port(address2, 0)];
                            }

                        },
                        (Expr::Constant(const1), Expr::Constant(const2)) => {
                            target_address = diagram.next_address + new_var_ids.len()+2;

                            if matches!(op, InfixOp::Subtract) || matches!(op, InfixOp::Divide) {
                                let (address0, port_index0) = get_or_create_equality_node(&var, Port(target_address, 1), &mut diagram, &mut variable_addresses);
                                //println!("9: Adding {:?} at {}", Node::Constant(const1.clone(), Port(target_address, 0)), diagram.next_address);
                                let address1 = diagram.add_node(Node::Constant(const1.clone(), Port(target_address, 0)));
                                //println!("10: Adding {:?} at {}", Node::Constant(const2.clone(), Port(target_address, 2)), diagram.next_address);
                                let address2 = diagram.add_node(Node::Constant(const2.clone(), Port(target_address, 2)));
                                ports = vec![Port(address1, 0), Port(address0, port_index0), Port(address2, 0)];
                            } else {
                                let (address0, port_index0) = get_or_create_equality_node(&var, Port(target_address, 0), &mut diagram, &mut variable_addresses);
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