// TODO:
// Definition resynthesis
// Addition-constmul reorientation
// Enum-driven reverse rewriting
// Distributivity-based greedy searching
// Constant op-equality distributivity
// Constant multiplication-addition orientation
// Constant multiplication-addition distributivity
// Efficient exponentiation unfolding
// Constant exponentiation-multiplication distributivity
// Equality self-link deletion
// Addition self-link unrestriction
// Multiplication self-link unrestriction
// Addition-equality link fusion
// Multiplication-equality link fusion
// ???Equality-checking based fusion???
// Implement factoring in a non-local way (detect polynomials => translate string to poly => factor poly => translate back)

use crate::ast::*;
use crate::transform::FieldOps;
use num_bigint::BigInt;
use std::collections::{HashMap, HashSet};

// Define the address type
pub type Address = usize;
pub type PortIndex = usize;

// Define a port as a pair of node address and a list index
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Port(pub Address, pub PortIndex);

// Define different types of nodes
#[derive(Clone, Debug)]
pub enum Node {
    Addition(Vec<Port>),       // The first port is the sum of the remaining ports
    Multiplication(Vec<Port>), // The first port is the product of the remaining ports
    Equality(Vec<Variable>, Vec<Port>), // All ports are equal
    AddConstant(BigInt, Port, Port), // First port equals the constant plus the second port
    MultiplyConstant(BigInt, Port, Port), // First port equals the constant times the second port
    ExponentiateConstant(BigInt, Port, Port), // First port equals the second port to the power of the constant
    Unrestricted(Port),                       // The port is an unrestricted natural number
    Constant(BigInt, Port),                   // The port is equal to the constant
    Contradiction,                            // Shouldn't exist in a coherent diagram

    CVVAdd(BigInt, Port, Port), // First port plus the second equals the constant (Only used during 3ac translation)
    CVVMul(BigInt, Port, Port), // First port times the second equals the constant (Only used during 3ac translation)
}

// Define the String Diagram
#[derive(Clone, Debug)]
pub struct StringDiagram {
    pub nodes: HashMap<Address, Node>,
    pub next_address: Address,
}

impl StringDiagram {
    pub fn new() -> Self {
        StringDiagram {
            nodes: HashMap::new(),
            next_address: 0,
        }
    }

    pub fn add_node(&mut self, node: Node) -> Address {
        let address = self.next_address;
        self.nodes.insert(address, node);
        self.next_address += 1;
        address
    }

    // Check if the diagram is well-formed, i.e., every port is connected to a port that points back to itself.
    pub fn is_well_formed(&self) -> bool {
        for (address, node) in &self.nodes {
            match node {
                Node::Addition(ports) | Node::Multiplication(ports) | Node::Equality(_, ports) => {
                    for (port_index, port) in ports.iter().enumerate() {
                        if !self.is_connected_back(port, (*address, port_index)) {
                            return false;
                        }
                    }
                }
                Node::AddConstant(_, port1, port2)
                | Node::MultiplyConstant(_, port1, port2)
                | Node::ExponentiateConstant(_, port1, port2)
                | Node::CVVAdd(_, port1, port2)
                | Node::CVVMul(_, port1, port2) => {
                    if !self.is_connected_back(port1, (*address, 0))
                        || !self.is_connected_back(port2, (*address, 1))
                    {
                        return false;
                    }
                }
                Node::Unrestricted(port) | Node::Constant(_, port) => {
                    if !self.is_connected_back(port, (*address, 0)) {
                        return false;
                    }
                }
                Node::Contradiction => {}
            }
        }
        true
    }

    // Helper function to check if the port is connected back to the original (address, port_index).
    fn is_connected_back(&self, port: &Port, original: (Address, PortIndex)) -> bool {
        let Port(target_address, target_port_index) = port;
        if target_address == &original.0 && target_port_index == &original.1 {
            return false;
        }
        if let Some(target_node) = self.nodes.get(target_address) {
            match target_node {
                Node::Addition(ports) | Node::Multiplication(ports) | Node::Equality(_, ports) => {
                    if let Some(Port(back_address, back_port_index)) = ports.get(*target_port_index)
                    {
                        return *back_address == original.0 && *back_port_index == original.1;
                    }
                }
                Node::AddConstant(_, port1, port2)
                | Node::MultiplyConstant(_, port1, port2)
                | Node::ExponentiateConstant(_, port1, port2)
                | Node::CVVAdd(_, port1, port2)
                | Node::CVVMul(_, port1, port2) => {
                    let back_port = if *target_port_index == 0 {
                        port1
                    } else {
                        port2
                    };
                    return back_port.0 == original.0 && back_port.1 == original.1;
                }
                Node::Unrestricted(port) | Node::Constant(_, port) => {
                    return port.0 == original.0 && port.1 == original.1;
                }
                Node::Contradiction => {}
            }
        }
        false
    }

    // Replace the port *at* target_port *with* new_port.
    fn replace_port(&mut self, target_port: &Port, new_port: &Port) {
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
                | Node::ExponentiateConstant(_, port1, port2)
                | Node::CVVAdd(_, port1, port2)
                | Node::CVVMul(_, port1, port2) => {
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
                Node::Contradiction => {}
            }
        }
    }

    // Get all the ports of an address.
    fn port_list(&self, target_address: &Address) -> Vec<Port> {
        if let Some(node) = self.nodes.get(target_address) {
            match node {
                Node::Equality(_, ports) | Node::Addition(ports) | Node::Multiplication(ports) => {
                    ports.to_vec()
                }
                Node::AddConstant(_, port1, port2)
                | Node::MultiplyConstant(_, port1, port2)
                | Node::ExponentiateConstant(_, port1, port2)
                | Node::CVVAdd(_, port1, port2)
                | Node::CVVMul(_, port1, port2) => {
                    vec![port1.clone(), port2.clone()]
                }
                Node::Unrestricted(port) | Node::Constant(_, port) => vec![port.clone()],
                Node::Contradiction => vec![],
            }
        } else {
            vec![]
        }
    }

    // Get all the addresses linked to an address.
    fn address_list(&self, target_address: &Address) -> Vec<Address> {
        return self.port_list(target_address).iter().map(|p| p.0).collect();
    }
}

// Used to assist in synthesizing new definitions.
pub struct DefinitionRegistry {
    pub initial_defs: Vec<Definition>,
    pub port_id_map: HashMap<Port, u32>,
    pub id_def_map: HashMap<u32, (HashSet<Port>, Box<TExpr>)>,
    //pub init_ids: HashSet<u32>, // Not sure if needed/useful
    pub next_id: u32,
}

impl DefinitionRegistry {
    fn port_def_map(&self, port: &Port) -> Option<&Box<TExpr>> {
        if let Some(id) = self.port_id_map.get(port) {
            return self.id_def_map.get(id).map(|(_, def)| def);
        } else {
            None
        }
    }

    pub fn new(defs: Vec<Definition>) -> Self {
        //let mut init_ids = HashSet::new();

        // Extract the highest variable id from the given definitions and store encountered ids into init_ids
        let next_id = defs
            .iter()
            .filter_map(|def| match def {
                Definition(LetBinding(
                    TPat {
                        v: Pat::Variable(Variable { id, .. }),
                        ..
                    },
                    _,
                )) => {
                    //init_ids.insert(*id);
                    Some(id)
                }
                _ => None,
            })
            .max()
            .unwrap_or(&0)
            + 1;

        DefinitionRegistry {
            initial_defs: defs,
            port_id_map: HashMap::new(),
            id_def_map: HashMap::new(),
            //init_ids,
            next_id,
        }
    }

    // Destructively empty synthesized definitions into original definition vector
    fn compile_definitions(&mut self) -> Vec<Definition> {
        let mut compiled_defs = std::mem::take(&mut self.initial_defs);

        for (id, (_, expr)) in self.id_def_map.drain() {
            let pattern = TPat {
                v: Pat::Variable(Variable { name: None, id }),
                t: None, // Adjust as needed based on your actual type information
            };
            let let_binding = LetBinding(pattern, expr);
            compiled_defs.push(Definition(let_binding));
        }

        self.id_def_map.clear();

        compiled_defs
    }
    fn register_definition(&mut self, ports: (Port, Port), definition: Expr) {
        // Insert the ports into the port_id_map
        self.port_id_map.insert(ports.0.clone(), self.next_id);
        self.port_id_map.insert(ports.1.clone(), self.next_id);

        // Insert the definition into the id_def_map
        let ports_set = vec![ports.0.clone(), ports.1]
            .into_iter()
            .collect::<HashSet<_>>();
        let boxed_def = Box::new(TExpr {
            v: definition,
            t: None,
        });
        self.id_def_map.insert(self.next_id, (ports_set, boxed_def));

        // Increment the next_id
        self.next_id += 1;
    }

    fn replace_definition_of_port(&mut self, target_port: &Port, input_expr: Expr) -> Option<u32> {
        // Fetch the id associated with the port
        if let Some(&id) = self.port_id_map.get(target_port) {
            // Wrap the input expression inside a TExpr
            let texpr = TExpr {
                v: input_expr,
                t: None, // no type information
            };

            // Update the definition associated with the id
            if let Some((_, def)) = self.id_def_map.get_mut(&id) {
                *def = Box::new(texpr);
            }

            return Some(id);
        }

        None // Return None if the port wasn't found in the map
    }

    pub fn remove_ports(&mut self, ports: (&Port, &Port)) {
        // Check if the first port exists in the port_id_map
        if let Some(var_id) = self.port_id_map.get(ports.0) {
            // Clone the variable ID since we might need it after removing the port from port_id_map
            let var_id = *var_id;

            // Remove the ports from the port_id_map
            self.port_id_map.remove(ports.0);
            self.port_id_map.remove(ports.1);

            // Access the port set in id_def_map for the variable ID
            if let Some((port_set, _)) = self.id_def_map.get_mut(&var_id) {
                // Remove the ports
                port_set.remove(ports.0);
                port_set.remove(ports.1);

                // If the port set is now empty, remove the variable ID and its associated definition
                if port_set.is_empty() {
                    self.id_def_map.remove(&var_id);
                }
            }
        }
    }
}

fn all_ports_registered(diagram: &StringDiagram, registry: &DefinitionRegistry) -> bool {
    for address in diagram.nodes.keys() {
        for port in diagram.port_list(address) {
            if !registry.port_id_map.contains_key(&port) {
                println!("{:?}", diagram.nodes.get(address));
                //println!("{:?}", diagram.nodes.get(diagram.nodes.get(address).unwrap().ports[0].0));
                return false;
            }
        }
    }
    true
}

fn get_or_create_equality_node(
    variable: &Variable,
    pointing_port: Port,
    diagram: &mut StringDiagram,
    variable_addresses: &mut HashMap<VariableId, Address>,
    input_ids: &HashSet<u32>,
    defs: &mut DefinitionRegistry,
) -> (Address, usize) {
    // Does this variable already have an address? If not, make one.
    let equality_address = *variable_addresses.entry(variable.id).or_insert_with(|| {
        // If we're looking at an input variable, store it.
        let input_vec = if input_ids.contains(&variable.id) {
            vec![variable.clone()]
        } else {
            vec![]
        };

        let address = diagram.add_node(Node::Equality(input_vec, Vec::new()));

        // If variable is new, add a default entry to defs.
        defs.id_def_map.entry(variable.id).or_insert_with(|| {
            let var_expr = TExpr {
                v: Expr::Variable(variable.clone()),
                t: None,
            };
            (HashSet::new(), Box::new(var_expr))
        });

        address
    });

    // Find the largest index in that equality node. Place the pointing port there.
    let next_idx;
    if let Node::Equality(_, ports) = diagram.nodes.get_mut(&equality_address).unwrap() {
        next_idx = ports.len();
        ports.push(pointing_port.clone());
    } else {
        panic!("Expected an equality node");
    }

    // Update port_id_map and id_def_map in defs.
    defs.port_id_map.insert(pointing_port.clone(), variable.id);
    defs.port_id_map
        .insert(Port(equality_address, next_idx), variable.id);
    if let Some((port_set, _)) = defs.id_def_map.get_mut(&variable.id) {
        port_set.insert(pointing_port);
        port_set.insert(Port(equality_address, next_idx));
    }

    (equality_address, next_idx)
}

// Rename port-associated variables so that original definitions remain untouched
fn preserve_initial_defs(defs: &mut DefinitionRegistry) {
    // Create a mapping from old IDs to new IDs
    let mut old_to_new_ids = HashMap::new();

    // Generate new IDs for existing entries and store in old_to_new_ids
    for &old_id in defs.id_def_map.keys() {
        let new_id = defs.next_id;
        old_to_new_ids.insert(old_id, new_id);
        defs.next_id += 1;
    }

    // Update port_id_map in place
    for (_, id) in defs.port_id_map.iter_mut() {
        if let Some(new_id) = old_to_new_ids.get(id) {
            *id = *new_id;
        }
    }

    // Update id_def_map in place
    for (old_id, new_id) in old_to_new_ids.iter() {
        if let Some(value) = defs.id_def_map.remove(old_id) {
            defs.id_def_map.insert(*new_id, value);
        }
    }
}

// Given a list of 3ac equations, generate a semantically equivalent string diagram.
pub fn build_string_diagram(
    equations: Vec<TExpr>,
    input_ids: &HashSet<u32>,
    defs: &mut DefinitionRegistry,
) -> StringDiagram {
    let mut diagram = StringDiagram::new();
    let mut variable_addresses: HashMap<VariableId, Address> = HashMap::new();

    for eq in equations {
        if let Expr::Infix(InfixOp::Equal, left, right) = eq.v {
            match (&left.v, &right.v) {
                (Expr::Variable(var), Expr::Constant(c)) => {
                    // TODO: There's probably a better way to do this.
                    let equality_address = variable_addresses.get(&var.id).cloned();
                    let was_newly_created = equality_address.is_none();

                    let next_addr = diagram.next_address;
                    let target_address = if was_newly_created {
                        next_addr + 1
                    } else {
                        next_addr
                    };

                    let (equality_address, new_port_index) = get_or_create_equality_node(
                        var,
                        Port(target_address, 0),
                        &mut diagram,
                        &mut variable_addresses,
                        input_ids,
                        defs,
                    );

                    let const_addr = diagram.add_node(Node::Constant(
                        c.clone(),
                        Port(equality_address, new_port_index),
                    ));

                    defs.register_definition(
                        (Port(const_addr, 0), Port(equality_address, new_port_index)),
                        Expr::Constant(c.clone()),
                    );
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
                        }
                        _ => next_id + 1,
                    };

                    let (equality_address1, new_port_index1) = get_or_create_equality_node(
                        var1,
                        Port(target_address, 0),
                        &mut diagram,
                        &mut variable_addresses,
                        input_ids,
                        defs,
                    );
                    let (equality_address2, new_port_index2) = get_or_create_equality_node(
                        var2,
                        Port(target_address, 1),
                        &mut diagram,
                        &mut variable_addresses,
                        input_ids,
                        defs,
                    );

                    // Connect these nodes by adding an equality node that points to both.
                    diagram.add_node(Node::Equality(
                        vec![],
                        vec![
                            Port(equality_address1, new_port_index1),
                            Port(equality_address2, new_port_index2),
                        ],
                    ));
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
                            }
                            _ => next_id + 1,
                        };

                        let (equality_address1, new_port_index1) = get_or_create_equality_node(
                            var1,
                            Port(target_address, 0),
                            &mut diagram,
                            &mut variable_addresses,
                            input_ids,
                            defs,
                        );
                        let (equality_address2, new_port_index2) = get_or_create_equality_node(
                            var2,
                            Port(target_address, 1),
                            &mut diagram,
                            &mut variable_addresses,
                            input_ids,
                            defs,
                        );

                        diagram.add_node(Node::MultiplyConstant(
                            (-1).into(),
                            Port(equality_address1, new_port_index1),
                            Port(equality_address2, new_port_index2),
                        ));
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
                                let (address0, port_index0) = get_or_create_equality_node(
                                    var,
                                    Port(target_address, 1),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                    defs,
                                );
                                let (address1, port_index1) = get_or_create_equality_node(
                                    term_var1,
                                    Port(target_address, 0),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                    defs,
                                );
                                let (address2, port_index2) = get_or_create_equality_node(
                                    term_var2,
                                    Port(target_address, 2),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                    defs,
                                );
                                ports = vec![
                                    Port(address1, port_index1),
                                    Port(address0, port_index0),
                                    Port(address2, port_index2),
                                ];
                            } else if matches!(op, InfixOp::Add) || matches!(op, InfixOp::Multiply)
                            {
                                let (address0, port_index0) = get_or_create_equality_node(
                                    var,
                                    Port(target_address, 0),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                    defs,
                                );
                                let (address1, port_index1) = get_or_create_equality_node(
                                    term_var1,
                                    Port(target_address, 1),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                    defs,
                                );
                                let (address2, port_index2) = get_or_create_equality_node(
                                    term_var2,
                                    Port(target_address, 2),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                    defs,
                                );
                                ports = vec![
                                    Port(address0, port_index0),
                                    Port(address1, port_index1),
                                    Port(address2, port_index2),
                                ];
                            } else {
                                panic!("Invalid operation encountered: {op}")
                            }
                        }
                        (Expr::Constant(const1), Expr::Variable(term_var2)) => {
                            if !variable_addresses.contains_key(&term_var2.id) {
                                new_var_ids.insert(term_var2.id);
                            }

                            target_address = diagram.next_address + new_var_ids.len() + 1;

                            if matches!(op, InfixOp::Subtract) || matches!(op, InfixOp::Divide) {
                                let (address0, port_index0) = get_or_create_equality_node(
                                    var,
                                    Port(target_address, 1),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                    defs,
                                );
                                let address1 = diagram.add_node(Node::Constant(
                                    const1.clone(),
                                    Port(target_address, 0),
                                ));
                                defs.register_definition(
                                    (Port(address1, 0), Port(target_address, 0)),
                                    Expr::Constant(const1.clone()),
                                );
                                let (address2, port_index2) = get_or_create_equality_node(
                                    term_var2,
                                    Port(target_address, 2),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                    defs,
                                );
                                ports = vec![
                                    Port(address1, 0),
                                    Port(address0, port_index0),
                                    Port(address2, port_index2),
                                ];
                            } else if matches!(op, InfixOp::Add) || matches!(op, InfixOp::Multiply)
                            {
                                let (address0, port_index0) = get_or_create_equality_node(
                                    var,
                                    Port(target_address, 0),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                    defs,
                                );
                                let address1 = diagram.add_node(Node::Constant(
                                    const1.clone(),
                                    Port(target_address, 1),
                                ));
                                defs.register_definition(
                                    (Port(address1, 0), Port(target_address, 1)),
                                    Expr::Constant(const1.clone()),
                                );
                                let (address2, port_index2) = get_or_create_equality_node(
                                    term_var2,
                                    Port(target_address, 2),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                    defs,
                                );
                                ports = vec![
                                    Port(address0, port_index0),
                                    Port(address1, 0),
                                    Port(address2, port_index2),
                                ];
                            } else {
                                panic!("Invalid operation encountered: {op}")
                            }
                        }
                        (Expr::Variable(term_var1), Expr::Constant(const2)) => {
                            if !variable_addresses.contains_key(&term_var1.id) {
                                new_var_ids.insert(term_var1.id);
                            }

                            target_address = diagram.next_address + new_var_ids.len() + 1;

                            if matches!(op, InfixOp::Subtract) || matches!(op, InfixOp::Divide) {
                                let (address0, port_index0) = get_or_create_equality_node(
                                    var,
                                    Port(target_address, 1),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                    defs,
                                );
                                let (address1, port_index1) = get_or_create_equality_node(
                                    term_var1,
                                    Port(target_address, 0),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                    defs,
                                );
                                let address2 = diagram.add_node(Node::Constant(
                                    const2.clone(),
                                    Port(target_address, 2),
                                ));
                                defs.register_definition(
                                    (Port(address2, 0), Port(target_address, 2)),
                                    Expr::Constant(const2.clone()),
                                );
                                ports = vec![
                                    Port(address1, port_index1),
                                    Port(address0, port_index0),
                                    Port(address2, 0),
                                ];
                            } else if matches!(op, InfixOp::Add) || matches!(op, InfixOp::Multiply)
                            {
                                let (address0, port_index0) = get_or_create_equality_node(
                                    var,
                                    Port(target_address, 0),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                    defs,
                                );
                                let (address1, port_index1) = get_or_create_equality_node(
                                    term_var1,
                                    Port(target_address, 1),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                    defs,
                                );
                                let address2 = diagram.add_node(Node::Constant(
                                    const2.clone(),
                                    Port(target_address, 2),
                                ));
                                defs.register_definition(
                                    (Port(address2, 0), Port(target_address, 2)),
                                    Expr::Constant(const2.clone()),
                                );
                                ports = vec![
                                    Port(address0, port_index0),
                                    Port(address1, port_index1),
                                    Port(address2, 0),
                                ];
                            } else {
                                panic!("Invalid operation encountered: {op}")
                            }
                        }
                        (Expr::Constant(const1), Expr::Constant(const2)) => {
                            target_address = diagram.next_address + new_var_ids.len() + 2;

                            if matches!(op, InfixOp::Subtract) || matches!(op, InfixOp::Divide) {
                                let (address0, port_index0) = get_or_create_equality_node(
                                    var,
                                    Port(target_address, 1),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                    defs,
                                );
                                let address1 = diagram.add_node(Node::Constant(
                                    const1.clone(),
                                    Port(target_address, 0),
                                ));
                                defs.register_definition(
                                    (Port(address1, 0), Port(target_address, 0)),
                                    Expr::Constant(const1.clone()),
                                );
                                let address2 = diagram.add_node(Node::Constant(
                                    const2.clone(),
                                    Port(target_address, 2),
                                ));
                                defs.register_definition(
                                    (Port(address2, 0), Port(target_address, 2)),
                                    Expr::Constant(const2.clone()),
                                );
                                ports = vec![
                                    Port(address1, 0),
                                    Port(address0, port_index0),
                                    Port(address2, 0),
                                ];
                            } else if matches!(op, InfixOp::Add) || matches!(op, InfixOp::Multiply)
                            {
                                let (address0, port_index0) = get_or_create_equality_node(
                                    var,
                                    Port(target_address, 0),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                    defs,
                                );
                                let address1 = diagram.add_node(Node::Constant(
                                    const1.clone(),
                                    Port(target_address, 1),
                                ));
                                defs.register_definition(
                                    (Port(address1, 0), Port(target_address, 1)),
                                    Expr::Constant(const1.clone()),
                                );
                                let address2 = diagram.add_node(Node::Constant(
                                    const2.clone(),
                                    Port(target_address, 2),
                                ));
                                defs.register_definition(
                                    (Port(address2, 0), Port(target_address, 2)),
                                    Expr::Constant(const2.clone()),
                                );
                                ports = vec![
                                    Port(address0, port_index0),
                                    Port(address1, 0),
                                    Port(address2, 0),
                                ];
                            } else {
                                panic!("Invalid operation encountered: {op}")
                            }
                        }
                        _ => panic!("Impossible"),
                    }

                    if matches!(op, InfixOp::Add) || matches!(op, InfixOp::Subtract) {
                        diagram.add_node(Node::Addition(ports));
                    } else if matches!(op, InfixOp::Multiply) || matches!(op, InfixOp::Divide) {
                        diagram.add_node(Node::Multiplication(ports));
                    }
                }
                expr => {
                    println!("Unsupported: {expr:?}")
                    // Unsupported case
                }
            }
        }
    }

    // Rename variables to preserve initial definitions
    preserve_initial_defs(defs);

    diagram
}

fn get_variable_ids_in_equality_nodes(diagram: &StringDiagram) -> HashSet<u32> {
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
fn construct_port_vars(
    diagram: &StringDiagram,
    reg: &DefinitionRegistry,
) -> HashMap<Port, Variable> {
    let named_var_ids = get_variable_ids_in_equality_nodes(diagram);

    let mut port_vars: HashMap<Port, Variable> = HashMap::new();
    let mut variable_id_counter = reg.next_id;
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
            } else if let Some(id) = reg.port_id_map.get(&Port(*address, 0)) {
                let var = Variable {
                    name: None,
                    id: *id,
                };

                for (index, target_port) in ports.iter().enumerate() {
                    let current_port = Port(*address, index);

                    port_vars.insert(current_port, var.clone());
                    port_vars.insert(target_port.clone(), var.clone());
                }
            } else {
                let var = Variable {
                    name: None,
                    id: variable_id_counter,
                };
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
    for (address, _) in &diagram.nodes {
        let ports = diagram.port_list(address);

        for (index, target_port) in ports.iter().enumerate() {
            let current_port = Port(*address, index);
            if !port_vars.contains_key(&current_port) {
                if let Some(id) = reg.port_id_map.get(&current_port) {
                    let variable = Variable {
                        name: None,
                        id: *id,
                    };
                    port_vars.insert(current_port, variable.clone());
                    port_vars.insert(target_port.clone(), variable);
                } else {
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
    }

    port_vars
}

// Converts a string diagram which has already been prepared into a list of 3AC constraints.
fn convert_to_3ac(diagram: &StringDiagram, reg: &DefinitionRegistry) -> Vec<TExpr> {
    let port_vars = construct_port_vars(diagram, reg);
    let mut expressions = Vec::new();

    for node in diagram.nodes.values() {
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
            Node::CVVAdd(value, p1, p2) => {
                expressions.push(TExpr {
                    v: Expr::Infix(
                        InfixOp::Equal,
                        Box::new(TExpr {
                            v: Expr::Constant(value.clone()),
                            t: None,
                        }),
                        Box::new(TExpr {
                            v: Expr::Infix(
                                InfixOp::Add,
                                Box::new(TExpr {
                                    v: Expr::Variable(port_vars[p1].clone()),
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
            Node::CVVMul(value, p1, p2) => {
                expressions.push(TExpr {
                    v: Expr::Infix(
                        InfixOp::Equal,
                        Box::new(TExpr {
                            v: Expr::Constant(value.clone()),
                            t: None,
                        }),
                        Box::new(TExpr {
                            v: Expr::Infix(
                                InfixOp::Multiply,
                                Box::new(TExpr {
                                    v: Expr::Variable(port_vars[p1].clone()),
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

#[derive(Clone, Debug)]
pub enum RewriteRule {
    DeleteEmptyEquality(Address),
    RemoveUnaryEquality(Address),
    RemoveUnaryAddition(Address),
    RemoveUnaryMultiplication(Address),
    SplitAddition(Address),
    SplitMultiplication(Address),
    SplitExponentiation(Address),
    FuseEquality(Address, PortIndex),
    FuseAddition(Address, PortIndex),
    RemoveBinaryAddition(Address, (Port, Port)),
    FuseMultiplication(Address, PortIndex),
    RemoveBinaryMultiplication(Address, (Port, Port)),
    ConstantConstantRemoval(Address, Address),
    UnrestrictedUnaryRemoval(Address, Address),
    AddConstantConstantHead(Address),
    AddConstantConstantTail(Address),
    AddConstantZero(Address, (Port, Port)),
    FuseAdditionByConstantHdTl(Address),
    FuseAdditionByConstantTlTl(Address),
    FuseAdditionByConstantHdHd(Address),
    MulConstantConstantHead(Address),
    MulConstantConstantTail(Address),
    MulConstantZero(Address, (Port, Port), Address),
    MulConstantOne(Address, (Port, Port)),
    FuseMultiplicationByConstantHdTl(Address),
    FuseMultiplicationByConstantTlTl(Address),
    FuseMultiplicationByConstantHdHd(Address),
    ExpConstantConstantTail(Address),
    ExpConstantOne(Address, (Port, Port)),
    FuseExponentiationByConstantHdTl(Address),
    EqualityUnrestricted(Address, PortIndex),
    RemoveBinaryEquality(Address, (Port, Port)),
    AdditionConstAddition(Address, PortIndex),
    MultiplicationConstMultiplication(Address, PortIndex),
    AdditionConst(Address, PortIndex),
    MultiplicationConst(Address, PortIndex),
    EqualityConst(Address, PortIndex),
    AddMulUnrestricted(Address, PortIndex),
    DeleteConstOpUnrestricted(Address, PortIndex),
    SwapAddMulConstantsHdTl(Address),
    SwapAddMulConstantsTlTl(Address),
    ReduceCVVAddition(Address, Port),
    ReduceCVVMultiplication(Address, Port),
}

pub type RewriteTrace = Vec<RewriteRule>;

fn split_addition_node(diagram: &mut StringDiagram, address: Address) {
    // Extract information about the node at the given address
    let (first_two_ports, mut remaining_ports) =
        if let Some(Node::Addition(ports)) = diagram.nodes.get(&address) {
            if ports.len() > 3 {
                (
                    vec![ports[0].clone(), ports[1].clone()],
                    ports[2..].to_vec(),
                )
            } else {
                return;
            }
        } else {
            return;
        };

    // Create a new addition node with the first two ports of the original node
    let new_node_ports = vec![
        first_two_ports[0].clone(),
        first_two_ports[1].clone(),
        Port(address, 0),
    ];
    let new_address = diagram.add_node(Node::Addition(new_node_ports));

    // Replace the first two ports from the original node with a port pointing to the second arg of the new node.
    remaining_ports.insert(0, Port(new_address, 2));

    // Replace the node in the diagram with the updated node
    if let Some(Node::Addition(ports)) = diagram.nodes.get_mut(&address) {
        ports.clear();
        ports.extend_from_slice(&remaining_ports);
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
fn decompose_addition_node(diagram: &mut StringDiagram, address: Address) -> RewriteTrace {
    let mut trace: RewriteTrace = vec![];

    while let Some(Node::Addition(ports)) = diagram.nodes.get(&address) {
        if ports.len() > 3 {
            split_addition_node(diagram, address);
            trace.push(RewriteRule::SplitAddition(address))
        } else {
            break; // Decomposition is complete
        }
    }

    trace
}

fn split_multiplication_node(diagram: &mut StringDiagram, address: Address) {
    // Extract information about the node at the given address
    let (first_two_ports, mut remaining_ports) =
        if let Some(Node::Multiplication(ports)) = diagram.nodes.get(&address) {
            if ports.len() > 3 {
                (
                    vec![ports[0].clone(), ports[1].clone()],
                    ports[2..].to_vec(),
                )
            } else {
                return;
            }
        } else {
            return;
        };

    // Create a new multiplication node with the first two ports of the original node
    let new_node_ports = vec![
        first_two_ports[0].clone(),
        first_two_ports[1].clone(),
        Port(address, 0),
    ];
    let new_address = diagram.add_node(Node::Multiplication(new_node_ports));

    // Replace the first two ports from the original node with a port pointing to the second arg of the new node.
    remaining_ports.insert(0, Port(new_address, 2));

    // Replace the node in the diagram with the updated node
    if let Some(Node::Multiplication(ports)) = diagram.nodes.get_mut(&address) {
        ports.clear();
        ports.extend_from_slice(&remaining_ports);
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
fn decompose_multiplication_node(diagram: &mut StringDiagram, address: Address) -> RewriteTrace {
    let mut trace: RewriteTrace = vec![];

    while let Some(Node::Multiplication(ports)) = diagram.nodes.get(&address) {
        if ports.len() > 3 {
            split_multiplication_node(diagram, address);
            trace.push(RewriteRule::SplitMultiplication(address))
        } else {
            break; // Decomposition is complete
        }
    }

    trace
}

// Split X = Y ^ n into X = Y * Y * ... * Y
// TODO: This is dumb. It needs to be split so that
//       X = Y ^ 2*n => Y1 = Y ^ n /\ X = Y1 * Y1, etc.
//       much smaller that way
fn split_exponentiation_node(diagram: &mut StringDiagram, address: Address) -> RewriteTrace {
    let trace: RewriteTrace = vec![];

    // Get the data needed from the node at the address
    let (exp, p1, p2) =
        if let Some(Node::ExponentiateConstant(exp, p1, p2)) = diagram.nodes.get(&address) {
            (exp.clone(), p1.clone(), p2.clone())
        } else {
            return trace;
        };

    // Convert bigint into usize
    let exp_str = exp.to_str_radix(10);
    let exp_value = if let Ok(exp_value) = exp_str.parse::<usize>() {
        exp_value
    } else {
        // The exponentiation is too large to handle
        return trace;
    };

    // Create a new equality node with as many ports as the size of the exponent plus one.
    let mut equality_ports = Vec::new();
    equality_ports.push(p2.clone()); // The first port of the equality node should be the second port of the original exponentiation node
    for i in 0..exp_value {
        // Point the equality ports to the multiplication ports
        equality_ports.push(Port(address, i + 1));
    }
    let equality_address = diagram.add_node(Node::Equality(vec![], equality_ports));

    // Create a new multiplication node
    let mut multiplication_ports = Vec::new();
    multiplication_ports.push(p1); // Keep the first argument the same
    for i in 0..exp_value {
        // Point the multiplication ports to the equality ports
        multiplication_ports.push(Port(equality_address, i + 1));
    }

    // Replace the exponentiation node with the multiplication node
    diagram
        .nodes
        .insert(address, Node::Multiplication(multiplication_ports));

    // The second link of the original exponent node should now point to the first port of the new equality node.
    diagram.replace_port(&p2, &Port(equality_address, 0));

    trace
}

fn fuse_equality_nodes(
    diagram: &mut StringDiagram,
    prime_node_address: Address,
    port_index: PortIndex,
) {
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
    let mut new_ports: Vec<Port> = second_node_ports
        .iter()
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

// If we have
// X = Y + Z + ... + W + ...
// W = A + B + ...
// We can fuse them into
// X = Y + Z + ... + A + B + ...
fn fuse_addition_nodes(
    diagram: &mut StringDiagram,
    prime_node_address: Address,
    port_index: PortIndex,
) {
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
    let mut new_ports: Vec<Port> = second_node_ports
        .iter()
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

// If we have X = Y expressed through an addition/multiplication node
// In other words, the sum/product of all the inputs when there's only one input
// The node can be removed.
fn remove_binary_node(diagram: &mut StringDiagram, address: Address) {
    // Extract information about the node at the given address
    let (first_port_target, second_port_target) =
        match diagram.nodes.get(&address) {
            Some(
                Node::Addition(ports) | Node::Multiplication(ports) | Node::Equality(_, ports),
            ) if ports.len() == 2 => (ports[0].clone(), ports[1].clone()),
            Some(
                Node::AddConstant(_, first_target_port, second_target_port)
                | Node::MultiplyConstant(_, first_target_port, second_target_port)
                | Node::ExponentiateConstant(_, first_target_port, second_target_port),
            ) => (first_target_port.clone(), second_target_port.clone()),
            _ => return, // return if not applicable
        };

    // Remove the Addition node from the diagram
    diagram.nodes.remove(&address);

    // Update the target links of both ports to point at each other
    diagram.replace_port(&first_port_target, &second_port_target);
    diagram.replace_port(&second_port_target, &first_port_target);
}

// If we have
// X = Y * Z * ... * W * ...
// W = A * B * ...
// We can fuse them into
// X = Y * Z * ... * A * B * ...
fn fuse_multiplication_nodes(
    diagram: &mut StringDiagram,
    prime_node_address: Address,
    port_index: PortIndex,
) {
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
    let mut new_ports: Vec<Port> = second_node_ports
        .iter()
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

// If two constants are connected, they are either trivially true (of their values are equal)
// or imply a contradiction (if their values are false).
fn constant_constant_removal(diagram: &mut StringDiagram, address: Address) {
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

// If a constant, m, is connected to the first port of an addition by constant, n, node, this
// produces the equations
// Y = m
// Y = n + X
// These can be combined into
// X = m - n
fn add_constant_constant_head(
    diagram: &mut StringDiagram,
    address: Address,
    field_ops: &dyn FieldOps,
) {
    // Extract the necessary data
    let (n, first_port, second_port) =
        if let Some(Node::AddConstant(n, first_port, second_port)) = diagram.nodes.get(&address) {
            (n.clone(), first_port.clone(), second_port.clone())
        } else {
            return; // Exit if it is not an AddConstant node
        };

    let m = if let Some(Node::Constant(m, _)) = diagram.nodes.get(&first_port.0) {
        m.clone()
    } else {
        return; // Exit if the target node is not a Constant node
    };

    // Now that the necessary data is extracted, perform the mutable operations
    let new_constant_value = field_ops.infix(InfixOp::Subtract, m, n);

    // Replace the AddConstant node with the new constant node
    diagram.nodes.insert(
        address,
        Node::Constant(new_constant_value, second_port.clone()),
    );

    // Update the target link of the second port of the AddConstant node
    diagram.replace_port(&second_port, &Port(address, 0));

    // Remove the original constant node connected to the first port
    diagram.nodes.remove(&first_port.0);
}

// If a constant, m, is connected to the second port of an addition by constant, n, node, this
// produces the equations
// Y = n + X
// X = m
// These can be combined into
// Y = n + m
fn add_constant_constant_tail(
    diagram: &mut StringDiagram,
    address: Address,
    field_ops: &dyn FieldOps,
) {
    // Extract the necessary data
    let (n, first_port, second_port) =
        if let Some(Node::AddConstant(n, first_port, second_port)) = diagram.nodes.get(&address) {
            (n.clone(), first_port.clone(), second_port.clone())
        } else {
            return; // Exit if it is not an AddConstant node
        };

    let m = if let Some(Node::Constant(m, _)) = diagram.nodes.get(&second_port.0) {
        m.clone()
    } else {
        return; // Exit if the target node is not a Constant node
    };

    // Now that the necessary data is extracted, perform the mutable operations
    let new_constant_value = field_ops.infix(InfixOp::Add, n, m);

    // Replace the AddConstant node with the new constant node
    diagram.nodes.insert(
        address,
        Node::Constant(new_constant_value, first_port.clone()),
    );

    // Update the target link of the second port of the AddConstant node
    diagram.replace_port(&first_port, &Port(address, 0));

    // Remove the original constant node connected to the first port
    diagram.nodes.remove(&second_port.0);
}

// If we have two AddConstant in series, this creates the equations
// Z = m + Y
// Y = n + X
// These can be fused into
// Z = (m + n) + X
fn fuse_addition_by_constant_hd_tl(
    diagram: &mut StringDiagram,
    address: Address,
    field_ops: &dyn FieldOps,
) {
    // Extract information about the node at the given address
    let (second_node_address, second_node_value, first_node_value, second_node_second_port) = {
        match diagram.nodes.get(&address) {
            Some(Node::AddConstant(value, _, second_port)) => {
                match diagram.nodes.get(&second_port.0) {
                    Some(Node::AddConstant(second_node_value, _, second_node_second_port))
                        if second_port.1 == 0 =>
                    {
                        (
                            second_port.0,
                            second_node_value.clone(),
                            value.clone(),
                            second_node_second_port.clone(),
                        )
                    }
                    _ => return, // The second port of the first node is not connected to the first port of another AddConstant node
                }
            }
            _ => return, // Not an AddConstant node or doesn't exist
        }
    };

    // Sum the values from the first and second node
    let new_value = field_ops.infix(InfixOp::Add, first_node_value, second_node_value);

    // Update the first node value in the diagram and set its second port to the second port of the second node
    if let Some(Node::AddConstant(value, _, second_port)) = diagram.nodes.get_mut(&address) {
        *value = new_value;
        *second_port = second_node_second_port.clone();
    }

    // Update the target link of the second port to point to the first node
    diagram.replace_port(&second_node_second_port, &Port(address, 1));

    // Remove the second AddConstant node from the diagram
    diagram.nodes.remove(&second_node_address);
}

// If we have two AddConstant connected by their heads, this means
// Y = n + Z
// Y = m + X
// These can be fused into
// X = (n - m) + Z
fn fuse_addition_by_constant_hd_hd(
    diagram: &mut StringDiagram,
    address: Address,
    field_ops: &dyn FieldOps,
) {
    // Extract information about the node at the given address
    let (second_node_address, second_node_value, first_node_value, second_node_second_port) = {
        match diagram.nodes.get(&address) {
            Some(Node::AddConstant(value, first_port, _)) => {
                match diagram.nodes.get(&first_port.0) {
                    Some(Node::AddConstant(second_node_value, _, second_node_second_port))
                        if first_port.1 == 0 =>
                    {
                        (
                            first_port.0,
                            second_node_value.clone(),
                            value.clone(),
                            second_node_second_port.clone(),
                        )
                    }
                    _ => return, // The second port of the first node is not connected to the first port of another AddConstant node
                }
            }
            _ => return, // Not an AddConstant node or doesn't exist
        }
    };

    // Difference the values from the first and second node
    let new_value = field_ops.infix(InfixOp::Subtract, first_node_value, second_node_value);

    // Update the first node value in the diagram and set its second port to the second port of the second node
    if let Some(Node::AddConstant(value, first_port, _)) = diagram.nodes.get_mut(&address) {
        *value = new_value;
        *first_port = second_node_second_port.clone();
    }

    // Update the target link of the second port to point to the first node
    diagram.replace_port(&second_node_second_port, &Port(address, 0));

    // Remove the second AddConstant node from the diagram
    diagram.nodes.remove(&second_node_address);
}

// If we have two AddConstant connected by their tails, this means
// Z = n + Y
// X = m + Y
// These can be fused into
// Z = (n - m) + X
fn fuse_addition_by_constant_tl_tl(
    diagram: &mut StringDiagram,
    address: Address,
    field_ops: &dyn FieldOps,
) {
    // Extract information about the node at the given address
    let (second_node_address, second_node_value, first_node_value, second_node_first_port) = {
        match diagram.nodes.get(&address) {
            Some(Node::AddConstant(value, _, second_port)) => {
                match diagram.nodes.get(&second_port.0) {
                    Some(Node::AddConstant(second_node_value, second_node_first_port, _))
                        if second_port.1 == 1 =>
                    {
                        (
                            second_port.0,
                            second_node_value.clone(),
                            value.clone(),
                            second_node_first_port.clone(),
                        )
                    }
                    _ => return, // The second port of the first node is not connected to the first port of another AddConstant node
                }
            }
            _ => return, // Not an AddConstant node or doesn't exist
        }
    };

    // Difference the values from the first and second node
    let new_value = field_ops.infix(InfixOp::Subtract, first_node_value, second_node_value);

    // Update the first node value in the diagram and set its second port to the second port of the second node
    if let Some(Node::AddConstant(value, _, second_port)) = diagram.nodes.get_mut(&address) {
        *value = new_value;
        *second_port = second_node_first_port.clone();
    }

    // Update the target link of the second port to point to the first node
    diagram.replace_port(&second_node_first_port, &Port(address, 1));

    // Remove the second AddConstant node from the diagram
    diagram.nodes.remove(&second_node_address);
}

// If a constant, m, is connected to the first port of an multiplication by constant, n, node, this
// produces the equations
// Y = m
// Y = n * X
// These can be combined into
// X = m / n
// This is wrong if n = 0, but that case is handled by mul_constant_zero.
fn mul_constant_constant_head(
    diagram: &mut StringDiagram,
    address: Address,
    field_ops: &dyn FieldOps,
) {
    // Extract the necessary data
    let (n, first_port, second_port) =
        if let Some(Node::MultiplyConstant(n, first_port, second_port)) =
            diagram.nodes.get(&address)
        {
            (n.clone(), first_port.clone(), second_port.clone())
        } else {
            return; // Exit if it is not an MultiplyConstant node
        };

    let m = if let Some(Node::Constant(m, _)) = diagram.nodes.get(&first_port.0) {
        m.clone()
    } else {
        return; // Exit if the target node is not a Constant node
    };

    // Now that the necessary data is extracted, perform the mutable operations
    let new_constant_value = field_ops.infix(InfixOp::Divide, m, n);

    // Replace the MultiplyConstant node with the new constant node
    diagram.nodes.insert(
        address,
        Node::Constant(new_constant_value, second_port.clone()),
    );

    // Update the target link of the second port of the MultiplyConstant node
    diagram.replace_port(&second_port, &Port(address, 0));

    // Remove the original constant node connected to the first port
    diagram.nodes.remove(&first_port.0);
}

// If a constant, m, is connected to the second port of a multiplication by constant, n, node, this
// produces the equations
// Y = n * X
// X = m
// These can be combined into
// Y = n * m
fn mul_constant_constant_tail(
    diagram: &mut StringDiagram,
    address: Address,
    field_ops: &dyn FieldOps,
) {
    // Extract the necessary data
    let (n, first_port, second_port) =
        if let Some(Node::MultiplyConstant(n, first_port, second_port)) =
            diagram.nodes.get(&address)
        {
            (n.clone(), first_port.clone(), second_port.clone())
        } else {
            return; // Exit if it is not an MultiplyConstant node
        };

    let m = if let Some(Node::Constant(m, _)) = diagram.nodes.get(&second_port.0) {
        m.clone()
    } else {
        return; // Exit if the target node is not a Constant node
    };

    // Now that the necessary data is extracted, perform the mutable operations
    let new_constant_value = field_ops.infix(InfixOp::Multiply, m, n);

    // Replace the MultiplyConstant node with the new constant node
    diagram.nodes.insert(
        address,
        Node::Constant(new_constant_value, first_port.clone()),
    );

    // Update the target link of the second port of the MultiplyConstant node
    diagram.replace_port(&first_port, &Port(address, 0));

    // Remove the original constant node connected to the first port
    diagram.nodes.remove(&second_port.0);
}

// If X = 0 * Y, then we can split this into
// X = 0
// No restriction on Y.
fn mul_constant_zero(diagram: &mut StringDiagram, address: Address) {
    // Extract information about the node at the given address
    let (port1, port2, is_zero) = match diagram.nodes.get(&address) {
        Some(Node::MultiplyConstant(value, port1, port2)) if *value == BigInt::from(0) => {
            (port1.clone(), port2.clone(), true)
        }
        _ => return, // Exit if it is not an MultiplyConstant node
    };

    // If it is a MultiplyConstant node with value 0
    if is_zero {
        // Create a new Unrestricted node with the target link of the second port
        let unrestricted_address = diagram.add_node(Node::Unrestricted(port2.clone()));

        // Replace the original MultiplyConstant node with a Constant node containing 0
        diagram
            .nodes
            .insert(address, Node::Constant(BigInt::from(0), port1));

        // Update the target link of the second port to point to the new Unrestricted node
        diagram.replace_port(&port2, &Port(unrestricted_address, 0));
    }
}

// If we have two MultiplyConstant in series, this creates the equations
// Z = m * Y
// Y = n * X
// These can be fused into
// Z = (m * n) * X
fn fuse_multiplication_by_constant_hd_tl(
    diagram: &mut StringDiagram,
    address: Address,
    field_ops: &dyn FieldOps,
) {
    // Extract information about the node at the given address
    let (second_node_address, second_node_value, first_node_value, second_node_second_port) = {
        match diagram.nodes.get(&address) {
            Some(Node::MultiplyConstant(value, _, second_port)) => {
                match diagram.nodes.get(&second_port.0) {
                    Some(Node::MultiplyConstant(second_node_value, _, second_node_second_port))
                        if second_port.1 == 0 =>
                    {
                        (
                            second_port.0,
                            second_node_value.clone(),
                            value.clone(),
                            second_node_second_port.clone(),
                        )
                    }
                    _ => return, // The second port of the first node is not connected to the first port of another MultiplyConstant node
                }
            }
            _ => return, // Not an MultiplyConstant node or doesn't exist
        }
    };

    // Sum the values from the first and second node
    let new_value = field_ops.infix(InfixOp::Multiply, first_node_value, second_node_value);

    // Update the first node value in the diagram and set its second port to the second port of the second node
    if let Some(Node::MultiplyConstant(value, _, second_port)) = diagram.nodes.get_mut(&address) {
        *value = new_value;
        *second_port = second_node_second_port.clone();
    }

    // Update the target link of the second port to point to the first node
    diagram.replace_port(&second_node_second_port, &Port(address, 1));

    // Remove the second MultiplyConstant node from the diagram
    diagram.nodes.remove(&second_node_address);
}

// If we have two MultiplyConstant connected by their heads, this means
// Y = n * Z
// Y = m * X
// These can be fused into
// Z = (m / n) * X
// This order is important so n = 0 isn't possible
// If we used X = (n / m) * Z, m might be 0, but we know n = 0 will have been delt with already.
fn fuse_multiplication_by_constant_hd_hd(
    diagram: &mut StringDiagram,
    address: Address,
    field_ops: &dyn FieldOps,
) {
    // Extract information about the node at the given address
    let (
        second_node_address,
        second_node_value,
        first_node_value,
        second_node_second_port,
        second_port,
    ) = {
        match diagram.nodes.get(&address) {
            Some(Node::MultiplyConstant(value, first_port, second_port)) => {
                match diagram.nodes.get(&first_port.0) {
                    Some(Node::MultiplyConstant(second_node_value, _, second_node_second_port))
                        if first_port.1 == 0 =>
                    {
                        (
                            first_port.0,
                            second_node_value.clone(),
                            value.clone(),
                            second_node_second_port.clone(),
                            second_port.clone(),
                        )
                    }
                    _ => return, // The second port of the first node is not connected to the first port of another MultiplyConstant node
                }
            }
            _ => return, // Not an MultiplyConstant node or doesn't exist
        }
    };

    // Ratio the values from the first and second node
    let new_value = field_ops.infix(InfixOp::Divide, second_node_value, first_node_value);

    // Update the first node value in the diagram
    diagram.nodes.insert(
        address,
        Node::MultiplyConstant(
            new_value,
            second_port.clone(),
            second_node_second_port.clone(),
        ),
    );

    // Update the target link of the first port to point to the first node
    diagram.replace_port(&second_port, &Port(address, 0));

    // Update the target link of the second port to point to the first node
    diagram.replace_port(&second_node_second_port, &Port(address, 1));

    // Remove the second MultiplyConstant node from the diagram
    diagram.nodes.remove(&second_node_address);
}

// If we have two MultiplyConstant connected by their tails, this means
// Z = n * Y
// X = m * Y
// These can be fused into
// X = (m / n) * Z
// This order is important so n = 0 isn't possible
// If we used Z = (n / m) * X, m might be 0, but we know n = 0 will have been delt with already.
fn fuse_multiplication_by_constant_tl_tl(
    diagram: &mut StringDiagram,
    address: Address,
    field_ops: &dyn FieldOps,
) {
    // Extract information about the node at the given address
    let (
        second_node_address,
        second_node_value,
        first_node_value,
        second_node_first_port,
        first_port,
    ) = {
        match diagram.nodes.get(&address) {
            Some(Node::MultiplyConstant(value, first_port, second_port)) => {
                match diagram.nodes.get(&second_port.0) {
                    Some(Node::MultiplyConstant(second_node_value, second_node_first_port, _))
                        if second_port.1 == 1 =>
                    {
                        (
                            second_port.0,
                            second_node_value.clone(),
                            value.clone(),
                            second_node_first_port.clone(),
                            first_port.clone(),
                        )
                    }
                    _ => return, // The second port of the first node is not connected to the first port of another MultiplyConstant node
                }
            }
            _ => return, // Not an MultiplyConstant node or doesn't exist
        }
    };

    // Ratio the values from the first and second node
    let new_value = field_ops.infix(InfixOp::Divide, second_node_value, first_node_value);

    // Update the first node value in the diagram
    diagram.nodes.insert(
        address,
        Node::MultiplyConstant(
            new_value,
            second_node_first_port.clone(),
            first_port.clone(),
        ),
    );

    // Update the target link of the first port to point to the first node
    diagram.replace_port(&second_node_first_port, &Port(address, 0));

    // Update the target link of the second port to point to the first node
    diagram.replace_port(&first_port, &Port(address, 1));

    // Remove the second MultiplyConstant node from the diagram
    diagram.nodes.remove(&second_node_address);
}

// If a constant, m, is connected to the second port of a multiplication by constant, n, node, this
// produces the equations
// Y = X ^ n
// X = m
// These can be combined into
// Y = m ^ n
fn exp_constant_constant_tail(
    diagram: &mut StringDiagram,
    address: Address,
    field_ops: &dyn FieldOps,
) {
    // Extract the necessary data
    let (n, first_port, second_port) =
        if let Some(Node::ExponentiateConstant(n, first_port, second_port)) =
            diagram.nodes.get(&address)
        {
            (n.clone(), first_port.clone(), second_port.clone())
        } else {
            return; // Exit if it is not an ExponentiateConstant node
        };

    let m = if let Some(Node::Constant(m, _)) = diagram.nodes.get(&second_port.0) {
        m.clone()
    } else {
        return; // Exit if the target node is not a Constant node
    };

    // Now that the necessary data is extracted, perform the mutable operations
    let new_constant_value = field_ops.infix(InfixOp::Exponentiate, m, n);

    // Replace the ExponentiateConstant node with the new constant node
    diagram.nodes.insert(
        address,
        Node::Constant(new_constant_value, first_port.clone()),
    );

    // Update the target link of the second port of the ExponentiateConstant node
    diagram.replace_port(&first_port, &Port(address, 0));

    // Remove the original constant node connected to the first port
    diagram.nodes.remove(&second_port.0);
}

// If we have two MultiplyConstant in series, this creates the equations
// Z = Y ^ m
// Y = X ^ n
// These can be fused into
// Z = X ^ (m * n)
// So long as m and n are positive. In a finite field, this is just being nonzero.
fn fuse_exponentiation_by_constant_hd_tl(
    diagram: &mut StringDiagram,
    address: Address,
    field_ops: &dyn FieldOps,
) {
    // Extract information about the node at the given address
    let (second_node_address, second_node_value, first_node_value, second_node_second_port) = {
        match diagram.nodes.get(&address) {
            Some(Node::ExponentiateConstant(value, _, second_port)) => {
                match diagram.nodes.get(&second_port.0) {
                    Some(Node::ExponentiateConstant(
                        second_node_value,
                        _,
                        second_node_second_port,
                    )) if second_port.1 == 0 => {
                        if *second_node_value != BigInt::from(0) && *value != BigInt::from(0) {
                            (
                                second_port.0,
                                second_node_value.clone(),
                                value.clone(),
                                second_node_second_port.clone(),
                            )
                        } else {
                            return;
                        }
                    }
                    _ => return, // The second port of the first node is not connected to the first port of another ExponentiateConstant node
                }
            }
            _ => return, // Not an ExponentiateConstant node or doesn't exist
        }
    };

    // Sum the values from the first and second node
    let new_value = field_ops.infix(InfixOp::Multiply, first_node_value, second_node_value);

    // Update the first node value in the diagram and set its second port to the second port of the second node
    if let Some(Node::ExponentiateConstant(value, _, second_port)) = diagram.nodes.get_mut(&address)
    {
        *value = new_value;
        *second_port = second_node_second_port.clone();
    }

    // Update the target link of the second port to point to the first node
    diagram.replace_port(&second_node_second_port, &Port(address, 1));

    // Remove the second ExponentiateConstant node from the diagram
    diagram.nodes.remove(&second_node_address);
}

// An unrestricted node connected to an equality port can simply be removed.
fn equality_unrestricted(diagram: &mut StringDiagram, address: Address, port_index: PortIndex) {
    // Extract information about the node at the given address
    let target_address = match diagram.nodes.get(&address) {
        Some(Node::Equality(_, ports)) => {
            if let Some(port) = ports.get(port_index) {
                port.0
            } else {
                return; // Invalid port index
            }
        }
        _ => return, // Not an equality node or doesn't exist
    };

    // Check if the target node is an unrestricted node
    match diagram.nodes.get(&target_address) {
        Some(Node::Unrestricted(_)) => {}
        _ => return, // The target node is not an unrestricted node or doesn't exist
    }

    // Remove the unrestricted node
    diagram.nodes.remove(&target_address);

    // Temporarily store the new ports to avoid borrowing issues
    let mut new_ports = Vec::new();

    // Update the ports for the equality node
    if let Some(Node::Equality(_, ports)) = diagram.nodes.get_mut(&address) {
        // Remove the port that was connected to the unrestricted node
        ports.remove(port_index);

        // Store the remaining ports
        new_ports = ports.clone();
    }

    // Now apply the stored new ports to the diagram
    for (index, new_port) in new_ports.iter().enumerate() {
        diagram.replace_port(new_port, &Port(address, index));
    }
}

// Turn X = A + B + ... + (c + D) + ... for constant c
// Into X = c + (A + B + ... + D + ...)
fn addition_const_addition(
    diagram: &mut StringDiagram,
    address: Address,
    port_index: PortIndex,
    field_ops: &dyn FieldOps,
) {
    // Extract the necessary information from the diagram.
    let (old_head_port, target_address, target_port_index) = {
        let ports = match diagram.nodes.get(&address) {
            Some(Node::Addition(ports)) if port_index < ports.len() => ports,
            _ => return,
        };
        (ports[0].clone(), ports[port_index].0, ports[port_index].1)
    };

    let (value, first_port, second_port) = match diagram.nodes.get(&target_address) {
        Some(Node::AddConstant(value, first_port, second_port)) => {
            (value.clone(), first_port.clone(), second_port.clone())
        }
        _ => return,
    };

    // If AddConstant node needs to be fliped, negate its value.
    let new_value = if target_port_index == 1 {
        field_ops.negate(value)
    } else {
        value
    };

    // Whatever the head of the Addition node is pointing to should now point to the head of the AddConstant node.
    diagram.replace_port(&old_head_port, &Port(target_address, 0));

    // The head of the Addition node should now point to the tail of the AddConstant node.
    diagram.replace_port(&Port(address, 0), &Port(target_address, 1));

    // Make the necessary connection based on which port of the AddConstant node was connected to the Addition node.
    if target_port_index == 0 {
        diagram.replace_port(&Port(address, port_index), &second_port);
        diagram.replace_port(&second_port, &Port(address, port_index));
    } else {
        diagram.replace_port(&Port(address, port_index), &first_port);
        diagram.replace_port(&first_port, &Port(address, port_index));
    };

    // Insert the updated AddConstant node.
    diagram.nodes.insert(
        target_address,
        Node::AddConstant(new_value, old_head_port, Port(address, 0)),
    );
}

// Turn X = A * B * ... * (c * D) * ... for constant c
// Into X = c * (A * B * ... * D * ...)
fn multiplication_const_multiplication(
    diagram: &mut StringDiagram,
    address: Address,
    port_index: PortIndex,
    field_ops: &dyn FieldOps,
) {
    // Extract the necessary information from the diagram.
    let (old_head_port, target_address, target_port_index) = {
        let ports = match diagram.nodes.get(&address) {
            Some(Node::Multiplication(ports)) if port_index < ports.len() => ports,
            _ => return,
        };
        (ports[0].clone(), ports[port_index].0, ports[port_index].1)
    };

    let (value, first_port, second_port) = match diagram.nodes.get(&target_address) {
        Some(Node::MultiplyConstant(value, first_port, second_port)) => {
            (value.clone(), first_port.clone(), second_port.clone())
        }
        _ => return,
    };

    // If MultiplyConstant node needs to be fliped, negate its value.
    let new_value = if target_port_index == 1 {
        field_ops.infix(InfixOp::Divide, BigInt::from(1), value)
    } else {
        value
    };

    // Whatever the head of the Multiplication node is pointing to should now point to the head of the MultiplyConstant node.
    diagram.replace_port(&old_head_port, &Port(target_address, 0));

    // The head of the Multiplication node should now point to the tail of the MultiplyConstant node.
    diagram.replace_port(&Port(address, 0), &Port(target_address, 1));

    // Make the necessary connection based on which port of the MultiplyConstant node was connected to the Multiplication node.
    if target_port_index == 0 {
        diagram.replace_port(&Port(address, port_index), &second_port);
        diagram.replace_port(&second_port, &Port(address, port_index));
    } else {
        diagram.replace_port(&Port(address, port_index), &first_port);
        diagram.replace_port(&first_port, &Port(address, port_index));
    };

    // Insert the updated MultiplyConstant node.
    diagram.nodes.insert(
        target_address,
        Node::MultiplyConstant(new_value, old_head_port, Port(address, 0)),
    );
}

// Turn X = A + B + ... + c + ... for constant c
// Into X = c + (A + B + ...)
fn addition_const(diagram: &mut StringDiagram, address: Address, port_index: PortIndex) {
    // Extract the necessary information from the diagram.
    let (old_head_port, target_address) = {
        let ports = match diagram.nodes.get(&address) {
            Some(Node::Addition(ports)) if port_index < ports.len() => ports,
            _ => return,
        };
        (ports[0].clone(), ports[port_index].0)
    };

    let value = match diagram.nodes.get(&target_address) {
        Some(Node::Constant(value, _)) => value.clone(),
        _ => return,
    };

    // Whatever the head of the Addition node is pointing to should now point to the head of the AddConstant node.
    diagram.replace_port(&old_head_port, &Port(target_address, 0));

    // The head of the Addition node should now point to the tail of the AddConstant node.
    diagram.replace_port(&Port(address, 0), &Port(target_address, 1));

    // Drop the previous port index and update linked ports.
    // Temporarily store the new ports to avoid borrowing issues
    let mut new_ports = Vec::new();

    // Update the ports for the equality node
    if let Some(Node::Addition(ports)) = diagram.nodes.get_mut(&address) {
        // Remove the port that was connected to the unrestricted node
        ports.remove(port_index);

        // Store the remaining ports
        new_ports = ports.clone();
    }

    // Now apply the stored new ports to the diagram
    for (index, new_port) in new_ports.iter().enumerate() {
        diagram.replace_port(new_port, &Port(address, index));
    }

    // Insert the updated AddConstant node.
    diagram.nodes.insert(
        target_address,
        Node::AddConstant(value, old_head_port, Port(address, 0)),
    );
}

// Turn X = A * B * ... * c * ... for constant c
// Into X = c * (A * B * ...)
fn multiplication_const(diagram: &mut StringDiagram, address: Address, port_index: PortIndex) {
    // Extract the necessary information from the diagram.
    let (old_head_port, target_address) = {
        let ports = match diagram.nodes.get(&address) {
            Some(Node::Multiplication(ports)) if port_index < ports.len() => ports,
            _ => return,
        };
        (ports[0].clone(), ports[port_index].0)
    };

    let value = match diagram.nodes.get(&target_address) {
        Some(Node::Constant(value, _)) => value.clone(),
        _ => return,
    };

    // Whatever the head of the Multiplication node is pointing to should now point to the head of the MultiplyConstant node.
    diagram.replace_port(&old_head_port, &Port(target_address, 0));

    // The head of the Multiplication node should now point to the tail of the MultiplyConstant node.
    diagram.replace_port(&Port(address, 0), &Port(target_address, 1));

    // Drop the previous port index and update linked ports.
    // Temporarily store the new ports to avoid borrowing issues
    let mut new_ports = Vec::new();

    // Update the ports for the equality node
    if let Some(Node::Multiplication(ports)) = diagram.nodes.get_mut(&address) {
        // Remove the port that was connected to the unrestricted node
        ports.remove(port_index);

        // Store the remaining ports
        new_ports = ports.clone();
    }

    // Now apply the stored new ports to the diagram
    for (index, new_port) in new_ports.iter().enumerate() {
        diagram.replace_port(new_port, &Port(address, index));
    }

    // Insert the updated MultiplyConstant node.
    diagram.nodes.insert(
        target_address,
        Node::MultiplyConstant(value, old_head_port, Port(address, 0)),
    );
}

// If X = c and Y = X and A = X etc. then Y = c and A = c, etc
fn equality_const(diagram: &mut StringDiagram, address: Address, port_index: PortIndex) {
    // Extract the necessary information
    let (has_variables, target_port, const_value) =
        if let Some(Node::Equality(variables, ports)) = diagram.nodes.get(&address) {
            if let Some(target_port) = ports.get(port_index) {
                if let Some(Node::Constant(value, _)) = diagram.nodes.get(&target_port.0) {
                    (!variables.is_empty(), target_port.clone(), value.clone())
                } else {
                    return;
                }
            } else {
                return;
            }
        } else {
            return;
        };

    // Gather all the ports that need to be updated
    let mut ports_to_update = Vec::new();
    if let Some(Node::Equality(_, ports)) = diagram.nodes.get_mut(&address) {
        for (index, port) in ports.iter().enumerate() {
            if index != port_index {
                ports_to_update.push(port.clone());
            }
        }

        // If the equality node does have variables, then all ports except for the one connected to
        // the original constant node should be dropped from the equality node.
        if has_variables {
            ports.retain(|port| *port == target_port);
            diagram.replace_port(&target_port, &Port(address, 0));
        }
    }

    // Update the gathered ports
    for port in ports_to_update {
        let new_const_node_addr =
            diagram.add_node(Node::Constant(const_value.clone(), port.clone()));
        diagram.replace_port(&port, &Port(new_const_node_addr, 0));
    }

    // If the equality node has no variables, it, along with the original constant node, should be deleted.
    if !has_variables {
        diagram.nodes.remove(&address);
        diagram.nodes.remove(&target_port.0);
    }
}

// If something unrestructed is used in an addition, then everything else is also unrestructed.
fn addmul_unrestricted(diagram: &mut StringDiagram, address: Address, port_index: PortIndex) {
    // Extract the necessary information
    let target_port = if let Some(Node::Addition(ports) | Node::Multiplication(ports)) =
        diagram.nodes.get(&address)
    {
        if let Some(target_port) = ports.get(port_index) {
            if let Some(Node::Unrestricted(_)) = diagram.nodes.get(&target_port.0) {
                target_port.clone()
            } else {
                return;
            }
        } else {
            return;
        }
    } else {
        return;
    };

    // Gather all the ports that need to be updated
    let mut ports_to_update = Vec::new();
    if let Some(Node::Addition(ports) | Node::Multiplication(ports)) =
        diagram.nodes.get_mut(&address)
    {
        for (index, port) in ports.iter().enumerate() {
            if index != port_index {
                ports_to_update.push(port.clone());
            }
        }
    }

    // Update the gathered ports
    for port in ports_to_update {
        let new_const_node_addr = diagram.add_node(Node::Unrestricted(port.clone()));
        diagram.replace_port(&port, &Port(new_const_node_addr, 0));
    }

    diagram.nodes.remove(&address);
    diagram.nodes.remove(&target_port.0);
}

fn delete_const_op_unrestricted(
    diagram: &mut StringDiagram,
    address: Address,
    port_index: PortIndex,
) {
    // Extract necessary information from the diagram
    let (port1, port2) = match diagram.nodes.get(&address) {
        Some(
            Node::AddConstant(_, p1, p2)
            | Node::MultiplyConstant(_, p1, p2)
            | Node::ExponentiateConstant(_, p1, p2),
        ) => {
            if port_index == 0 || port_index == 1 {
                (p1.clone(), p2.clone())
            } else {
                return;
            }
        }
        _ => return,
    };

    // Determine if either of the ports is connected to an Unrestricted node
    let is_port1_unrestricted = matches!(diagram.nodes.get(&port1.0), Some(Node::Unrestricted(_)));
    let is_port2_unrestricted = matches!(diagram.nodes.get(&port2.0), Some(Node::Unrestricted(_)));

    let (unrestricted_port, other_port) = if is_port1_unrestricted {
        (port1, port2)
    } else if is_port2_unrestricted {
        (port2, port1)
    } else {
        return;
    };

    // At this point, one of the ports is connected to an Unrestricted node
    // Update the diagram by connecting the other port to where the Unrestricted port was pointing
    diagram.replace_port(&unrestricted_port, &other_port);
    diagram.replace_port(&other_port, &unrestricted_port);

    // Remove the AddConstant or MultiplyConstant node
    diagram.nodes.remove(&address);
}

// Implements constant distributivity.
// That is,
// X = m * Y
// Y = n + Z
// becomes
// X = (m * n) + Y
// Y = m * Z
fn swap_add_and_multiply_constants_hd_tl(
    diagram: &mut StringDiagram,
    address: Address,
    field_ops: &dyn FieldOps,
) {
    // Extract necessary information from the diagram.
    let (add_value, add_head_port, add_tail_port) = match diagram.nodes.get(&address) {
        Some(Node::AddConstant(value, head_port, tail_port)) => {
            (value.clone(), head_port.clone(), tail_port.clone())
        }
        _ => return,
    };

    // Check if the head of the AddConstant node is pointing to the tail of a MultiplyConstant node.
    let (multiply_address, multiply_value, multiply_head_port, multiply_tail_port) =
        match diagram.nodes.get(&add_head_port.0) {
            Some(Node::MultiplyConstant(value, head_port, tail_port)) if add_head_port.1 == 1 => (
                add_head_port.0,
                value.clone(),
                head_port.clone(),
                tail_port.clone(),
            ),
            _ => return,
        };

    // Update the values of the nodes
    let new_add_value = field_ops.infix(InfixOp::Multiply, add_value, multiply_value.clone());

    // Update the diagram.

    // Turn the AddConstant node into a MultiplyConstant node.
    diagram.nodes.insert(
        address,
        Node::MultiplyConstant(multiply_value, add_head_port, add_tail_port),
    );

    // Turn the MultiplyConstant node into an AddConstant node.
    diagram.nodes.insert(
        multiply_address,
        Node::AddConstant(new_add_value, multiply_head_port, multiply_tail_port),
    );
}

// Implements constant distributivity.
// That is,
// X = m * Y
// Z = n + Y
// becomes
// X = (- m * n) + Y
// Y = m * Z
fn swap_add_and_multiply_constants_tl_tl(
    diagram: &mut StringDiagram,
    address: Address,
    field_ops: &dyn FieldOps,
) {
    // Extract necessary information from the diagram.
    let (add_value, add_head_port, add_tail_port) = match diagram.nodes.get(&address) {
        Some(Node::AddConstant(value, head_port, tail_port)) => {
            (value.clone(), head_port.clone(), tail_port.clone())
        }

        _ => return,
    };

    // Check if the head of the AddConstant node is pointing to the tail of a MultiplyConstant node.
    let (multiply_address, multiply_value, multiply_head_port, _multiply_tail_port) =
        match diagram.nodes.get(&add_tail_port.0) {
            Some(Node::MultiplyConstant(value, head_port, tail_port)) if add_tail_port.1 == 1 => (
                add_tail_port.0,
                value.clone(),
                head_port.clone(),
                tail_port.clone(),
            ),
            _ => return,
        };

    // Update the values of the nodes
    let new_add_value =
        field_ops.negate(field_ops.infix(InfixOp::Multiply, add_value, multiply_value.clone()));

    // Update the diagram.

    // Turn the AddConstant node into a MultiplyConstant node with swapped ports.
    diagram.nodes.insert(
        address,
        Node::MultiplyConstant(multiply_value, add_tail_port, add_head_port.clone()),
    );

    // Update the linking ports to compensate for the swap.
    diagram.replace_port(&add_head_port, &Port(address, 1));

    // Turn the MultiplyConstant node into an AddConstant node.
    diagram.nodes.insert(
        multiply_address,
        Node::AddConstant(new_add_value, multiply_head_port, Port(address, 0)),
    );
}

// Try finding an appropriate rewrite for a given address
fn gen_string_diagram_step(diagram: &StringDiagram, address: Address) -> Option<RewriteRule> {
    if let Some(node) = diagram.nodes.get(&address).cloned() {
        match node {
            Node::Equality(vars, ports) => {
                if ports.is_empty() {
                    // Delete empty equations
                    return Some(RewriteRule::DeleteEmptyEquality(address));
                }
                if vars.is_empty() && ports.len() == 1 {
                    return Some(RewriteRule::RemoveUnaryEquality(address));
                }
                if vars.is_empty() && ports.len() == 2 {
                    return Some(RewriteRule::RemoveBinaryEquality(
                        address,
                        (ports[0].clone(), ports[1].clone()),
                    ));
                }
                for (port_index, target_port) in ports.iter().enumerate() {
                    if let Some(Node::Unrestricted(_)) = diagram.nodes.get(&target_port.0) {
                        return Some(RewriteRule::EqualityUnrestricted(address, port_index));
                    }
                    if let Some(Node::Constant(_, _)) = diagram.nodes.get(&target_port.0) {
                        if ports.len() == 1 && !vars.is_empty() {
                            return Some(RewriteRule::EqualityConst(address, port_index));
                        }
                    }

                    if let Some(Node::Equality(_, _)) = diagram.nodes.get(&target_port.0) {
                        return Some(RewriteRule::FuseEquality(address, port_index));
                    }
                }
                None
            }
            Node::Addition(ports) => {
                if ports.len() == 1 {
                    return Some(RewriteRule::RemoveUnaryAddition(address));
                }
                if ports.len() == 2 {
                    return Some(RewriteRule::RemoveBinaryAddition(
                        address,
                        (ports[0].clone(), ports[1].clone()),
                    ));
                }
                for (port_index, target_port) in ports.iter().enumerate() {
                    if let Some(Node::Unrestricted(_)) = diagram.nodes.get(&target_port.0) {
                        return Some(RewriteRule::AddMulUnrestricted(address, port_index));
                    }
                    if port_index > 0 {
                        if let Some(Node::Addition(_)) = diagram.nodes.get(&target_port.0) {
                            if target_port.1 == 0 {
                                return Some(RewriteRule::FuseAddition(address, port_index));
                            }
                        }
                        if let Some(Node::AddConstant(_, _, _)) = diagram.nodes.get(&target_port.0)
                        {
                            return Some(RewriteRule::AdditionConstAddition(address, port_index));
                        }
                        if let Some(Node::Constant(_, _)) = diagram.nodes.get(&target_port.0) {
                            return Some(RewriteRule::AdditionConst(address, port_index));
                        }
                    }
                }
                None
            }
            Node::Multiplication(ports) => {
                if ports.len() == 1 {
                    return Some(RewriteRule::RemoveUnaryMultiplication(address));
                }
                if ports.len() == 2 {
                    return Some(RewriteRule::RemoveBinaryMultiplication(
                        address,
                        (ports[0].clone(), ports[1].clone()),
                    ));
                }
                for (port_index, target_port) in ports.iter().enumerate() {
                    if port_index == 0 {
                        if let Some(Node::Unrestricted(_)) = diagram.nodes.get(&target_port.0) {
                            return Some(RewriteRule::AddMulUnrestricted(address, port_index));
                        }
                    }
                    if port_index > 0 {
                        if let Some(Node::Multiplication(_)) = diagram.nodes.get(&target_port.0) {
                            if target_port.1 == 0 {
                                return Some(RewriteRule::FuseMultiplication(address, port_index));
                            }
                        }

                        if let Some(Node::MultiplyConstant(val, _, _)) =
                            diagram.nodes.get(&target_port.0)
                        {
                            if val != &BigInt::from(0) {
                                return Some(RewriteRule::MultiplicationConstMultiplication(
                                    address, port_index,
                                ));
                            }
                        }
                        if let Some(Node::Constant(_, _)) = diagram.nodes.get(&target_port.0) {
                            return Some(RewriteRule::MultiplicationConst(address, port_index));
                        }
                    }
                }
                None
            }
            Node::AddConstant(value, port1, port2) => {
                if value == BigInt::from(0) {
                    return Some(RewriteRule::AddConstantZero(address, (port1, port2)));
                }
                if let Some(Node::Constant(_, _)) = diagram.nodes.get(&port1.0) {
                    return Some(RewriteRule::AddConstantConstantHead(address));
                }
                if let Some(Node::Constant(_, _)) = diagram.nodes.get(&port2.0) {
                    return Some(RewriteRule::AddConstantConstantTail(address));
                }
                if let Some(Node::Unrestricted(_)) = diagram.nodes.get(&port1.0) {
                    return Some(RewriteRule::DeleteConstOpUnrestricted(address, port1.1));
                }
                if let Some(Node::Unrestricted(_)) = diagram.nodes.get(&port2.0) {
                    return Some(RewriteRule::DeleteConstOpUnrestricted(address, port2.1));
                }
                if let Some(Node::AddConstant(_, _, _)) = diagram.nodes.get(&port1.0) {
                    if port1.1 == 0 {
                        return Some(RewriteRule::FuseAdditionByConstantHdHd(address));
                    }
                }
                if let Some(Node::AddConstant(_, _, _)) = diagram.nodes.get(&port2.0) {
                    if port2.1 == 0 {
                        return Some(RewriteRule::FuseAdditionByConstantHdTl(address));
                    }
                    if port2.1 == 1 {
                        return Some(RewriteRule::FuseAdditionByConstantTlTl(address));
                    }
                }
                if let Some(Node::MultiplyConstant(_, _, _)) = diagram.nodes.get(&port2.0) {
                    if port2.1 == 1 {
                        return Some(RewriteRule::SwapAddMulConstantsTlTl(address));
                    }
                }
                if let Some(Node::MultiplyConstant(_, _, _)) = diagram.nodes.get(&port1.0) {
                    if port1.1 == 1 {
                        return Some(RewriteRule::SwapAddMulConstantsHdTl(address));
                    }
                }
                None
            }
            Node::MultiplyConstant(value, port1, port2) => {
                if value == BigInt::from(0) {
                    return Some(RewriteRule::MulConstantZero(
                        address,
                        (port1, port2),
                        diagram.next_address,
                    ));
                }
                if value == BigInt::from(1) {
                    return Some(RewriteRule::MulConstantOne(address, (port1, port2)));
                }
                if let Some(Node::Constant(_, _)) = diagram.nodes.get(&port1.0) {
                    return Some(RewriteRule::MulConstantConstantHead(address));
                }
                if let Some(Node::Constant(_, _)) = diagram.nodes.get(&port2.0) {
                    return Some(RewriteRule::MulConstantConstantTail(address));
                }
                if let Some(Node::Unrestricted(_)) = diagram.nodes.get(&port1.0) {
                    return Some(RewriteRule::DeleteConstOpUnrestricted(address, port1.1));
                }
                if let Some(Node::Unrestricted(_)) = diagram.nodes.get(&port2.0) {
                    if value != BigInt::from(0) {
                        return Some(RewriteRule::DeleteConstOpUnrestricted(address, port2.1));
                    }
                }
                if let Some(Node::MultiplyConstant(_, _, _)) = diagram.nodes.get(&port1.0) {
                    if port2.1 == 0 {
                        return Some(RewriteRule::FuseMultiplicationByConstantHdHd(address));
                    }
                }
                if let Some(Node::MultiplyConstant(_, _, _)) = diagram.nodes.get(&port2.0) {
                    if port2.1 == 0 {
                        return Some(RewriteRule::FuseMultiplicationByConstantHdTl(address));
                    }
                    if port2.1 == 1 {
                        return Some(RewriteRule::FuseMultiplicationByConstantTlTl(address));
                    }
                }
                None
            }
            Node::ExponentiateConstant(value, port1, port2) => {
                if value == BigInt::from(1) {
                    return Some(RewriteRule::ExpConstantOne(address, (port1, port2)));
                }
                if let Some(Node::Constant(_, _)) = diagram.nodes.get(&port2.0) {
                    return Some(RewriteRule::ExpConstantConstantTail(address));
                }

                if let Some(Node::Unrestricted(_)) = diagram.nodes.get(&port1.0) {
                    if value != BigInt::from(0) {
                        return Some(RewriteRule::DeleteConstOpUnrestricted(address, port1.1));
                    }
                }

                // ??? if let Some(Node::Unrestricted(_)) = diagram.nodes.get(&port2.0) ????
                if let Some(Node::MultiplyConstant(_, _, _)) = diagram.nodes.get(&port2.0) {
                    if port2.1 == 0 {
                        return Some(RewriteRule::FuseExponentiationByConstantHdTl(address));
                    }
                }
                None
            }
            Node::Constant(_, port) => {
                if let Some(Node::Constant(_, _)) = diagram.nodes.get(&port.0) {
                    return Some(RewriteRule::ConstantConstantRemoval(address, port.0));
                }
                None
            }
            Node::Unrestricted(port) => {
                if let Some(Node::Constant(_, _) | Node::Unrestricted(_)) =
                    diagram.nodes.get(&port.0)
                {
                    return Some(RewriteRule::UnrestrictedUnaryRemoval(address, port.0));
                }
                None
            }
            _ => None,
        }
    } else {
        None
    }
}

// Apply a rewrite rule and return effected addresses
pub fn apply_rewrite_step(
    diagram: &mut StringDiagram,
    field_ops: &dyn FieldOps,
    rule: RewriteRule,
) -> Vec<Address> {
    match rule {
        RewriteRule::DeleteEmptyEquality(address) => {
            diagram.nodes.remove(&address);
            vec![]
        }
        RewriteRule::RemoveUnaryEquality(address) => {
            if let Some(Node::Equality(_, ports)) = diagram.nodes.get(&address).cloned() {
                diagram
                    .nodes
                    .insert(address, Node::Unrestricted(ports.get(0).unwrap().clone()));
            }
            vec![]
        }
        RewriteRule::RemoveUnaryAddition(address) => {
            if let Some(Node::Addition(ports)) = diagram.nodes.get(&address).cloned() {
                diagram.nodes.insert(
                    address,
                    Node::Constant(BigInt::from(0), ports.get(0).unwrap().clone()),
                );
            }
            vec![]
        }
        RewriteRule::RemoveUnaryMultiplication(address) => {
            if let Some(Node::Multiplication(ports)) = diagram.nodes.get(&address).cloned() {
                diagram.nodes.insert(
                    address,
                    Node::Constant(BigInt::from(1), ports.get(0).unwrap().clone()),
                );
            }
            vec![]
        }
        RewriteRule::SplitAddition(address) => {
            split_addition_node(diagram, address);
            vec![address]
        }
        RewriteRule::SplitMultiplication(address) => {
            split_multiplication_node(diagram, address);
            vec![address]
        }
        RewriteRule::SplitExponentiation(address) => {
            split_exponentiation_node(diagram, address);
            vec![address]
        }
        RewriteRule::FuseEquality(address, port_index) => {
            fuse_equality_nodes(diagram, address, port_index);

            vec![address]
        }
        RewriteRule::FuseAddition(address, port_index) => {
            fuse_addition_nodes(diagram, address, port_index);
            vec![address]
        }
        RewriteRule::RemoveBinaryAddition(address, _) => {
            if let Some(Node::Addition(ports)) = diagram.nodes.get(&address).cloned() {
                remove_binary_node(diagram, address);

                vec![ports[0].0, ports[1].0]
            } else {
                vec![]
            }
        }
        RewriteRule::FuseMultiplication(address, port_index) => {
            fuse_multiplication_nodes(diagram, address, port_index);
            vec![address]
        }
        RewriteRule::RemoveBinaryMultiplication(address, _) => {
            if let Some(Node::Multiplication(ports)) = diagram.nodes.get(&address).cloned() {
                remove_binary_node(diagram, address);

                vec![ports[0].0, ports[1].0]
            } else {
                vec![]
            }
        }
        RewriteRule::AddConstantZero(address, _) => {
            if let Some(Node::AddConstant(_, port1, port2)) = diagram.nodes.get(&address).cloned() {
                remove_binary_node(diagram, address);
                vec![port1.0, port2.0]
            } else {
                vec![]
            }
        }
        RewriteRule::AddConstantConstantHead(address) => {
            add_constant_constant_head(diagram, address, field_ops);
            vec![address]
        }
        RewriteRule::AddConstantConstantTail(address) => {
            add_constant_constant_tail(diagram, address, field_ops);
            vec![address]
        }
        RewriteRule::FuseAdditionByConstantHdTl(address) => {
            fuse_addition_by_constant_hd_tl(diagram, address, field_ops);
            vec![address]
        }
        RewriteRule::FuseAdditionByConstantTlTl(address) => {
            fuse_addition_by_constant_tl_tl(diagram, address, field_ops);
            vec![address]
        }
        RewriteRule::FuseAdditionByConstantHdHd(address) => {
            fuse_addition_by_constant_hd_hd(diagram, address, field_ops);
            vec![address]
        }
        RewriteRule::EqualityUnrestricted(address, port_index) => {
            equality_unrestricted(diagram, address, port_index);
            vec![address]
        }
        RewriteRule::RemoveBinaryEquality(address, _) => {
            if let Some(Node::Equality(_, ports)) = diagram.nodes.get(&address).cloned() {
                remove_binary_node(diagram, address);

                vec![ports[0].0, ports[1].0]
            } else {
                vec![]
            }
        }
        RewriteRule::AdditionConstAddition(address, port_index) => {
            if let Some(Node::Addition(ports)) = diagram.nodes.get(&address).cloned() {
                let target_port = &ports[port_index];
                addition_const_addition(diagram, address, port_index, field_ops);
                vec![address, target_port.0]
            } else {
                vec![]
            }
        }
        RewriteRule::MultiplicationConstMultiplication(address, port_index) => {
            if let Some(Node::Multiplication(ports)) = diagram.nodes.get(&address).cloned() {
                let target_port = &ports[port_index];
                multiplication_const_multiplication(diagram, address, port_index, field_ops);
                vec![address, target_port.0]
            } else {
                vec![]
            }
        }
        RewriteRule::AdditionConst(address, port_index) => {
            if let Some(Node::Addition(ports)) = diagram.nodes.get(&address).cloned() {
                let target_port = &ports[port_index];
                addition_const(diagram, address, port_index);
                vec![address, target_port.0]
            } else {
                vec![]
            }
        }
        RewriteRule::MultiplicationConst(address, port_index) => {
            if let Some(Node::Multiplication(ports)) = diagram.nodes.get(&address).cloned() {
                let target_port = &ports[port_index];
                multiplication_const(diagram, address, port_index);
                vec![address, target_port.0]
            } else {
                vec![]
            }
        }
        RewriteRule::EqualityConst(address, port_index) => {
            if let Some(Node::Equality(_, ports)) = diagram.nodes.get(&address).cloned() {
                equality_const(diagram, address, port_index);

                return ports.iter().map(|p| p.0).collect();
            } else {
                vec![]
            }
        }
        RewriteRule::AddMulUnrestricted(address, port_index) => {
            if let Some(Node::Addition(ports) | Node::Multiplication(ports)) =
                diagram.nodes.get(&address).cloned()
            {
                addmul_unrestricted(diagram, address, port_index);
                return ports.iter().map(|p| p.0).collect();
            } else {
                vec![]
            }
        }
        RewriteRule::DeleteConstOpUnrestricted(address, port_index) => {
            delete_const_op_unrestricted(diagram, address, port_index);
            vec![address]
        }
        RewriteRule::SwapAddMulConstantsHdTl(address) => {
            if let Some(Node::AddConstant(_, port1, _)) = diagram.nodes.get(&address).cloned() {
                swap_add_and_multiply_constants_hd_tl(diagram, address, field_ops);
                vec![address, port1.0]
            } else {
                vec![]
            }
        }
        RewriteRule::SwapAddMulConstantsTlTl(address) => {
            if let Some(Node::AddConstant(_, _, port2)) = diagram.nodes.get(&address).cloned() {
                swap_add_and_multiply_constants_tl_tl(diagram, address, field_ops);
                vec![address, port2.0]
            } else {
                vec![]
            }
        }
        RewriteRule::MulConstantZero(address, _, _) => {
            if let Some(Node::MultiplyConstant(_, port1, port2)) =
                diagram.nodes.get(&address).cloned()
            {
                mul_constant_zero(diagram, address);
                vec![port1.0, port2.0]
            } else {
                vec![]
            }
        }
        RewriteRule::MulConstantOne(address, _) => {
            if let Some(Node::MultiplyConstant(_, port1, port2)) =
                diagram.nodes.get(&address).cloned()
            {
                remove_binary_node(diagram, address);
                vec![port1.0, port2.0]
            } else {
                vec![]
            }
        }
        RewriteRule::MulConstantConstantHead(address) => {
            mul_constant_constant_head(diagram, address, field_ops);
            vec![address]
        }
        RewriteRule::MulConstantConstantTail(address) => {
            mul_constant_constant_tail(diagram, address, field_ops);
            vec![address]
        }
        RewriteRule::FuseMultiplicationByConstantHdTl(address) => {
            fuse_multiplication_by_constant_hd_tl(diagram, address, field_ops);
            vec![address]
        }
        RewriteRule::FuseMultiplicationByConstantTlTl(address) => {
            fuse_multiplication_by_constant_tl_tl(diagram, address, field_ops);
            vec![address]
        }
        RewriteRule::FuseMultiplicationByConstantHdHd(address) => {
            fuse_multiplication_by_constant_hd_hd(diagram, address, field_ops);
            vec![address]
        }
        RewriteRule::ExpConstantOne(address, _) => {
            if let Some(Node::ExponentiateConstant(_, port1, port2)) =
                diagram.nodes.get(&address).cloned()
            {
                remove_binary_node(diagram, address);
                vec![port1.0, port2.0]
            } else {
                vec![]
            }
        }
        RewriteRule::ExpConstantConstantTail(address) => {
            exp_constant_constant_tail(diagram, address, field_ops);
            vec![address]
        }
        RewriteRule::FuseExponentiationByConstantHdTl(address) => {
            fuse_exponentiation_by_constant_hd_tl(diagram, address, field_ops);
            vec![address]
        }
        RewriteRule::ConstantConstantRemoval(address, _) => {
            constant_constant_removal(diagram, address);
            vec![]
        }
        RewriteRule::UnrestrictedUnaryRemoval(address1, address2) => {
            diagram.nodes.remove(&address1);
            diagram.nodes.remove(&address2);
            vec![]
        }
        RewriteRule::ReduceCVVAddition(address, _) => {
            cvv_addmul(diagram, address);
            vec![]
        }
        RewriteRule::ReduceCVVMultiplication(address, _) => {
            cvv_addmul(diagram, address);
            vec![]
        }
    }
}

// Try applying a single simplification step to an address.
// Return a list of *still existing* addresses of nodes that have been modified.
fn simplify_string_diagram_step(
    diagram: &mut StringDiagram,
    field_ops: &dyn FieldOps,
    address: Address,
) -> (Option<RewriteRule>, Vec<Address>) {
    let step = gen_string_diagram_step(diagram, address);

    if let Some(rule) = &step {
        let res = apply_rewrite_step(diagram, field_ops, rule.clone());
        (step, res)
    } else {
        (None, vec![])
    }
}

fn simplify_string_diagram(diagram: &mut StringDiagram, field_ops: &dyn FieldOps) -> RewriteTrace {
    // Initialize the vector with all the addresses in the diagram.
    let mut addresses_to_process: Vec<Address> = diagram.nodes.keys().cloned().collect();
    let mut trace: RewriteTrace = Vec::new();

    while !addresses_to_process.is_empty() {
        let mut new_addresses = HashSet::new();

        for address in addresses_to_process {
            // Apply a single simplification step to the address.
            let (rule, modified_addresses) =
                simplify_string_diagram_step(diagram, field_ops, address);
            // Add the modified addresses to the hash set.
            for modified_address in modified_addresses {
                new_addresses.insert(modified_address);
                // Look up any connecting addresses and add them to the hash set.
                let connected_addresses = diagram.address_list(&modified_address);
                for connected_address in connected_addresses {
                    new_addresses.insert(connected_address);
                }
            }

            if let Some(rule) = rule {
                trace.push(rule)
            }
        }

        // Convert the HashSet to a Vec for the next iteration.
        addresses_to_process = new_addresses.into_iter().collect();
    }

    trace
}

// If we have c = v1 + v2 or c = v1 * v2, we can avoid making a new variable
// by obsorbing the conatant into a bespoke node.
fn cvv_addmul(diagram: &mut StringDiagram, address: Address) {
    // Clone the node
    let node = if let Some(node) = diagram.nodes.get(&address) {
        node.clone()
    } else {
        return;
    };

    let (head_port, tail1_port, tail2_port) = match &node {
        // Check if it's an Addition or Multiplication node with exactly 3 ports
        Node::Addition(ports) | Node::Multiplication(ports) if ports.len() == 3 => {
            // Get the ports
            (ports[0].clone(), ports[1].clone(), ports[2].clone())
        }
        _ => return,
    };

    // Check if the head is pointing at a Constant node
    if let Some(Node::Constant(value, _)) = diagram.nodes.get(&head_port.0) {
        // Replace the Addition/Multiplication node with a CVVAdd/CVVMul node
        let new_node = match &node {
            Node::Addition(_) => {
                Node::CVVAdd(value.clone(), tail1_port.clone(), tail2_port.clone())
            }
            Node::Multiplication(_) => {
                Node::CVVMul(value.clone(), tail1_port.clone(), tail2_port.clone())
            }
            _ => return,
        };

        // Update the node in the diagram
        diagram.nodes.insert(address, new_node);

        // Update the linking ports of the new node
        diagram.replace_port(&tail1_port, &Port(address, 0));
        diagram.replace_port(&tail2_port, &Port(address, 1));

        // Now that we have updated the node and linking ports, we can safely remove the Constant node
        diagram.nodes.remove(&head_port.0);
    }
}

// Ensure there aren't any additions/multiplications with more than 2 inputs
// And also that there aren't any exponential nodes.
fn prep_for_3ac(diagram: &mut StringDiagram) -> RewriteTrace {
    let mut changed = true;
    let mut trace: RewriteTrace = vec![];

    while changed {
        changed = false;

        // Collect the keys into a Vec
        let current_node_keys: Vec<_> = diagram.nodes.keys().cloned().collect();

        // Now iterate through the node keys
        for prime_node_address in current_node_keys {
            // It's a good practice to check if the node still exists, in case it was deleted
            if let Some(node) = diagram.nodes.get(&prime_node_address).cloned() {
                match node {
                    Node::Addition(ports) => {
                        // If there are more than three ports in the addition node, decompose it
                        if ports.len() > 3 {
                            trace.extend(decompose_addition_node(diagram, prime_node_address));
                            changed = true;
                        }
                        if ports.len() == 3 {
                            if let Some(Node::Constant(_, _)) = diagram.nodes.get(&ports[0].0) {
                                cvv_addmul(diagram, prime_node_address);
                                trace.push(RewriteRule::ReduceCVVAddition(
                                    prime_node_address,
                                    ports[0].clone(),
                                ));
                                changed = true;
                            }
                        }
                    }
                    Node::Multiplication(ports) => {
                        // If there are more than three ports in the multiplication node, decompose it
                        if ports.len() > 3 {
                            trace
                                .extend(decompose_multiplication_node(diagram, prime_node_address));
                            changed = true;
                        }
                        if ports.len() == 3 {
                            if let Some(Node::Constant(_, _)) = diagram.nodes.get(&ports[0].0) {
                                cvv_addmul(diagram, prime_node_address);
                                trace.push(RewriteRule::ReduceCVVMultiplication(
                                    prime_node_address,
                                    ports[0].clone(),
                                ));
                                changed = true;
                            }
                        }
                    }
                    Node::ExponentiateConstant(_, _, _) => {
                        trace.extend(split_exponentiation_node(diagram, prime_node_address));
                        changed = true;
                    }
                    _ => {}
                }
            }
        }
    }

    trace
}

// Update definitions after binary node is removed
fn binary_removal_update(
    defs: &mut DefinitionRegistry,
    address: &Address,
    port1: &Port,
    port2: &Port,
) {
    // Get the id of the definition both remaining ports will be assinged to
    let main_id = *defs.port_id_map.get(port1).unwrap();

    // Remove the ansciliary connection (second port will be added back later)
    defs.remove_ports((&Port(*address, 1), port2));

    // Remove principal port, and add second port to definition
    defs.port_id_map.remove(&Port(*address, 0));
    if let Some((ports, _)) = defs.id_def_map.get_mut(&main_id) {
        ports.remove(&Port(*address, 0));
        ports.insert(port2.clone());
    }

    // Map second port to its new definition
    defs.port_id_map.insert(port2.clone(), main_id);
}

fn calculate_defs(defs: &mut DefinitionRegistry, rule: RewriteRule) {
    match rule {
        RewriteRule::DeleteEmptyEquality(_address) => {
            // Nothing to do; no defs need to be modified
        }
        RewriteRule::RemoveUnaryEquality(_address) => {
            // Nothing to do; no defs need to be modified
        }
        RewriteRule::RemoveUnaryAddition(address) => {
            // Overwrite old definition with constant expression
            defs.replace_definition_of_port(&Port(address, 0), Expr::Constant(BigInt::from(0)));
        }
        RewriteRule::RemoveUnaryMultiplication(address) => {
            // Overwrite old definition with constant expression
            defs.replace_definition_of_port(&Port(address, 0), Expr::Constant(BigInt::from(1)));
        }
        RewriteRule::SplitAddition(_address) => {
            // Not implemented
        }
        RewriteRule::SplitMultiplication(_address) => {
            // Not implemented
        }
        RewriteRule::SplitExponentiation(_address) => {
            // Not implemented
        }
        RewriteRule::FuseEquality(_address, _port_index) => {
            // Not implemented
        }
        RewriteRule::FuseAddition(_address, _port_index) => {
            // Not implemented
        }
        RewriteRule::RemoveBinaryAddition(address, (port1, port2)) => {
            binary_removal_update(defs, &address, &port1, &port2);
        }
        RewriteRule::FuseMultiplication(_address, _port_index) => {
            // Not implemented
        }
        RewriteRule::RemoveBinaryMultiplication(address, (port1, port2)) => {
            binary_removal_update(defs, &address, &port1, &port2);
        }
        RewriteRule::AddConstantZero(address, (port1, port2)) => {
            binary_removal_update(defs, &address, &port1, &port2);
        }
        RewriteRule::AddConstantConstantHead(_address) => {
            // Not implemented
        }
        RewriteRule::AddConstantConstantTail(_address) => {
            // Not implemented
        }
        RewriteRule::FuseAdditionByConstantHdTl(_address) => {
            // Not implemented
        }
        RewriteRule::FuseAdditionByConstantTlTl(_address) => {
            // Not implemented
        }
        RewriteRule::FuseAdditionByConstantHdHd(_address) => {
            // Not implemented
        }
        RewriteRule::EqualityUnrestricted(_address, _port_index) => {
            // Not implemented
        }
        RewriteRule::RemoveBinaryEquality(address, (port1, port2)) => {
            binary_removal_update(defs, &address, &port1, &port2);
        }
        RewriteRule::AdditionConstAddition(_address, _port_index) => {
            // Not implemented
        }
        RewriteRule::MultiplicationConstMultiplication(_address, _port_index) => {
            // Not implemented
        }
        RewriteRule::AdditionConst(_address, _port_index) => {
            // Not implemented
        }
        RewriteRule::MultiplicationConst(_address, _port_index) => {
            // Not implemented
        }
        RewriteRule::EqualityConst(_address, _port_index) => {
            // Not implemented
        }
        RewriteRule::AddMulUnrestricted(_address, _port_index) => {
            // Not implemented
        }
        RewriteRule::DeleteConstOpUnrestricted(_address, _port_index) => {
            // Not implemented
        }
        RewriteRule::SwapAddMulConstantsHdTl(_address) => {
            // Not implemented
        }
        RewriteRule::SwapAddMulConstantsTlTl(_address) => {
            // Not implemented
        }
        RewriteRule::MulConstantZero(_address, (_port1, _port2), _unr_addr) => {
            // Not implemented
        }
        RewriteRule::MulConstantOne(address, (port1, port2)) => {
            binary_removal_update(defs, &address, &port1, &port2);
        }
        RewriteRule::MulConstantConstantHead(_address) => {
            // Not implemented
        }
        RewriteRule::MulConstantConstantTail(_address) => {
            // Not implemented
        }
        RewriteRule::FuseMultiplicationByConstantHdTl(_address) => {
            // Not implemented
        }
        RewriteRule::FuseMultiplicationByConstantTlTl(_address) => {
            // Not implemented
        }
        RewriteRule::FuseMultiplicationByConstantHdHd(_address) => {
            // Not implemented
        }
        RewriteRule::ExpConstantOne(address, (port1, port2)) => {
            binary_removal_update(defs, &address, &port1, &port2);
        }
        RewriteRule::ExpConstantConstantTail(_address) => {
            // Not implemented
        }
        RewriteRule::FuseExponentiationByConstantHdTl(_address) => {
            // Not implemented
        }
        RewriteRule::ConstantConstantRemoval(address1, address2) => {
            defs.remove_ports((&Port(address1, 0), &Port(address2, 0)));
        }
        RewriteRule::UnrestrictedUnaryRemoval(address1, address2) => {
            defs.remove_ports((&Port(address1, 0), &Port(address2, 0)));
        }
        RewriteRule::ReduceCVVAddition(add_address, const_port) => {
            defs.remove_ports((&Port(add_address, 0), &const_port));
        }
        RewriteRule::ReduceCVVMultiplication(mul_address, const_port) => {
            defs.remove_ports((&Port(mul_address, 0), &const_port));
        }
    }
}

fn synthesize_defs(defs: &mut DefinitionRegistry, trace: RewriteTrace) -> Vec<Definition> {
    for rule in trace {
        calculate_defs(defs, rule)
    }

    defs.compile_definitions()
}

pub fn simplify_3ac(
    equations: &mut Vec<TExpr>,
    input_ids: &HashSet<u32>,
    defs: &mut Vec<Definition>,
    field_ops: &dyn FieldOps,
) {
    let mut reg: DefinitionRegistry = DefinitionRegistry::new(defs.to_vec());
    defs.clear();
    let mut diag: StringDiagram = build_string_diagram(equations.to_vec(), input_ids, &mut reg);

    equations.clear();
    let mut trace = simplify_string_diagram(&mut diag, field_ops);
    println!("Converting back into 3AC...");
    trace.extend(prep_for_3ac(&mut diag));
    equations.extend(convert_to_3ac(&diag, &reg));
    defs.extend(synthesize_defs(&mut reg, trace));
}
