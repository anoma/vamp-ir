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
pub type VarId = u32;
pub type Address = usize;
pub type PortIndex = usize;

// Define a port as a pair of node address and a list index
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Port(pub Address, pub PortIndex, pub VarId);

impl Port {
    pub fn location(&self) -> (Address, PortIndex) {
        (self.0, self.1)
    }
}

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
                            println!("{node:?}");
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
                        println!("{node:?}");
                        return false;
                    }
                }
                Node::Unrestricted(port) | Node::Constant(_, port) => {
                    if !self.is_connected_back(port, (*address, 0)) {
                        println!("{node:?}");
                        return false;
                    }
                }
                Node::Contradiction => {}
            }
        }
        true
    }

    // Helper function to check if the port is connected back to the original (address, port_index).
    // Also checks that every port link has a single, unique variable id.
    fn is_connected_back(&self, port: &Port, original: (Address, PortIndex)) -> bool {
        let Port(target_address, target_port_index, var_id) = port;
        // Port should not link to itself
        if target_address == &original.0 && target_port_index == &original.1 {
            return false;
        }
        if let Some(target_node) = self.nodes.get(target_address) {
            match target_node {
                Node::Addition(ports) | Node::Multiplication(ports) | Node::Equality(_, ports) => {
                    if let Some(Port(back_address, back_port_index, var_id_2)) =
                        ports.get(*target_port_index)
                    {
                        return *back_address == original.0
                            && *back_port_index == original.1
                            && var_id == var_id_2;
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
                    return back_port.0 == original.0
                        && back_port.1 == original.1
                        && back_port.2 == *var_id;
                }
                Node::Unrestricted(port) | Node::Constant(_, port) => {
                    return port.0 == original.0 && port.1 == original.1 && port.2 == *var_id;
                }
                Node::Contradiction => {}
            }
        }
        false
    }

    // Replace the port *at* target_port *with* new_port.
    fn replace_port(
        &mut self,
        (target_address, target_index): &(Address, PortIndex),
        new_port: &Port,
    ) {
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
    pub id_def_map: HashMap<VarId, Box<TExpr>>,
    pub next_id: VarId,
}

impl DefinitionRegistry {
    pub fn new(defs: &mut Vec<Definition>) -> Self {
        let mut id_def_map: HashMap<u32, Box<TExpr>> = HashMap::new();

        // Using drain to move elements out of defs into id_def_map while finding max_id
        let max_id = defs
            .drain(..)
            .filter_map(|def| match def {
                Definition(LetBinding(
                    TPat {
                        v: Pat::Variable(Variable { id, .. }),
                        ..
                    },
                    e,
                )) => {
                    id_def_map.insert(id, e);
                    Some(id)
                }
                _ => None,
            })
            .max()
            .unwrap_or(0)
            + 1;

        DefinitionRegistry {
            id_def_map,
            next_id: max_id,
        }
    }

    // Destructively empty synthesized definitions into original definition vector
    fn compile_definitions(&mut self) -> Vec<Definition> {
        let mut compiled_defs = vec![];

        for (id, expr) in self.id_def_map.drain() {
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

    // Destructively empty synthesized definitions into original definition vector
    fn get_next_var_id(&mut self) -> u32 {
        let new_id = self.next_id;

        self.next_id += 1;

        new_id
    }

    fn register_definition(&mut self, definition: Expr) -> VarId {
        // Insert the definition into the id_def_map
        let boxed_def = Box::new(TExpr {
            v: definition,
            t: None,
        });
        self.id_def_map.insert(self.next_id, boxed_def);

        // Increment the next_id
        self.get_next_var_id()
    }
}

fn get_or_create_equality_node(
    variable: &Variable,
    pointing_port: Port,
    diagram: &mut StringDiagram,
    variable_addresses: &mut HashMap<VariableId, Address>,
    input_ids: &HashSet<VarId>,
) -> (Address, PortIndex) {
    // Does this variable already have an address? If not, make one.
    let equality_address = *variable_addresses.entry(variable.id).or_insert_with(|| {
        // If we're looking at an input variable, store it.
        let input_vec = if input_ids.contains(&variable.id) {
            vec![variable.clone()]
        } else {
            vec![]
        };

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

    (equality_address, next_idx)
}

// Given a list of 3ac equations, generate a semantically equivalent string diagram.
pub fn build_string_diagram(
    equations: Vec<TExpr>,
    input_ids: &HashSet<VarId>,
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
                        Port(target_address, 0, var.id),
                        &mut diagram,
                        &mut variable_addresses,
                        input_ids,
                    );

                    let _const_addr = diagram.add_node(Node::Constant(
                        c.clone(),
                        Port(equality_address, new_port_index, var.id),
                    ));
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
                        Port(target_address, 0, var1.id),
                        &mut diagram,
                        &mut variable_addresses,
                        input_ids,
                    );
                    let (equality_address2, new_port_index2) = get_or_create_equality_node(
                        var2,
                        Port(target_address, 1, var2.id),
                        &mut diagram,
                        &mut variable_addresses,
                        input_ids,
                    );

                    // Connect these nodes by adding an equality node that points to both.
                    diagram.add_node(Node::Equality(
                        vec![],
                        vec![
                            Port(equality_address1, new_port_index1, var1.id),
                            Port(equality_address2, new_port_index2, var2.id),
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
                            Port(target_address, 0, var1.id),
                            &mut diagram,
                            &mut variable_addresses,
                            input_ids,
                        );
                        let (equality_address2, new_port_index2) = get_or_create_equality_node(
                            var2,
                            Port(target_address, 1, var2.id),
                            &mut diagram,
                            &mut variable_addresses,
                            input_ids,
                        );

                        diagram.add_node(Node::MultiplyConstant(
                            (-1).into(),
                            Port(equality_address1, new_port_index1, var1.id),
                            Port(equality_address2, new_port_index2, var2.id),
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
                                    Port(target_address, 1, var.id),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                );
                                let (address1, port_index1) = get_or_create_equality_node(
                                    term_var1,
                                    Port(target_address, 0, term_var1.id),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                );
                                let (address2, port_index2) = get_or_create_equality_node(
                                    term_var2,
                                    Port(target_address, 2, term_var2.id),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                );
                                ports = vec![
                                    Port(address1, port_index1, term_var1.id),
                                    Port(address0, port_index0, var.id),
                                    Port(address2, port_index2, term_var2.id),
                                ];
                            } else if matches!(op, InfixOp::Add) || matches!(op, InfixOp::Multiply)
                            {
                                let (address0, port_index0) = get_or_create_equality_node(
                                    var,
                                    Port(target_address, 0, var.id),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                );
                                let (address1, port_index1) = get_or_create_equality_node(
                                    term_var1,
                                    Port(target_address, 1, term_var1.id),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                );
                                let (address2, port_index2) = get_or_create_equality_node(
                                    term_var2,
                                    Port(target_address, 2, term_var2.id),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                );
                                ports = vec![
                                    Port(address0, port_index0, var.id),
                                    Port(address1, port_index1, term_var1.id),
                                    Port(address2, port_index2, term_var2.id),
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
                                    Port(target_address, 1, var.id),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                );
                                let address1 = diagram.add_node(Node::Constant(
                                    const1.clone(),
                                    Port(target_address, 0, var.id),
                                ));
                                let (address2, port_index2) = get_or_create_equality_node(
                                    term_var2,
                                    Port(target_address, 2, term_var2.id),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                );
                                ports = vec![
                                    Port(address1, 0, var.id),
                                    Port(address0, port_index0, var.id),
                                    Port(address2, port_index2, term_var2.id),
                                ];
                            } else if matches!(op, InfixOp::Add) || matches!(op, InfixOp::Multiply)
                            {
                                let (address0, port_index0) = get_or_create_equality_node(
                                    var,
                                    Port(target_address, 0, var.id),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                );
                                let address1 = diagram.add_node(Node::Constant(
                                    const1.clone(),
                                    Port(target_address, 1, var.id),
                                ));
                                let (address2, port_index2) = get_or_create_equality_node(
                                    term_var2,
                                    Port(target_address, 2, term_var2.id),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                );
                                ports = vec![
                                    Port(address0, port_index0, var.id),
                                    Port(address1, 0, var.id),
                                    Port(address2, port_index2, term_var2.id),
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
                                    Port(target_address, 1, var.id),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                );
                                let (address1, port_index1) = get_or_create_equality_node(
                                    term_var1,
                                    Port(target_address, 0, term_var1.id),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                );
                                let address2 = diagram.add_node(Node::Constant(
                                    const2.clone(),
                                    Port(target_address, 2, var.id),
                                ));
                                ports = vec![
                                    Port(address1, port_index1, term_var1.id),
                                    Port(address0, port_index0, var.id),
                                    Port(address2, 0, var.id),
                                ];
                            } else if matches!(op, InfixOp::Add) || matches!(op, InfixOp::Multiply)
                            {
                                let (address0, port_index0) = get_or_create_equality_node(
                                    var,
                                    Port(target_address, 0, var.id),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                );
                                let (address1, port_index1) = get_or_create_equality_node(
                                    term_var1,
                                    Port(target_address, 1, term_var1.id),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                );
                                let address2 = diagram.add_node(Node::Constant(
                                    const2.clone(),
                                    Port(target_address, 2, var.id),
                                ));
                                ports = vec![
                                    Port(address0, port_index0, var.id),
                                    Port(address1, port_index1, term_var1.id),
                                    Port(address2, 0, var.id),
                                ];
                            } else {
                                panic!("Invalid operation encountered: {op}")
                            }
                        }
                        (Expr::Constant(const1), Expr::Constant(const2)) => {
                            // Should this ever happen?
                            target_address = diagram.next_address + new_var_ids.len() + 2;

                            if matches!(op, InfixOp::Subtract) || matches!(op, InfixOp::Divide) {
                                let (address0, port_index0) = get_or_create_equality_node(
                                    var,
                                    Port(target_address, 1, var.id),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                );
                                let const_id1 =
                                    defs.register_definition(Expr::Constant(const1.clone()));
                                let address1 = diagram.add_node(Node::Constant(
                                    const1.clone(),
                                    Port(target_address, 0, const_id1),
                                ));
                                let const_id2 =
                                    defs.register_definition(Expr::Constant(const2.clone()));
                                let address2 = diagram.add_node(Node::Constant(
                                    const2.clone(),
                                    Port(target_address, 2, const_id2),
                                ));
                                ports = vec![
                                    Port(address1, 0, const_id1),
                                    Port(address0, port_index0, var.id),
                                    Port(address2, 0, const_id2),
                                ];
                            } else if matches!(op, InfixOp::Add) || matches!(op, InfixOp::Multiply)
                            {
                                let (address0, port_index0) = get_or_create_equality_node(
                                    var,
                                    Port(target_address, 0, var.id),
                                    &mut diagram,
                                    &mut variable_addresses,
                                    input_ids,
                                );
                                let const_id1 =
                                    defs.register_definition(Expr::Constant(const1.clone()));
                                let address1 = diagram.add_node(Node::Constant(
                                    const1.clone(),
                                    Port(target_address, 1, const_id1),
                                ));
                                let const_id2 =
                                    defs.register_definition(Expr::Constant(const2.clone()));
                                let address2 = diagram.add_node(Node::Constant(
                                    const2.clone(),
                                    Port(target_address, 2, const_id2),
                                ));
                                ports = vec![
                                    Port(address0, port_index0, var.id),
                                    Port(address1, 0, const_id1),
                                    Port(address2, 0, const_id2),
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

    diagram
}

fn get_variable_ids_in_equality_nodes(diagram: &StringDiagram) -> HashSet<VarId> {
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
) -> HashMap<(Address, PortIndex), Variable> {
    let named_var_ids = get_variable_ids_in_equality_nodes(diagram);

    let mut port_vars: HashMap<(Address, PortIndex), Variable> = HashMap::new();
    let mut variable_id_counter = reg.next_id;
    while named_var_ids.contains(&variable_id_counter) {
        variable_id_counter += 1;
    }

    // Assign names to ports from equality nodes that already know who they are
    for (address, node) in &diagram.nodes {
        if let Node::Equality(vars, ports) = node {
            if let Some(var) = vars.first() {
                for (index, target_port) in ports.iter().enumerate() {
                    let current_port = (*address, index);
                    let target_port = (target_port.0, target_port.1);

                    port_vars.insert(current_port, var.clone());
                    port_vars.insert(target_port, var.clone());
                }
            } else if let Some(Port(target_addr, target_ptidx, target_id)) = ports.first() {
                let var = Variable {
                    name: None,
                    id: *target_id,
                };

                for (index, _target_port) in ports.iter().enumerate() {
                    let current_port = (*address, index);
                    let target_port = (*target_addr, *target_ptidx);

                    port_vars.insert(current_port, var.clone());
                    port_vars.insert(target_port, var.clone());
                }
            }
        }
    }

    // Assign variables to the remaining ports if they don't have one
    for (address, _) in &diagram.nodes {
        let ports = diagram.port_list(address);

        for (index, Port(target_addr, target_ptidx, target_id)) in ports.iter().enumerate() {
            let current_port = (*address, index);
            let target_port = (*target_addr, *target_ptidx);
            let variable = Variable {
                name: None,
                id: *target_id,
            };

            port_vars.insert(current_port, variable.clone());
            port_vars.insert(target_port, variable.clone());
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
                    let first_port = &(ports[0].0, ports[0].1);
                    let second_port = &(ports[1].0, ports[1].1);
                    let third_port = &(ports[2].0, ports[2].1);

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
                    let first_port = &(ports[0].0, ports[0].1);
                    let second_port = &(ports[1].0, ports[1].1);
                    let third_port = &(ports[2].0, ports[2].1);

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
                            v: Expr::Variable(port_vars[&(p1.0, p1.1)].clone()),
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
                                    v: Expr::Variable(port_vars[&(p2.0, p2.1)].clone()),
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
                            v: Expr::Variable(port_vars[&(p1.0, p1.1)].clone()),
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
                                    v: Expr::Variable(port_vars[&(p2.0, p2.1)].clone()),
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
                                    v: Expr::Variable(port_vars[&(p1.0, p1.1)].clone()),
                                    t: None,
                                }),
                                Box::new(TExpr {
                                    v: Expr::Variable(port_vars[&(p2.0, p2.1)].clone()),
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
                                    v: Expr::Variable(port_vars[&(p1.0, p1.1)].clone()),
                                    t: None,
                                }),
                                Box::new(TExpr {
                                    v: Expr::Variable(port_vars[&(p2.0, p2.1)].clone()),
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
                            v: Expr::Variable(port_vars[&(p.0, p.1)].clone()),
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
    // A = B, where A appears nowhere else   ==>   Unrestricted(B)
    RemoveUnaryEquality(Address),
    // The sum of nothing is 0
    // Contains addres of addition node
    RemoveUnaryAddition(Address),
    // The product of nothing is 1
    // Contains addres of multiplication node
    RemoveUnaryMultiplication(Address),
    // A = X + Y + Z   ==>   A = X + V & V = Y + Z
    // Stores the address of the add node, ports for the head and left argument of add node,
    // and all the ports connected to the args of the new node
    SplitAddition(Address, Port, Port, Vec<Port>),
    // A = X * Y * Z   ==>   A = X * V & V = Y * Z
    // Stores the address of the add node, ports for the head and left argument of mul node,
    // and all the ports connected to the args of the new node
    SplitMultiplication(Address, Port, Port, Vec<Port>),
    // A = B ^ n    =>   A = B * B * ... * B
    SplitExponentiation(Address),
    // Store address of equality node, index pointing to fusable/conserved node,
    FuseEquality(Address, PortIndex),
    // If we have
    // X = Y + Z + ... + W + ...
    // W = A + B + ...
    // We can fuse them into
    // X = Y + Z + ... + A + B + ...
    // Store address of addition node, index pointing to fusable/conserved node,
    FuseAddition(Address, PortIndex),
    // A = B, expressed through addtition, eliminated.
    // Constains address of Addition node and The two ports it connects to.
    RemoveBinaryAddition(Address, (Port, Port)),
    // If we have
    // X = Y * Z * ... * W * ...
    // W = A * B * ...
    // We can fuse them into
    // X = Y * Z * ... * A * B * ...
    // Store address of multiplication node, index pointing to fusable/conserved node,
    FuseMultiplication(Address, PortIndex),
    // A = B, expressed through multiplication, eliminated.
    // Constains address of Multiplication node and The two ports it connects to.
    RemoveBinaryMultiplication(Address, (Port, Port)),
    // n = m for constants n and m, either contradictory or trivial.
    // Stores the addresses of both constant nodes
    ConstantConstantRemoval(Address, Address),
    // A = B for unstestricted A and (unrestricted or constant) B.
    // Stores the addresses of both nodes
    UnrestrictedUnaryRemoval(Address, Address),
    AddConstantConstantHead(Address),
    AddConstantConstantTail(Address),
    // A = 0 + B   ==>   A = B
    // Constains address of ConstantAddition node and The two ports it connects to.
    AddConstantZero(Address, (Port, Port)),
    FuseAdditionByConstantHdTl(Address),
    FuseAdditionByConstantTlTl(Address),
    FuseAdditionByConstantHdHd(Address),
    MulConstantConstantHead(Address),
    MulConstantConstantTail(Address),
    // A = 0 * B   ==> A = 0 & unrestricted B
    // Stores the address of the muliplication, the two ports it connects to,
    // and the new address of the unrestricted node
    MulConstantZero(Address, (Port, Port), Address),
    // A = 1 * B   ==>   A = B
    // Constains address of ConstantMultiplication node and the two ports it connects to.
    MulConstantOne(Address, (Port, Port)),
    FuseMultiplicationByConstantHdTl(Address),
    FuseMultiplicationByConstantTlTl(Address),
    FuseMultiplicationByConstantHdHd(Address),
    ExpConstantConstantTail(Address),
    // A = B ^ 1   ==>   A = B
    // Constains address of ConstantExponentiation node and the two ports it connects to.
    ExpConstantOne(Address, (Port, Port)),
    FuseExponentiationByConstantHdTl(Address),
    EqualityUnrestricted(Address, PortIndex),
    // A = B, expressed through a redundant equality, eliminated.
    // Constains address of Equality node and The two ports it connects to.
    RemoveBinaryEquality(Address, (Port, Port)),
    // Turns X = A + (c + B) + D into X = c + X' & X' = A + B + D, for constant c
    // Stores address of addition node, port index of constant addition,
    // Old variable ID of the removed link,
    // And the constant value
    AdditionConstAddition(Address, PortIndex, VarId, BigInt),
    // Turns X = A * (c * B) * D into X = c * X' & X' = A * B * D, for constant c
    // Stores address of multiplication node, port index of constant multiplication,
    // Old variable ID of the removed link,
    // And the constant value
    MultiplicationConstMultiplication(Address, PortIndex, VarId, BigInt),
    // Turns X = A + B + c + D into X = c + X' & X' = A + B + D, for constant c
    // Stores address of addition node, port index of constant addition,
    // Old variable ID of the removed link,
    // And the constant value
    AdditionConst(Address, PortIndex, VarId, BigInt),
    // Turns X = A * B * c * D into X = c * X' & X' = A * B * D, for constant c
    // Stores address of addition node, port index of constant addition,
    // Old variable ID of the removed link,
    // And the constant value
    MultiplicationConst(Address, PortIndex, VarId, BigInt),
    // n = X & X = Y & X = Z & ...   ==>    n = Y & n = Z & ,,,
    // Stores the address of the equality node, the index that points to a constant
    // the address of that constant, a list of the outlying ports connected to the equality node
    // and the lowest address of the newly spawned constant nodes.
    // And the value stored in the constant
    EqualityConst(Address, PortIndex, Address, Vec<Port>, Address, BigInt),
    // An unrestricted value deletes ops under some conditions.
    // X = Y + Z, if Y is unrestricted, is equivalent to "there exists a Y s.t. X = Y + Z", which is trivially true.
    // The addition can then be deleted, leading to an unrestricted X and Z.
    // Also applies to multiplication, with some caviats.
    // This stores the address of the op node, the index connected to the unrestricted node
    // The remaining ports connected to theo operation
    // The address of the unrestricted node,
    // And the smallest address of the new unrestricted nodes
    AddMulUnrestricted(Address, PortIndex, Vec<Port>, Address, Address),
    DeleteConstOpUnrestricted(Address, PortIndex),
    // X = m * Y
    // Y = n + Z
    // becomes
    // X = (m * n) + Y
    // Y = m * Z
    // Stores original address of addition node, old outer var_id of addition node, and multiplicative constant
    SwapAddMulConstantsHdTl(Address, VarId, BigInt),
    // X = m * Y
    // Z = n + Y
    // becomes
    // X = (- m * n) + Y
    // Y = m * Z
    // Stores original address of addition node, old outer var_id of addition node, and multiplicative constant
    SwapAddMulConstantsTlTl(Address, VarId, BigInt),
    // n = V & V = X + Y   ==>    n = X + Y
    // Contains address of addition node and the port of the constant
    ReduceCVVAddition(Address, Port),
    // n = V & V = X * Y   ==>    n = X * Y
    // Contains address of addition node and the port of the constant
    ReduceCVVMultiplication(Address, Port),
}

pub type RewriteTrace = Vec<RewriteRule>;

fn split_node(diagram: &mut StringDiagram, defs: &mut DefinitionRegistry, address: Address) {
    // Extract information about the node at the given address
    let (first_two_ports, remaining_ports) =
        if let Some(Node::Addition(ports) | Node::Multiplication(ports)) =
            diagram.nodes.get(&address)
        {
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

    let new_id = defs.next_id;

    // Create a new node with the rightmost argument of the original
    let new_node_ports = vec![Port(address, 2, new_id)]
        .into_iter()
        .chain(remaining_ports.iter().cloned())
        .collect();
    let new_address = if let Some(Node::Addition(_)) = diagram.nodes.get_mut(&address) {
        diagram.add_node(Node::Addition(new_node_ports))
    } else if let Some(Node::Multiplication(_)) = diagram.nodes.get_mut(&address) {
        diagram.add_node(Node::Multiplication(new_node_ports))
    } else {
        panic!("Node magically changed type!")
    };

    // Replace the node in the diagram with the updated node
    if let Some(Node::Addition(ports) | Node::Multiplication(ports)) =
        diagram.nodes.get_mut(&address)
    {
        *ports = first_two_ports
            .into_iter()
            .chain(std::iter::once(Port(new_address, 0, new_id)))
            .collect();
    }

    // Update the port links for the remaining ports in the original node
    for (idx, port) in remaining_ports.iter().enumerate() {
        diagram.replace_port(&port.location(), &Port(new_address, idx + 1, port.2));
    }
}

// Completely decompose addition/multiplication node at address so it only has three arguments
fn decompose_node(diagram: &mut StringDiagram, defs: &mut DefinitionRegistry, address: Address) {
    while let Some(Node::Addition(ports)) = diagram.nodes.get(&address) {
        if ports.len() > 3 {
            let right_ports = ports[2..].to_vec();
            let rule = RewriteRule::SplitAddition(
                address,
                ports[0].clone(),
                ports[1].clone(),
                right_ports,
            );
            split_node(diagram, defs, address);
            calculate_defs(defs, rule);
        } else {
            break; // Decomposition is complete
        }
    }
}

// Split X = Y ^ n into X = Y * Y * ... * Y
// TODO: This is dumb. It needs to be split so that
//       X = Y ^ 2*n => Y1 = Y ^ n /\ X = Y1 * Y1, etc.
//       much smaller that way
fn split_exponentiation_node(
    diagram: &mut StringDiagram,
    defs: &mut DefinitionRegistry,
    address: Address,
) -> RewriteTrace {
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

    // Create a new multiplication node
    let mut multiplication_ports = Vec::new();
    multiplication_ports.push(p1); // Keep the first argument the same

    for i in 0..exp_value {
        let new_id = defs.next_id;
        // Point the equality ports to the multiplication ports
        equality_ports.push(Port(address, i + 1, new_id));

        // Point the multiplication ports to the equality ports
        multiplication_ports.push(Port(diagram.next_address, i + 1, new_id));
    }
    let equality_address = diagram.add_node(Node::Equality(vec![], equality_ports));

    // Replace the exponentiation node with the multiplication node
    diagram
        .nodes
        .insert(address, Node::Multiplication(multiplication_ports));

    // The second link of the original exponent node should now point to the first port of the new equality node.
    diagram.replace_port(&p2.location(), &Port(equality_address, 0, p2.2));

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
        diagram.replace_port(&port.location(), &Port(prime_node_address, index, port.2));
    }
}

fn spider_fusion(diagram: &mut StringDiagram, prime_node_address: Address, port_index: PortIndex) {
    // Extract information about the first node
    let (second_node_address, second_node_port_index, first_node_ports) = {
        if let Some(Node::Addition(ports) | Node::Multiplication(ports)) =
            diagram.nodes.get_mut(&prime_node_address)
        {
            if let Some(target_port) = ports.get(port_index) {
                (target_port.0, target_port.1, ports.clone())
            } else {
                return; // Invalid port index
            }
        } else {
            return; // Not a fusable node or doesn't exist
        }
    };

    // Extract information about the second node
    let second_node_ports = {
        if let Some(Node::Addition(ports) | Node::Multiplication(ports)) =
            diagram.nodes.get_mut(&second_node_address)
        {
            ports.clone()
        } else {
            return; // Not a fusable node or doesn't exist
        }
    };

    // Get the ports excluding the target port in the second fusable node
    let mut new_ports: Vec<Port> = second_node_ports
        .iter()
        .enumerate()
        .filter(|(i, _)| *i != second_node_port_index)
        .map(|(_, port)| port.clone())
        .collect();

    // Remove the port at port_index in the first fusable node
    new_ports.extend_from_slice(&first_node_ports[0..port_index]);
    new_ports.extend_from_slice(&first_node_ports[port_index + 1..]);

    // Update the first node in the diagram
    {
        if let Some(Node::Addition(ports) | Node::Multiplication(ports)) =
            diagram.nodes.get_mut(&prime_node_address)
        {
            *ports = new_ports.clone();
        }
    }

    // Remove the second fusable node from the diagram
    diagram.nodes.remove(&second_node_address);

    // Update the target ports in the diagram
    for (index, port) in new_ports.iter().enumerate() {
        diagram.replace_port(&port.location(), &Port(prime_node_address, index, port.2));
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
    diagram.replace_port(
        &first_port_target.location(),
        &Port(
            second_port_target.0,
            second_port_target.1,
            first_port_target.2,
        ),
    );
    diagram.replace_port(&second_port_target.location(), &first_port_target);
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
    diagram.replace_port(&second_port.location(), &Port(address, 0, second_port.2));

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
    diagram.replace_port(&first_port.location(), &Port(address, 0, first_port.2));

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
    diagram.replace_port(
        &second_node_second_port.location(),
        &Port(address, 1, second_node_second_port.2),
    );

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
    diagram.replace_port(
        &second_node_second_port.location(),
        &Port(address, 0, second_node_second_port.2),
    );

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
    diagram.replace_port(
        &second_node_first_port.location(),
        &Port(address, 1, second_node_first_port.2),
    );

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
    diagram.replace_port(&second_port.location(), &Port(address, 0, second_port.2));

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
    diagram.replace_port(&first_port.location(), &Port(address, 0, first_port.2));

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
        diagram.replace_port(&port2.location(), &Port(unrestricted_address, 0, port2.2));
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
    diagram.replace_port(
        &second_node_second_port.location(),
        &Port(address, 1, second_node_second_port.2),
    );

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
    diagram.replace_port(&second_port.location(), &Port(address, 0, second_port.2));

    // Update the target link of the second port to point to the first node
    diagram.replace_port(
        &second_node_second_port.location(),
        &Port(address, 1, second_node_second_port.2),
    );

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
    diagram.replace_port(
        &second_node_first_port.location(),
        &Port(address, 0, second_node_first_port.2),
    );

    // Update the target link of the second port to point to the first node
    diagram.replace_port(&first_port.location(), &Port(address, 1, first_port.2));

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
    diagram.replace_port(&first_port.location(), &Port(address, 0, first_port.2));

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
    diagram.replace_port(
        &second_node_second_port.location(),
        &Port(address, 1, second_node_second_port.2),
    );

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
        diagram.replace_port(&new_port.location(), &Port(address, index, new_port.2));
    }
}

// Turn X = A + B + ... + (c + D) + ... for constant c
// Into X = c + (A + B + ... + D + ...)
fn addition_const_addition(
    diagram: &mut StringDiagram,
    defs: &mut DefinitionRegistry,
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
    diagram.replace_port(
        &old_head_port.location(),
        &Port(target_address, 0, old_head_port.2),
    );

    let new_id = defs.next_id;

    // The head of the Addition node should now point to the tail of the AddConstant node.
    diagram.replace_port(&(address, 0), &Port(target_address, 1, new_id));

    // Make the necessary connection based on which port of the AddConstant node was connected to the Addition node.
    if target_port_index == 0 {
        diagram.replace_port(&(address, port_index), &second_port);
        diagram.replace_port(
            &second_port.location(),
            &Port(address, port_index, second_port.2),
        );
    } else {
        diagram.replace_port(&(address, port_index), &first_port);
        diagram.replace_port(
            &first_port.location(),
            &Port(address, port_index, first_port.2),
        );
    };

    // Insert the updated AddConstant node.
    diagram.nodes.insert(
        target_address,
        Node::AddConstant(new_value, old_head_port, Port(address, 0, new_id)),
    );
}

// Turn X = A * B * ... * (c * D) * ... for constant c
// Into X = c * (A * B * ... * D * ...)
fn multiplication_const_multiplication(
    diagram: &mut StringDiagram,
    defs: &mut DefinitionRegistry,
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
    diagram.replace_port(
        &old_head_port.location(),
        &Port(target_address, 0, old_head_port.2),
    );

    let new_id = defs.next_id;

    // The head of the Multiplication node should now point to the tail of the MultiplyConstant node.
    diagram.replace_port(&(address, 0), &Port(target_address, 1, new_id));

    // Make the necessary connection based on which port of the MultiplyConstant node was connected to the Multiplication node.
    if target_port_index == 0 {
        diagram.replace_port(&(address, port_index), &second_port);
        diagram.replace_port(
            &second_port.location(),
            &Port(address, port_index, second_port.2),
        );
    } else {
        diagram.replace_port(&(address, port_index), &first_port);
        diagram.replace_port(
            &first_port.location(),
            &Port(address, port_index, first_port.2),
        );
    };

    // Insert the updated MultiplyConstant node.
    diagram.nodes.insert(
        target_address,
        Node::MultiplyConstant(new_value, old_head_port, Port(address, 0, new_id)),
    );
}

// Turn X = A + B + ... + c + ... for constant c
// Into X = c + (A + B + ...)
fn addition_const(
    diagram: &mut StringDiagram,
    defs: &mut DefinitionRegistry,
    address: Address,
    port_index: PortIndex,
) {
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
    diagram.replace_port(
        &old_head_port.location(),
        &Port(target_address, 0, old_head_port.2),
    );

    let new_id = defs.next_id;

    // The head of the Addition node should now point to the tail of the AddConstant node.
    diagram.replace_port(&(address, 0), &Port(target_address, 1, new_id));

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
        diagram.replace_port(&new_port.location(), &Port(address, index, new_port.2));
    }

    // Insert the updated AddConstant node.
    diagram.nodes.insert(
        target_address,
        Node::AddConstant(value, old_head_port, Port(address, 0, new_id)),
    );
}

// Turn X = A * B * ... * c * ... for constant c
// Into X = c * (A * B * ...)
fn multiplication_const(
    diagram: &mut StringDiagram,
    defs: &mut DefinitionRegistry,
    address: Address,
    port_index: PortIndex,
) {
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
    diagram.replace_port(
        &old_head_port.location(),
        &Port(target_address, 0, old_head_port.2),
    );

    let new_id = defs.next_id;

    // The head of the Multiplication node should now point to the tail of the MultiplyConstant node.
    diagram.replace_port(&(address, 0), &Port(target_address, 1, new_id));

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
        diagram.replace_port(&new_port.location(), &Port(address, index, new_port.2));
    }

    // Insert the updated MultiplyConstant node.
    diagram.nodes.insert(
        target_address,
        Node::MultiplyConstant(value, old_head_port, Port(address, 0, new_id)),
    );
}

// If X = c and Y = X and A = X etc. then Y = c and A = c, etc
fn equality_const(diagram: &mut StringDiagram, address: Address, port_index: PortIndex) {
    // Extract the necessary information
    let (has_variables, target_port, const_value, cvar_id) =
        if let Some(Node::Equality(variables, ports)) = diagram.nodes.get(&address) {
            if let Some(target_port) = ports.get(port_index) {
                if let Some(Node::Constant(value, Port(_, _, cvar_id))) =
                    diagram.nodes.get(&target_port.0)
                {
                    (
                        !variables.is_empty(),
                        target_port.clone(),
                        value.clone(),
                        *cvar_id,
                    )
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
            diagram.replace_port(&target_port.location(), &Port(address, 0, target_port.2));
        }
    }

    // Update the gathered ports
    for port in ports_to_update {
        let new_const_node_addr = diagram.add_node(Node::Constant(
            const_value.clone(),
            Port(port.0, port.1, cvar_id),
        ));
        diagram.replace_port(&port.location(), &Port(new_const_node_addr, 0, cvar_id));
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
        diagram.replace_port(&port.location(), &Port(new_const_node_addr, 0, port.2));
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

    let (unrestricted_port, other_port) = if port_index == 0 {
        (port2.clone(), Port(port1.0, port1.1, port2.2))
    } else {
        (port1.clone(), Port(port2.0, port2.1, port1.2))
    };

    // At this point, one of the ports is connected to an Unrestricted node
    // Update the diagram by connecting the other port to where the Unrestricted port was pointing
    diagram.replace_port(&unrestricted_port.location(), &other_port);
    diagram.replace_port(&other_port.location(), &unrestricted_port);

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
    defs: &mut DefinitionRegistry,
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
    let new_id = defs.next_id;

    // Turn the AddConstant node into a MultiplyConstant node.
    diagram.nodes.insert(
        address,
        Node::MultiplyConstant(
            multiply_value,
            Port(add_head_port.0, add_head_port.1, new_id),
            add_tail_port,
        ),
    );

    // Turn the MultiplyConstant node into an AddConstant node.
    diagram.nodes.insert(
        multiply_address,
        Node::AddConstant(
            new_add_value,
            multiply_head_port,
            Port(multiply_tail_port.0, multiply_tail_port.1, new_id),
        ),
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
    defs: &mut DefinitionRegistry,
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
    let new_id = defs.next_id;

    // Turn the AddConstant node into a MultiplyConstant node with swapped ports.
    diagram.nodes.insert(
        address,
        Node::MultiplyConstant(
            multiply_value,
            Port(add_tail_port.0, add_tail_port.1, new_id),
            add_head_port.clone(),
        ),
    );

    // Update the linking ports to compensate for the swap.
    diagram.replace_port(
        &add_head_port.location(),
        &Port(address, 1, add_head_port.2),
    );

    // Turn the MultiplyConstant node into an AddConstant node.
    diagram.nodes.insert(
        multiply_address,
        Node::AddConstant(new_add_value, multiply_head_port, Port(address, 0, new_id)),
    );
}

// Try finding an appropriate rewrite for a given address
fn gen_string_diagram_step(
    diagram: &StringDiagram,
    _defs: &DefinitionRegistry,
    address: Address,
) -> Option<RewriteRule> {
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
                    if let Some(Node::Constant(val, _)) = diagram.nodes.get(&target_port.0) {
                        if ports.len() == 1 && !vars.is_empty() {
                            let outer_ports = ports
                                .iter()
                                .filter(|p| p != &target_port)
                                .cloned()
                                .collect();

                            return Some(RewriteRule::EqualityConst(
                                address,
                                port_index,
                                target_port.0,
                                outer_ports,
                                diagram.next_address,
                                val.clone(),
                            ));
                        }
                    }

                    if let Some(Node::Equality(_, _ports2)) = diagram.nodes.get(&target_port.0) {
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
                        let outer_ports = ports
                            .iter()
                            .filter(|p| p != &target_port)
                            .cloned()
                            .collect();
                        return Some(RewriteRule::AddMulUnrestricted(
                            address,
                            port_index,
                            outer_ports,
                            target_port.0,
                            diagram.next_address,
                        ));
                    }
                    if port_index > 0 {
                        if let Some(Node::Addition(_ports2)) = diagram.nodes.get(&target_port.0) {
                            if target_port.1 == 0 {
                                return Some(RewriteRule::FuseAddition(address, port_index));
                            }
                        }
                        if let Some(Node::AddConstant(const_val, _, _)) =
                            diagram.nodes.get(&target_port.0)
                        {
                            return Some(RewriteRule::AdditionConstAddition(
                                address,
                                port_index,
                                target_port.2,
                                const_val.clone(),
                            ));
                        }
                        if let Some(Node::Constant(const_val, _)) =
                            diagram.nodes.get(&target_port.0)
                        {
                            return Some(RewriteRule::AdditionConst(
                                address,
                                port_index,
                                target_port.2,
                                const_val.clone(),
                            ));
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
                            let outer_ports = ports
                                .iter()
                                .filter(|p| p != &target_port)
                                .cloned()
                                .collect();
                            return Some(RewriteRule::AddMulUnrestricted(
                                address,
                                port_index,
                                outer_ports,
                                target_port.0,
                                diagram.next_address,
                            ));
                        }
                    }
                    if port_index > 0 {
                        if let Some(Node::Multiplication(_ports2)) =
                            diagram.nodes.get(&target_port.0)
                        {
                            if target_port.1 == 0 {
                                return Some(RewriteRule::FuseMultiplication(address, port_index));
                            }
                        }

                        if let Some(Node::MultiplyConstant(val, _, _)) =
                            diagram.nodes.get(&target_port.0)
                        {
                            if val != &BigInt::from(0) {
                                return Some(RewriteRule::MultiplicationConstMultiplication(
                                    address,
                                    port_index,
                                    target_port.2,
                                    val.clone(),
                                ));
                            }
                        }
                        if let Some(Node::Constant(val, _)) = diagram.nodes.get(&target_port.0) {
                            return Some(RewriteRule::MultiplicationConst(
                                address,
                                port_index,
                                target_port.2,
                                val.clone(),
                            ));
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
                if let Some(Node::MultiplyConstant(value, _, _)) = diagram.nodes.get(&port2.0) {
                    if port2.1 == 1 {
                        return Some(RewriteRule::SwapAddMulConstantsTlTl(
                            address,
                            port1.2,
                            value.clone(),
                        ));
                    }
                }
                if let Some(Node::MultiplyConstant(value, _, _)) = diagram.nodes.get(&port1.0) {
                    if port1.1 == 1 {
                        return Some(RewriteRule::SwapAddMulConstantsHdTl(
                            address,
                            port2.2,
                            value.clone(),
                        ));
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
    defs: &mut DefinitionRegistry,
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
                diagram
                    .nodes
                    .insert(address, Node::Constant(BigInt::from(0), ports[0].clone()));
            }
            vec![]
        }
        RewriteRule::RemoveUnaryMultiplication(address) => {
            if let Some(Node::Multiplication(ports)) = diagram.nodes.get(&address).cloned() {
                diagram
                    .nodes
                    .insert(address, Node::Constant(BigInt::from(1), ports[0].clone()));
            }
            vec![]
        }
        RewriteRule::SplitAddition(address, _, _, _) => {
            split_node(diagram, defs, address);
            vec![address]
        }
        RewriteRule::SplitMultiplication(address, _, _, _) => {
            split_node(diagram, defs, address);
            vec![address]
        }
        RewriteRule::SplitExponentiation(address) => {
            split_exponentiation_node(diagram, defs, address);
            vec![address]
        }
        RewriteRule::FuseEquality(address, port_index) => {
            fuse_equality_nodes(diagram, address, port_index);

            vec![address]
        }
        RewriteRule::FuseAddition(address, port_index) => {
            spider_fusion(diagram, address, port_index);
            vec![address]
        }
        RewriteRule::RemoveBinaryAddition(address, (port1, port2)) => {
            remove_binary_node(diagram, address);
            vec![port1.0, port2.0]
        }
        RewriteRule::FuseMultiplication(address, port_index) => {
            spider_fusion(diagram, address, port_index);
            vec![address]
        }
        RewriteRule::RemoveBinaryMultiplication(address, (port1, port2)) => {
            remove_binary_node(diagram, address);
            vec![port1.0, port2.0]
        }
        RewriteRule::AddConstantZero(address, (port1, port2)) => {
            remove_binary_node(diagram, address);
            vec![port1.0, port2.0]
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
        RewriteRule::RemoveBinaryEquality(address, (port1, port2)) => {
            remove_binary_node(diagram, address);
            vec![port1.0, port2.0]
        }
        RewriteRule::AdditionConstAddition(address, port_index, _, _) => {
            if let Some(Node::Addition(ports)) = diagram.nodes.get(&address).cloned() {
                let target_port = &ports[port_index];
                addition_const_addition(diagram, defs, address, port_index, field_ops);
                vec![address, target_port.0]
            } else {
                vec![]
            }
        }
        RewriteRule::MultiplicationConstMultiplication(address, port_index, _, _) => {
            if let Some(Node::Multiplication(ports)) = diagram.nodes.get(&address).cloned() {
                let target_port = &ports[port_index];
                multiplication_const_multiplication(diagram, defs, address, port_index, field_ops);
                vec![address, target_port.0]
            } else {
                vec![]
            }
        }
        RewriteRule::AdditionConst(address, port_index, _, _) => {
            if let Some(Node::Addition(ports)) = diagram.nodes.get(&address).cloned() {
                let target_port = &ports[port_index];
                addition_const(diagram, defs, address, port_index);
                vec![address, target_port.0]
            } else {
                vec![]
            }
        }
        RewriteRule::MultiplicationConst(address, port_index, _, _) => {
            if let Some(Node::Multiplication(ports)) = diagram.nodes.get(&address).cloned() {
                let target_port = &ports[port_index];
                multiplication_const(diagram, defs, address, port_index);
                vec![address, target_port.0]
            } else {
                vec![]
            }
        }
        RewriteRule::EqualityConst(address, port_index, _, _, _, _) => {
            if let Some(Node::Equality(_, ports)) = diagram.nodes.get(&address).cloned() {
                equality_const(diagram, address, port_index);

                return ports.iter().map(|p| p.0).collect();
            } else {
                vec![]
            }
        }
        RewriteRule::AddMulUnrestricted(address, port_index, _, _, _) => {
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
        RewriteRule::SwapAddMulConstantsHdTl(address, _, _) => {
            if let Some(Node::AddConstant(_, port1, _)) = diagram.nodes.get(&address).cloned() {
                swap_add_and_multiply_constants_hd_tl(diagram, defs, address, field_ops);
                vec![address, port1.0]
            } else {
                vec![]
            }
        }
        RewriteRule::SwapAddMulConstantsTlTl(address, _, _) => {
            if let Some(Node::AddConstant(_, _, port2)) = diagram.nodes.get(&address).cloned() {
                swap_add_and_multiply_constants_tl_tl(diagram, defs, address, field_ops);
                vec![address, port2.0]
            } else {
                vec![]
            }
        }
        RewriteRule::MulConstantZero(address, (port1, port2), _) => {
            mul_constant_zero(diagram, address);
            vec![port1.0, port2.0]
        }
        RewriteRule::MulConstantOne(address, (port1, port2)) => {
            remove_binary_node(diagram, address);
            vec![port1.0, port2.0]
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
        RewriteRule::ExpConstantOne(address, (port1, port2)) => {
            remove_binary_node(diagram, address);
            vec![port1.0, port2.0]
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
    defs: &mut DefinitionRegistry,
    field_ops: &dyn FieldOps,
    address: Address,
) -> (Option<RewriteRule>, Vec<Address>) {
    let step = gen_string_diagram_step(diagram, defs, address);

    if let Some(rule) = &step {
        let res = apply_rewrite_step(diagram, defs, field_ops, rule.clone());
        (step, res)
    } else {
        (None, vec![])
    }
}

fn simplify_string_diagram(
    diagram: &mut StringDiagram,
    defs: &mut DefinitionRegistry,
    field_ops: &dyn FieldOps,
) {
    // Initialize the vector with all the addresses in the diagram.
    let mut addresses_to_process: Vec<Address> = diagram.nodes.keys().cloned().collect();

    while !addresses_to_process.is_empty() {
        let mut new_addresses = HashSet::new();

        for address in addresses_to_process {
            // Apply a single simplification step to the address.
            let (rule, modified_addresses) =
                simplify_string_diagram_step(diagram, defs, field_ops, address);
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
                calculate_defs(defs, rule)
            }
        }

        // Convert the HashSet to a Vec for the next iteration.
        addresses_to_process = new_addresses.into_iter().collect();
    }
}

// If we have c = v1 + v2 or c = v1 * v2, we can avoid making a new variable
// by absorbing the conatant into a bespoke node.
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
        diagram.replace_port(&tail1_port.location(), &Port(address, 0, tail1_port.2));
        diagram.replace_port(&tail2_port.location(), &Port(address, 1, tail2_port.2));

        // Now that we have updated the node and linking ports, we can safely remove the Constant node
        diagram.nodes.remove(&head_port.0);
    }
}

// Ensure there aren't any additions/multiplications with more than 2 inputs
// And also that there aren't any exponential nodes.
fn prep_for_3ac(diagram: &mut StringDiagram, defs: &mut DefinitionRegistry) {
    let mut changed = true;

    while changed {
        changed = false;

        // Collect the keys into a Vec
        let current_node_keys: Vec<_> = diagram.nodes.keys().cloned().collect();

        // Now iterate through the node keys
        for prime_node_address in current_node_keys {
            // Check if the node still exists, in case it was deleted
            if let Some(node) = diagram.nodes.get(&prime_node_address).cloned() {
                match node {
                    Node::Addition(ports) => {
                        // If there are more than three ports in the addition node, decompose it
                        if ports.len() > 3 {
                            decompose_node(diagram, defs, prime_node_address);
                            changed = true;
                        }
                        if ports.len() == 3 {
                            if let Some(Node::Constant(_, _)) = diagram.nodes.get(&ports[0].0) {
                                cvv_addmul(diagram, prime_node_address);
                                let rule = RewriteRule::ReduceCVVAddition(
                                    prime_node_address,
                                    ports[0].clone(),
                                );
                                calculate_defs(defs, rule);
                                changed = true;
                            }
                        }
                    }
                    Node::Multiplication(ports) => {
                        // If there are more than three ports in the multiplication node, decompose it
                        if ports.len() > 3 {
                            decompose_node(diagram, defs, prime_node_address);
                            changed = true;
                        }
                        if ports.len() == 3 {
                            if let Some(Node::Constant(_, _)) = diagram.nodes.get(&ports[0].0) {
                                cvv_addmul(diagram, prime_node_address);
                                let rule = RewriteRule::ReduceCVVMultiplication(
                                    prime_node_address,
                                    ports[0].clone(),
                                );
                                calculate_defs(defs, rule);
                                changed = true;
                            }
                        }
                    }
                    Node::ExponentiateConstant(_, _, _) => {
                        split_exponentiation_node(diagram, defs, prime_node_address);
                        changed = true;
                    }
                    _ => {}
                }
            }
        }
    }
}

fn split_operation_update(
    defs: &mut DefinitionRegistry,
    split_head_port: &Port,
    split_left_port: &Port,
    right_ports: &Vec<Port>,
    is_add: bool,
) {
    // Determine operation
    let op = if is_add {
        InfixOp::Add
    } else {
        InfixOp::Multiply
    };

    // If the operation being split is ternary, then the definition of the right side should be the sum of the right two values
    //    or, rather, the variables holding those values.
    // If the operation being split isn't ternary, then `right_two_address` will be `None`, and the right value is (temporarily) set to 0.
    let new_definition = match right_ports.len() {
        2 => {
            let var1 = right_ports[0].2;
            let var2 = right_ports[1].2;
            Expr::Infix(
                op,
                Box::new(TExpr {
                    v: Expr::Variable(Variable {
                        id: var1,
                        name: None,
                    }),
                    t: None,
                }),
                Box::new(TExpr {
                    v: Expr::Variable(Variable {
                        id: var2,
                        name: None,
                    }),
                    t: None,
                }),
            )
        }
        _ => Expr::Constant(0.into()), // default to constant 0
    };

    // Register this new definition
    let right_id = defs.register_definition(new_definition);

    // Update the definition of the id associated with the operation head.
    let head_id = split_head_port.2;
    let left_id = split_left_port.2;
    if let Some(expr) = defs.id_def_map.get_mut(&head_id) {
        *expr = Box::new(TExpr {
            v: Expr::Infix(
                op,
                Box::new(TExpr {
                    v: Expr::Variable(Variable {
                        id: left_id,
                        name: None,
                    }),
                    t: None,
                }),
                Box::new(TExpr {
                    v: Expr::Variable(Variable {
                        id: right_id,
                        name: None,
                    }),
                    t: None,
                }),
            ),
            t: None,
        });
    }
}

fn const_update(defs: &mut DefinitionRegistry, old_id: u32, const_val: BigInt, is_add: bool) {
    let op = if is_add {
        InfixOp::Subtract
    } else {
        InfixOp::Divide
    };

    // Create the new definition
    let definition = Expr::Infix(
        op,
        Box::new(TExpr {
            v: Expr::Variable(Variable {
                id: old_id,
                name: None,
            }),
            t: None,
        }),
        Box::new(TExpr {
            v: Expr::Constant(const_val),
            t: None,
        }),
    );

    defs.register_definition(definition);
}

fn swap_addmul_update(defs: &mut DefinitionRegistry, south_id: u32, const_val: BigInt) {
    // Create the new definition
    let definition = Expr::Infix(
        InfixOp::Multiply,
        Box::new(TExpr {
            v: Expr::Variable(Variable {
                id: south_id,
                name: None,
            }),
            t: None,
        }),
        Box::new(TExpr {
            v: Expr::Constant(const_val),
            t: None,
        }),
    );

    defs.register_definition(definition);
}

fn calculate_defs(defs: &mut DefinitionRegistry, rule: RewriteRule) {
    match rule {
        RewriteRule::SplitAddition(_address, split_head_port, split_left_port, right_ports) => {
            split_operation_update(defs, &split_head_port, &split_left_port, &right_ports, true)
        }
        RewriteRule::SplitMultiplication(
            _address,
            split_head_port,
            split_left_port,
            right_ports,
        ) => split_operation_update(
            defs,
            &split_head_port,
            &split_left_port,
            &right_ports,
            false,
        ),
        RewriteRule::SplitExponentiation(_address) => {
            // Not implemented!!!
        }
        RewriteRule::AdditionConstAddition(_address, _port_index, old_id, const_val) => {
            const_update(defs, old_id, const_val, true)
        }
        RewriteRule::MultiplicationConstMultiplication(
            _address,
            _port_index,
            old_id,
            const_val,
        ) => const_update(defs, old_id, const_val, false),
        RewriteRule::AdditionConst(_address, _port_index, old_id, const_val) => {
            const_update(defs, old_id, const_val, true)
        }
        RewriteRule::MultiplicationConst(_address, _port_index, old_id, const_val) => {
            const_update(defs, old_id, const_val, false)
        }
        RewriteRule::SwapAddMulConstantsHdTl(_address, south_id, mul_constant) => {
            swap_addmul_update(defs, south_id, mul_constant)
        }
        RewriteRule::SwapAddMulConstantsTlTl(_address, south_id, mul_constant) => {
            swap_addmul_update(defs, south_id, mul_constant)
        }
        _ => {
            // Nothing to do; no defs affected
        }
    }
}

pub fn simplify_3ac(
    equations: &mut Vec<TExpr>,
    input_ids: &HashSet<VarId>,
    defs: &mut Vec<Definition>,
    field_ops: &dyn FieldOps,
) {
    let mut reg: DefinitionRegistry = DefinitionRegistry::new(defs);
    let mut diag: StringDiagram = build_string_diagram(equations.to_vec(), input_ids, &mut reg);
    equations.clear();
    //println!("Performing simplifications");
    simplify_string_diagram(&mut diag, &mut reg, field_ops);
    println!("Converting back into 3AC...");
    prep_for_3ac(&mut diag, &mut reg);
    equations.extend(convert_to_3ac(&diag, &reg));
    defs.extend(reg.compile_definitions());
}
