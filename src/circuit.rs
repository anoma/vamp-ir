use std::{collections::HashMap, default};

use crate::ast::{Node, Vampir, Definition};

#[derive(Debug)]
pub struct Circuit {
    nodes: Vec<Box<Node>>,
    wires: Vec<Wire>,
    gates: Vec<Gate>,
}

#[derive(Debug)]
pub struct Gate {
    gate_type: String,
    inputs: Vec<usize>,
    outputs: Vec<usize>,
}
/*
#################################################
to do:
- gates should change from (gate_type: String, offset: usize) to (gate_type: String, offset: usize, length: usize)
    to accomodate gates with multiple outputs
- modify `flatten_node_tree` to accomodate gates with multiple outputs
- then alias invocations can be expanded
- alias invocations may need integer parameters (?) for instance x^n gate (or not...think about this)
- I think constants are not handled at all right now
    - allow constant wires
    - use BigInt for constants
- create data structure for wire list with an API
    - ideas for things it might have:
        - vector of wires
        - hashmap of wire indices (may add later if at all)
        - push() or allocate() method for creating a new wire with generated wire name if it doesn't exist already
        - extend() or concat() method for incorporating new wire lists from alias invocations
        - inputs() which fetches the *original input* wires the Prover supplies, including public inputs (i.e. not the interal wires named automatically)
        - public_inputs() which fetches just the public input wires
        - basic type information:
            wires can be `pub` or `const`
        - ???
- create data structure for gate list with an API
    - ideas things it might have:
        - vector of gates
        - push() method for adding a new gate
        - extend() or concat() method for incorporating new gate lists from alias invocations
        - checker that can see if an alias is invoked with the correct number of inputs
            - actually this might want to live as a method under Vampir struct
              so it can be checked without flattening
        - ???
- change Circuit struct to incorporate new data structures
    - it might have:
        - original node tree (ast)
        - wire list struct
        - gate list struct
        - list of alias definitions to reference
            - some backend helpers:
                - just because an alias definition is supplied doesn't mean it ought to be expanded.
                  a circuit writer ought to be able to supply a list of gates that should (or should not?) be expanded
        - methods for manipulating the circuit
            - optimizations on the tree (which need to carry over to the other data structures or be regenerated)
            - optimizations on the flattened data structures (which need to carry over to the tree to stay in sync)
            - regenerate tree from modified flattened data (?)
            - counting wires, gates, etc
- rewrite flattening functions to use new APIs
- simplification to the ast
    - for instance i have nodes and gates which are basically the same. consider whether a new gate structure is needed
        or if we should parse directly to gates. it is possible that a Gate struct ought to include extra information not
        captured in parsing. think about this
    - consider merging circuit.rs and ast.rs (?)
- rewrite display functions
- rewrite From functions (if needed after simplifying/combining modules)
- display polynomials in LaTeX
- error handling
    - handle parser errors
    - handle other errors
        - incorrect number of inputs in alias invocation

#################################################
*/

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Wire(pub String);

// impl From<&Node> for Gate {
//     fn from(node: &Node) -> Self {
//         match node {
//             Node::AliasInvocation(name, inputs) =>
//                 Self {
//                     name: name.into(),
//                     inputs: inputs.to_vec(),
//                 },
//             _ => unreachable!(),
//         }
//     }
// }

// impl From<Vec<Node>> for Circuit {
//     fn from(nodes: Vec<Node>) -> Self {
//         let inputs = construct_wire_map(&nodes);
//         let gates = construct_gate_list(&nodes, &inputs);
//         Self { inputs, gates }
//     }
// }

// impl From<Vampir> for Circuit {
//     fn from(vampir: Vampir) -> Self {
//         let inputs = construct_wire_map(&vampir.expressions);
//         let gates = construct_gate_list(&vampir.expressions, &inputs);
//         Self { inputs, gates }
//     }
// }


// rewrite these in a better way
fn flatten(nodes: Vec<Box<Node>>, definitions: &HashMap<String, Definition>) -> (Vec<Wire>, Vec<Gate>) {
    let mut wire_list = vec![Wire("zero".into())];
    let mut gate_list = vec![];
    nodes
        .iter()
        .map(|node| flatten_node(node, &mut wire_list, &mut gate_list, &definitions)).collect::<Vec<_>>();
    (wire_list, gate_list)
}

fn flatten_node(node: &Node, wire_list: &mut Vec<Wire>, gate_list: &mut Vec<Gate>, definitions: &HashMap<String, Definition>) -> Vec<usize> {
    match node {
        // if the wire exists already we return its index, otherwise we append a new one and return the new index
        Node::Wire(name) => {
            let position = wire_list.iter().position(|Wire(n)| name == n);
            let output = match position {
                Some(index) => index,
                None => {wire_list.push(Wire(name.into())); wire_list.len()-1}
            };
            vec![output]
        },
        Node::Node(gate_type, nodes) => {
            // the inputs of a gate is the flattened list of the outputs of the child nodes
            let inputs = nodes
                .iter()
                .flat_map(|node| flatten_node(node, wire_list, gate_list, definitions)).collect();

            // look up how many outputs a gate is supposed to have
            let num_outputs = definitions[gate_type].outputs.len();

            // if a gate has "no output" we use the zero wire as its output
            let outputs = match num_outputs {
                0 => vec![0],
                _ => (0..num_outputs).map(|i| {
                    wire_list.push(Wire(format!("w_{}", wire_list.len())));
                    wire_list.len()-1
                }).collect(),
            };

            gate_list.push(Gate {
                gate_type: gate_type.into(),
                inputs,
                outputs: outputs.clone(),
            });
            outputs
        }
        _ => unreachable!(),
    }
}

// looks up the supplied node from the definitions and returns a list of nodes that replace it
fn lookup_node(node: &Node, definitions: &HashMap<String, Definition>) -> Option<Definition> {
    match node {
        Node::Node(gate_type, inputs) => 
            match definitions.get(gate_type) {
                Some(definition) => Some(definition.clone()),
                None => None,
            }
        _ => None,
    }
}

// loops through a list of nodes, retrieves its definition nodes if it finds one, and flat_maps the results
fn expand_nodes(nodes: &Vec<Box<Node>>, definitions: &HashMap<String, Definition>) -> Vec<Box<Node>> {
    println!("expanding {:?}", nodes);
    nodes.into_iter().flat_map(|node| {
        match &**node {
            Node::Node(name, inputs) => match lookup_node(node, definitions) {
                Some(Definition{nodes,..}) => expand_nodes(&nodes, definitions),
                None => vec![Box::new(Node::Node(name.into(), expand_nodes(&inputs, definitions)))],
            },
            _ => vec![node.clone()]
        }
    }).collect()
}

impl Vampir {

    // replaces a leaf node of `gate_type: String` with the root node of an alias definition of that type if it finds one
    fn expand(&mut self) {
        self.expressions = expand_nodes(&self.expressions, &self.definitions)
    }
}




#[cfg(test)]
mod tests {
    use crate::{
        ast::Vampir,
        circuit::{Circuit, flatten},
        parser::{from_str, pairs},
    };

    #[test]
    pub(crate) fn test_circuit_construction() {
        let test_expressions = "
                def ec_check x y {
                    x^3 + 3 = y^2
                }
                //def add x y -> z {}
                //def mul x y -> z {}
                //def eq x y -> z {}
                //def exp x y -> z {}
                x * (ec_check y z) + (ec_check x y)
            ";
        let mut vampir = Vampir::from(test_expressions);
        println!("{:?}", vampir);
        //let (wires, gates) = flatten(vampir.expressions, &vampir.definitions);
        //println!("wires: {:?}\nexpressions: {:?}", wires, gates);
        vampir.expand();
        println!("{:?}", vampir);
        // let circuit = Circuit::from(vampir);
        // println!("{:?}", circuit);
    }
}
