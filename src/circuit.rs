use std::collections::HashMap;

use crate::ast::{Node, Vampir};

#[derive(Debug)]
pub struct Circuit {
    inputs: HashMap<Wire, usize>,
    gates: Vec<Gate>,
}

#[derive(Debug)]
pub struct Gate {
    name: String,
    inputs: Vec<Wire>,
    // TODO maybe include outputs
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

// (String, usize) is how I am referring to a "gate" right now. This will be changed
// consider incorporating these into `From` functions
fn flatten(node_forest: Vec<Box<Node>>) -> (Vec<Wire>, Vec<(String, usize)>) {
    let mut wires = vec![];
    let mut nodes = vec![];
    node_forest.iter().for_each(|node_tree| flatten_node_tree(node_tree, &mut wires, &mut nodes));

    (wires, nodes)
}

fn flatten_node_tree(node_tree: &Node, wires: &mut Vec<Wire>, nodes: &mut Vec<(String, usize)>) {
    match node_tree {
        Node::Wire(string) => if !wires.contains(&Wire(string.into())) {wires.push(Wire(string.into()))},
        Node::Node(node_type, node_forest) => {
            node_forest.iter().for_each(|node_tree| flatten_node_tree(node_tree, wires, nodes));
            nodes.push((node_type.into(), wires.len()));
            // following line should actually be a for_each on the outputs of this node type
            // to accodmodate gates with more than one output
            wires.push(Wire(format!("w_{}", wires.len())));
        }
        _ => unreachable!(),
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
                x * (ec_check y z) + (ec_check x y)
            ";
        let vampir = Vampir::from(test_expressions);
        println!("{:?}", vampir);
        let (wires, nodes) = flatten(vampir.expressions);
        println!("wires: {:?}\nexpressions: {:?}", wires, nodes);
        // let circuit = Circuit::from(vampir);
        // println!("{:?}", circuit);
    }
}
