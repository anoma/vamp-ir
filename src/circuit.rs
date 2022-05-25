use std::collections::HashMap;

use crate::ast::{Definition, Node, Vampir, Wire, Gate};

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

// looks up the supplied invocation from the definitions and returns a list of nodes that replace it
fn lookup_invocation<'a>(
    node: &Node,
    definitions: &'a HashMap<String, Definition>,
) -> Option<&'a Definition> {
    match node {
        Node::Gate(name, _, _) => match definitions.get(name) {
            Some(definition) => Some(definition),
            None => None,
        },
        _ => None,
    }
}

// this is an initial flattening step that creates new wires for the outputs of each invocation
// and separates the each invocation into its own expression
// for instance:
//      Invocation(range_2 inputs: [Invocation(volume, inputs:[a, Invocation(div_mod, inputs: [b, c])])])
// becomes
//      Invocation(range_2 inputs: [Invocation(volume, inputs:[a, w_2, w_3])])
//      Expression(div_mod, inputs: [b, c], outputs: [w_2, w_3])
// becomes
//      Expression(range_2 inputs: [w_5], outputs: [])
//      Expression(volume, inputs:[a, w_2, w_3], outputs: [w_5])
//      Expression(div_mod, inputs: [b, c], outputs: [w_2, w_3])

fn wire_to_node(wire: Wire) -> Node {
    match wire {
        Wire(string) => Node::Wire(string),
    }
}

// lookup an index in the current list of wires and replace the index with the wire
fn rename_node(node: Node, wires: &[Node]) -> Node {
    match node {
        Node::Index(i) => wires[i].clone(),
        Node::Gate(name, inputs, outputs) => Node::Gate(
            name,
            inputs
                .iter()
                .map(|node| rename_node(node.clone(), wires))
                .collect(),
            outputs,
        ),
        _ => node.clone(),
    }
}

// flatten_node and flatten_nodes are a recursive pair of functionstakes a parent node that may have other nodes as children, creates the correct number of output wires for each
// child, and flat_maps the outputs of the children together to give the inputs of the parent.
// parent(child1(..), child2(..), ...)
// [flatten_node(child1), flatten_node(child2), ..., parent(child1(..), child2(..))]
fn flatten_node(node: &Node) -> Vec<Node> {
    match node {
        Node::Gate(_, inputs, _) => {
            let children = flatten_nodes(inputs.to_vec());
            [children, vec![node.clone()]].concat()
        }
        _ => vec![node.clone()],
    }
}

fn flatten_nodes(nodes: Vec<Node>) -> Vec<Node> {
    nodes
        .into_iter()
        .flat_map(|node| match node {
            Node::Gate(_, _, _) => flatten_node(&node),
            _ => vec![],
        })
        .collect()
}

// traverses the nodes and applies the supplied function
// to each node
fn traverse(nodes: Vec<Node>, f: fn(Node) -> Node) -> Vec<Node> {
    nodes.into_iter().map(|node| {
        match node {
            Node::Gate(name, inputs, outputs) => f(Node::Gate(name, traverse(inputs, f), outputs)),
            _ => f(node),
        }
    }).collect()
}

impl Vampir {
    // mutates the nodes in a vampir circuit to be partially flattened. each alias
    // invocation becomes the root of its own tree
    fn flatten(&mut self) {
        self.nodes = self.nodes.clone()
            .into_iter()
            .flat_map(|node| flatten_node(&node))
            .collect();        
    }

    // replaces input nodes of parent with output wires of child
    fn replace_inputs(mut self) {
        self.nodes = self.nodes.into_iter().map(|node| 
            match node {
                Node::Gate(name, inputs, outputs) => 
                    Node::Gate(
                        name.to_string(), 
                        inputs
                        .into_iter()
                        .flat_map(|node| {
                            node.outputs()
                        }).collect(),
                        outputs),
                _ => unreachable!(),
            }).collect();
    }



    // // queries the definition of an alias and creates the right number of wires to serve as its outputs, and returns them
    // fn set_outputs(
    //     mut self,
    //     node: Node,
    // ) -> Node {
    //     match node {
    //         Node::Gate(name, inputs, outputs) => match lookup_invocation(&node, &self.definitions) {
    //             Some(def) => {
    //                 let new_outputs = (0..def.outputs.len())
    //                     .map(|_| {
    //                         let new_wire = Wire(format!("w_{}", &self.wires.len()));
    //                         self.wires.push(new_wire.clone());
    //                         new_wire
    //                     })
    //                     .collect();
    //                 Node::Gate(name.to_string(), inputs.clone(), new_outputs)
    //             }
    //             None => node.clone(),
    //         },
    //         _ => node.clone(),
    //     }
    // }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Node, Vampir},
        parser::{from_str, pairs},
    };

    #[test]
    pub(crate) fn test_circuit_construction() {
        let test_expressions = "
            def volume x y z -> v {
                x*y*z-v
            }
            def range_2 x {
                b0 * b0 - b0
                b1 * b1 - b1
                2*b1 + b0 - x
            }
            def div_mod x y -> q r {
                q * y + r - x
            }
            (range_2 (volume a (div_mod b c)))
        ";
        let mut vampir = Vampir::from(test_expressions);
        println!("{:?}", vampir);
        vampir.flatten();
        println!("{:?}", vampir);
    }
}
