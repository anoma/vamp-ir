use std::collections::HashMap;

use crate::ast::{Definition, Gate, Node, Vampir, Wire};

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
fn lookup_nodes<'a>(
    node: &Node,
    definitions: &'a HashMap<String, Definition>,
) -> Option<&'a Definition> {
    match node {
        Node::Gate(gate) => match definitions.get(&gate.name) {
            Some(definition) => Some(definition),
            None => None,
        },
        _ => None,
    }
}

fn wire_to_node(wire: Wire) -> Node {
    match wire {
        Wire(string) => Node::Wire(string),
    }
}

impl Vampir {
    // creates a new Vampir struct after applying the transformation `f` to its nodes
    fn transform(self, f: impl Fn(&Node) -> Node) -> Self {
        Vampir {
            definitions: self.definitions,
            inputs: self.inputs,
            nodes: self.nodes.iter().map(|n| n.traverse(&f)).collect(),
        }
    }

    // creates a new Vampir struct after applying the transformation `f` to its nodes and flattening the results
    fn flat_transform(self, f: impl Fn(&Node) -> Vec<Node>) -> Self {
        Vampir {
            definitions: self.definitions,
            inputs: self.inputs,
            nodes: self
                .nodes
                .iter()
                .flat_map(|n| n.flat_traverse(&f))
                .collect(),
        }
    }

    // creates a partially flattened representation of vampir where each alias
    // invocation becomes the root of its own tree
    fn flatten_to_invocations(self) -> Self {
        self.flat_transform(|n| n.into_iter().collect::<Vec<Node>>())
    }

    fn expand(&self, name: String) -> Self {
        let with_outputs = self.clone().assign_outputs();
        with_outputs.flat_transform(|n| match n {
            Node::Gate(gate) => {
                if gate.name == name {
                    let all_wires = [&gate.inputs[..], &gate.wires[..]].concat();
                    println!("all wires {:?}", all_wires);
                    self.definitions[&gate.name]
                        .nodes
                        .iter()
                        .map(|node| {
                            node.traverse(&|node| match node {
                                Node::Index(i) => all_wires[*i].clone(),
                                _ => node.clone(),
                            })
                        })
                        .collect()
                } else {
                    vec![n.clone()]
                }
            }
            _ => vec![n.clone()],
        })
    }

    // traverses the tree and populates gate.wires with the correct number of wires from the definition
    // counts allocated wires as it goes, giving output wires an index
    // QUESTION: can this be reworked to use `traverse` or `transform` ?
    fn assign_outputs(self) -> Self {
        let mut n = 0usize;

        Vampir {
            definitions: self.definitions.clone(),
            inputs: self.inputs.clone(),
            nodes: self
                .nodes
                .iter()
                .map(|node| self.fill_node_wires(node, &mut n))
                .collect(),
        }
    }

    fn fill_node_wires(&self, node: &Node, n: &mut usize) -> Node {
        match node {
            Node::Gate(Gate {
                ref name,
                ref inputs,
                ..
            }) => {
                let new_inputs = inputs
                    .iter()
                    .map(|node| self.fill_node_wires(node, n))
                    .collect();
                let wires: Vec<Node> = (0..self.definitions[name].clone().wires.len())
                    .map(|i| Node::Wire(format!("w_{}", i + *n)))
                    .collect();
                *n += wires.len();
                Node::Gate(Gate {
                    name: name.clone(),
                    inputs: new_inputs,
                    wires,
                })
            }
            _ => node.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Node, Vampir};

    #[test]
    pub(crate) fn test_circuit_construction() {
        let test_expressions = "
            def volume x y z -> v {
                x*y*z = v
            }
            def range_2 x {
                b0 * b0 = b0
                b1 * b1 = b1
                2*b1 + b0 = x
            }
            def div_mod x y -> q r {
                q * y + r = x
            }
            (range_2 (volume a (div_mod b c)))
        ";
        let vampir = Vampir::from(test_expressions);
    }

    #[test]
    pub(crate) fn test_power_expansion() {
        let with_powers = "
            y^2 = x^3 + 3
        ";
        let with_mul = "
            y*y = x*x*x + 3
        ";
        assert_eq!(
            Vampir::from(with_mul).nodes,
            Vampir::from(with_powers)
                .transform(Node::expand_powers)
                .nodes
        );
    }

    #[test]
    pub(crate) fn test_eq_to_sub() {
        let with_eq = "
            y^2 = x^3 + 3
        ";
        let with_sub = "
            y^2 - (x^3 + 3)
        ";
        assert_eq!(
            Vampir::from(with_sub).nodes,
            Vampir::from(with_eq).transform(Node::eq_to_sub).nodes
        );
    }
    #[test]
    pub(crate) fn test_expansion() {
        let test_expressions = "
            def volume x y z -> v {
                x*y*z = v
            }
            def range_2 x {
                b0 * b0 = b0
                b1 * b1 = b1
                2*b1 + b0 = x
            }
            def div_mod x y -> q r {
                q * y + r = x
            }
            (range_2 (volume a (div_mod b c)))
            (volume a b c)
        ";
        let vampir = Vampir::from(test_expressions);
        let node = &vampir.nodes[0];
        let expanded = node.expand(&vampir.definitions);
    }
}
