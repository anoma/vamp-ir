use crate::ast::{Circuit, Definitions, Invocation, Node, Wire, WireList, Signature};

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
    invocation: &Invocation,
    definitions: &'a Definitions,
) -> Option<&'a Circuit> {
    match definitions.get(&invocation.name) {
        Some(circuit) => Some(circuit),
        None => None,
    }
}

// lookup an index in the current list of wires and replace the index with the wire
fn rename_node(node: Node, wires: WireList) -> Node {
    match node {
        Node::Wire(Wire::Index(i)) => Node::Wire(wires[i].clone()),
        Node::Op(op) => Node::Op(
            op.same(
                op.inputs()
                    .iter()
                    .map(|node| rename_node(node.clone(), wires.clone()))
                    .collect(),
            ),
        ),
        _ => node,
    }
}

fn reindex_wire(wire: &Wire, offset: usize) -> Wire {
    match wire {
        Wire::Index(num) => Wire::Index(num+offset),
        _ => wire.clone(),
    }
}

fn reindex_node(node: &Node, offset: usize) -> Node {
    match node {
        Node::Op(op) => Node::Op(op.same(op.inputs().iter().map(|node| reindex_node(node, offset)).collect())),
        Node::Wire(wire) => Node::Wire(reindex_wire(&wire, offset)),
        Node::Invocation(Invocation{name, inputs}) => Node::Invocation(Invocation { name: name.to_string(), inputs: inputs.into_iter().map(|node| reindex_node(node, offset)).collect()}),
    }
}

fn reindex_signature(sig: &Signature, offset: usize) -> Signature {
    Signature{
        inputs: sig.inputs.iter().map(|wire| reindex_wire(wire, offset)).collect(),
        outputs: sig.outputs.iter().map(|wire| reindex_wire(wire, offset)).collect()
    }
}

fn reindex_circuit(circuit: &Circuit, offset: usize) -> Circuit {
    let signature = reindex_signature(&circuit.signature, offset);
    let nodes = circuit.nodes.iter().map(|node| reindex_node(node, offset)).collect();
    let equalities = circuit.equalities.iter().map(|(left, right)| (reindex_wire(left, offset), reindex_wire(right, offset))).collect();
    Circuit { signature, wires: circuit.wires.clone(), nodes, equalities }
}

#[cfg(test)]
mod tests {
    use crate::ast::Vampir;

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
    }
}
