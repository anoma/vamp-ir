use crate::ast::{
    Circuit, Constant, Definitions, Gate, GateList, Invocation, Priv, Pub, Signature, Wire,
};

/*
#################################################
to do:
- gates should change from (gate_type: String, offset: usize) to (gate_type: String, offset: usize, length: usize)
    to accomodate gates with multiple outputs
- modify `flatten_gate_tree` to accomodate gates with multiple outputs
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
        - original gate tree (ast)
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
    - for instance i have gates and gates which are basically the same. consider whether a new gate structure is needed
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

// // lookup an index in the current list of wires and replace the index with the wire
// fn rename_gate(gate: Gate, wires: WireList) -> Gate {
//     match gate {
//         Gate::Input(Wire::Index(i)) => Gate::Input(wires[i].clone()),
//         Gate::Op(op) => Gate::Op(
//             op.same(
//                 op.inputs()
//                     .iter()
//                     .map(|gate| rename_gate(gate.clone(), wires.clone()))
//                     .collect(),
//             ),
//         ),
//         _ => gate,
//     }
// }

impl Circuit {
    pub fn reindex(&self, offset: usize) -> Circuit {
        let signature = self.signature.reindex(offset);
        let gates = self.gates.iter().map(|gate| gate.reindex(offset)).collect();
        let equalities = self
            .equalities
            .iter()
            .map(|(left, right)| (left.reindex(offset), right.reindex(offset)))
            .collect();
        Circuit {
            signature,
            gates,
            equalities,
        }
    }

    pub fn expand(&self, definitions: &Definitions) -> Circuit {
        let signature = self.signature.clone();
        let mut equalities = self.equalities.clone();

        let gates = self.gates.clone()
            .into_iter()
            .map(|gate| gate.expand(definitions))
            .fold(GateList::new(), |state, gate_list| {
                GateList::concat(state, gate_list)
            });

        Circuit {
            signature,
            gates,
            equalities,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Vampir;
    use crate::ast::{Gate, GateList};

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
            a*(b+c*a)
            (range_2 (volume a (div_mod b c)))
        ";
        let mut vampir = Vampir::from(test_expressions);
        println!("{:?}\n", vampir);
        vampir
            .circuit
            .clone()
            .gates
            .into_iter()
            .flat_map(|gate| GateList::from(gate))
            .collect::<GateList>()
            //.remove_names()
            .pprint();
        println!("\nexpanded:");
        vampir.circuit.expand(&vampir.definitions).gates.pprint();
    }
}
