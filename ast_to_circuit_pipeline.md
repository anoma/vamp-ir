# High-level sketch of AST to Plonk circuit

## Global variables to keep track

Backend should keep two `HashMap`s, one for wires and one for aliases, mapping
their symbols to metadata and other relevant information.

- Wires: Public / Private, Output / non-output, Value. If a wire is marked as
  an output wire, the backend also stores information on how it can be derived
  from the values of other wires. Example: `x = y + z` marks `x` an output wire
  which can be computed given values of `y` and `z`.
- Alias: Input wire count, output wire count, body (vector of statements).
  Should be initialized with the set of built-in gates.

## "Compilation" pipeline

1. Algebraic simplification
  - Canonicalize polynomials: remove duplicate terms, collapse constants (e.g. `2^4 -> 16`)
2. Alias expansion
  - Recursively expand all aliases
  - Naming of local wires: for example, a local wire `b` in `some_gate` alias
    during the 2nd expansion can be named `some_gate#local#2#b`.
2. Unnamed wire instantiation
  - Allocate all unnamed wires from gate expressions in the `HashMap` for wires.
  - Example naming: `some_gate#output#1#2` for the 2nd output of the 1st invocation of the gate `some_gate`.
3. **(Not necessary for first iteration)** algebraic optimizations, wire eliminations, etc.
4. Algebraic constraint translation
  - Translate algebraic constraints to PLONK `arithmetic_gate`
  - `arithmetic_gate` only supports `z = a * x + b * y + c * x * y`, where z is
    either `pub` or not and `a, b, c` are possibly zero.
  - For first iteration: do something naive here, allocate an `arithmetic_gate`
    for each multiplication and addition is fine.
5. Final AST walks to circuit
  - At this point the AST contains only gate constraint statements on
    explicitly named wires.
  - First, we need to obtain the metadata for the circuit from the AST
    - Names of PI wires
    - Names of output wires (wires that are output of custom gates)
    - Names of non-output witness wires
    - At this point we can instantiate Plonk circuit struct in rust, specifying
      all PI wires and non-output witness wires.
  - Second, to synth the circuit, we simply walk the AST and call the corresponding Plonk functions.
