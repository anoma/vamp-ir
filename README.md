# Vampir Aliased Multivariate Polynomial Intermediate Representation

Proof system agnostic representation of circuits / constraint systems.

- [Design rationale](./ir_design.md) / [Spec](./ir_spec.md)
- Provides interfaces for `compile`, `prove`, and `verify` using IR.
- Calls interfaces of `plonk-core`.

## Interfaces

- Compile: takes input an IR file, outputs compiled prover key, verifier data, as
  well as necessary metadata mapping IR wires to positions.
- Prove: takes input the output of compile, as well as a table `PI` mapping
  public input wires to values and a table `W` mapping non-ouput witness wires
  to values, outputs a proof.
- Verify: takes input the output of compile, as well as a table `PI` (same as
  in prove), and a proof; outputs a boolean value.

## Testing

```=sh
cd plonk-ir
cargo test
```

## How it (should) work

### Global variables to keep track

Synthesizer use `HashMap`s to keep track of private wires, public wires, and
gates / aliases.

- Wires: Public / Private, Output / non-output, Value. If a wire is marked as
  an output wire, the backend also stores information on how it can be derived
  from the values of other wires. Example: `x = y + z` marks `x` an output wire
  which can be computed given values of `y` and `z`.
- Alias: Input wire count, output wire count, body (vector of statements).
  Should be initialized with the set of built-in gates.

### Compilation pipeline

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
  - Translate algebraic constraints to PLONK polynomial gates
  - Supported polynomial gates
    ```
    poly_gate[m l r o c] xl xr -> xo {
      o * xo = -m * (xl * xr) - l * xl - r * xr - c
    }
    ```
    ```
    pubout_poly_gate[m l r o c] xl xr xo -> xp {
      pub xp = - m * (xl * xr) - l * xl - r * xr - o * xo - c
    }
    ```
  - For first iteration: do something naive here, allocate a `poly_gate`
    for each internal multiplication and addition.
5. Circuit synthesis (proof-of-concept done in `synth.rs`)
  - At this point the AST contains only gate constraint statements on
    explicitly named wires.
  - First, we need to obtain the metadata for the circuit from the AST
    - Names of PI wires
    - Names of output wires (wires that are output of custom gates)
    - Names of non-output witness wires (and how they are derived from other
      wires in terms of arithmetic expressions and built-in gates)
  - Second, to synth the circuit, we simply walk the AST and call the corresponding Plonk functions.
  - Finally, metadata and output of `plonk-core` (prover key and verifier data) is returned as output.

## Ordered TODO List

1. Implement internal AST structs that can be easily manipulated (for alias
   expansion / algebraic manipulations)
2. Support alias expansion
3. Support marking of output wires at compile time and derivation of their values at prover runtime
4. CLI interfaces. (Use `clap`?)
5. Support general expressions
