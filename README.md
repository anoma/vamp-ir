# IR module

- [Spec](https://org.heliax.dev/products/plonkup/ir_spec.html)
- Provides interfaces for `compile`, `prove`, and `verify` using IR.
- Calls interfaces of `plonk-core`.

## Interfaces

- Compile: takes input an IR file, outputs compiled prover key, verifier data, as
  well as necessary metadata mapping IR wires to positions.
- Prove: takes input the output of compile, as well as a table `PI` mapping
  public input wires to values and a table `W` mapping witness wires to values,
  outputs a proof.
- Verify: takes input the output of compile, as well as a table `PI` (same as
  in prove), and a proof; outputs a boolean value.

## TODO

1. Implement internal AST and code converting output of PEST parser to internal AST.
2. First implementation of AST Walk that generate circuit using `poly_gate` and `arithmetic_gate`.
3. Basic AST optimization: useless variable elimination, ...?
4. CLI interfaces. (Use `clap`?)
