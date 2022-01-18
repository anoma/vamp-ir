# Example: TestCircuit in IR

We describe how the full workflow working with the test circuit given in
[circuit.rs:465](../plonk-core/src/circuit.rs#L465).

First, the IR file is as follows.
```
// circuit.pir
pub c d f
range 6 a
range 5 b
c = a + b
d = a * b
f = fixed_base_scalar_mul e
```

Note that the base for the gate `fixed_base_scalar_mul` is fixed to be
`generator` by the backend, where
```rust
let (x, y) = P::AFFINE_GENERATOR_COEFFS;
let generator = GroupAffine::new(x, y);
```

## Compile

To compile, we can run
```
pir compile circuit.pir -op circuit.prover.bin -ov circuit.verifier.bin
```
The two binary files stores the necessary data and metadata for the prover and
the verifier.

### Output wires

During compilation, the backend should realize that wires `c`, `d`, `f` are
**output** wires, meaning they are derivable given values of `a`, `b`, `e`.

## Proof generation

Since the only non-output wires are `a`, `b`, `e`, one should be able to
generate a proof by specifying the following two JSON files.

The public input table `PI.json` is actually empty, since all public wires are
output wires and can be computed by the backend during prover runtime.

```
// PI.json
{}
```

The witness input table `W.json` contain values for `a`, `b`, and `e`.

```
// W.json
{
  "a": 20,
  "b": 5,
  "e": 2
}
```

To generate a proof, we run

```
pir proof circuit.prover.bin PI W -o proof.bin
```

The above command should (1) populate the table `PI.json` and (2) generate a
binary file containing the Plonk proof.

Table `PI.json` should now contain the computed values of `c`, `d`, `f`. (For
general circuits, both PI and W both could contain more values than they
started with after proof generation).

```
// PI.json, after proof generation
{
  "c": 25,
  "d": 100,
  "f": 2
}
```

## Proof verification

To verify a proof, we can run the following command to determine the validity of
a proof.

```
pir verify circuit.verifier.bin PI proof.bin
```
