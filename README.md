# Vampir Aliased Multivariate Polynomial Intermediate Representation

Proof system agnostic representation of circuits / constraint systems.

- [Design rationale](https://specs.anoma.net/master/architecture/language/j-group/vampir/ir_design.html) / [Spec](https://specs.anoma.net/master/architecture/language/j-group/vampir/ir_spec.html)
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
cd vamp-ir
cargo test
```

## Terminology

### Wires
A **Wire** is a leaf of an expression tree. A wire may have a name, but often carry no data in the circuit at all. In the latter case they are given an index only. 

### Nodes, Expressions, Constraints, Equations
An expression is a tree of **Nodes**, which can be binary operations, alias invocations, or wires. Expressions are implicitly constrained to equal zero unless otherwise specified. Equations are rewritten so the right-hand-side is 0. This means that *expressions*, *constraints*, and *equations* are represented the same way in Vamp-IR. The following are equivalent:
```
String Input        Tree Representation

"x*x - x"           Sub(Mul(Wire("x"), Wire("x")), Wire("x"))
"x*x = x"           Eq(Mul(Wire("x"), Wire("x")), Wire("x"))
"x*x - x = 0"       Eq(Sub(Mul(Wire("x"), Wire("x")), Wire("x")), Constant(0))
```
A simplification pass from the compiler would rewrite each representation in the same way: `Sub(Mul(Wire("x"), Wire("x")), Wire("x"))`

### Circuits
In Vamp-IR, a **Circuit** is a list of polynomial constraints which can optionally be augmented with a Signature, which allows it to be used as a function. The constraints in a circuit can take multiple forms, including an expression tree or flattened list of gates.

To aid in compilation, Circuits are augmented with extra information. Circuits have a list of wires so that wires may be referred to by index. There is also a list of **Equalities** which record when two wires are constrained to the same value (called Copy Constraints in the Plonk paper, for instance). Recording Equalities makes it easy to join Circuits---simply concatenate their internal data and append some new equalities.

### Signatures
An arithmetic circuit is not a "function" in the usual sense. However it is convenient to imagine arithmetic circuits as functions when writing programs. To get an arithmetic circuit to become a function we augment it with a **Signature**, which designates that certain wires are to be treated as "inputs" or "outputs". Circuits which have Signatures can be composed into new circuits (as long as the inputs and outputs match accordingly.)

### Definitions
A **Definition** is a Circuit that is given a name. Definitions are stored in a hash map whose keys are Strings and values are Circuits.

## Example

### Input

```javascript
// definitions

def bool x {
    x*x - x
}

def range_4 x {
    bool b0
    bool b1
    bool b2
    bool b3
    x = 8*b3 + 4*b2 + 2*b1 + b0
}

def pythagorean x y -> z {
    z^2 = x^2 + y^2
}

// circuit

range_4 a
range_4 b
range_4 (pythagorean a b)
```

### Representation in Vamp-IR

```java
Definitions {
    "bool": Circuit(
        signature: Signature(inputs: [Index(0)], outputs: []),
        wires: [Named("x"), Named("x"), Named("x")],
        nodes: [
            Sub(
                Mul(
                    Index(0),
                    Index(1),
                ),
            Index(2),
            ),
        ],
        equalities: [
            (Index(0), Index(0)),
            (Index(1), Index(0)),
            (Index(2), Index(0)),
        ],
    ),
    "range_4": Circuit(
        signature: Signature(inputs: [Index(0)], outputs: []),
        wires: [
            Named("b0"),
            Named("b1"),
            Named("b2"),
            Named("b3"),
            Named("x"),
            Named("b3"),
            Named("b2"),
            Named("b1"),
            Named("b0"),
        ],            
        nodes: [
            Invocation(name: "bool", inputs: [Index(0)]),
            Invocation(name: "bool", inputs: [Index(1)]),
            Invocation(name: "bool", inputs: [Index(2)]),
            Invocation(name: "bool", inputs: [Index(3)]),
            Eq(
                Index(4),
                Add(
                    Scale(Constant(8), Index(5)),
                    Add(
                        Scale(Constant(4), Index(6)),
                        Add(
                            Scale(Constant(2), Index(7)),
                            Index(8),
                        ),
                    ),
                ),
            ),
        ],
        equalities: [
            (Index(0), Index(0)),
            (Index(1), Index(1)),
            (Index(2), Index(2)),
            (Index(3), Index(3)),
            (Index(4), Index(4)),
            (Index(5), Index(3)),
            (Index(6), Index(2)),
            (Index(7), Index(1)),
            (Index(8), Index(0)),
        ],
    )
    "pythagorean": Circuit (
        signature: Signature(inputs: [Index(0), Index(1)], outputs: [Index(2)]),
        wires: [
            Named("z"),
            Named("z"),
            Named("x"),
            Named("x"),
            Named("y"),
            Named("y"),
        ],
        nodes: [
            Eq(
                Mul(
                    Index(0),
                    Index(1),
                ),
                Add(
                    Mul(
                        Index(2),
                        Index(3),
                    ),
                    Mul(
                        Index(4),
                        Index(5),
                    ),
                ),
            ),
        ],
        equalities: [
            (Index(0), Index(0)),
            (Index(1), Index(0)),
            (Index(2), Index(1)),
            (Index(3), Index(1)),
            (Index(4), Index(2)),
            (Index(5), Index(2)),
        ],
    )
}

Circuit(
    signature: Signature(inputs: [], outputs: []),
    wires: [Named("a"), Named("b"), Named("c")],
    nodes: [
        Invocation(name: "range_4", inputs: [Index(0)]),
        Invocation(name: "range_4", inputs: [Index(1)]),
        Eq(
            Index(2),
            Invocation(name: "pythagorean", inputs: [Index(0), Index(1)])
        ),
    ],
)
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
   expansion, expression evaluation, and algebraic manipulations)
2. Support alias expansion
3. Support expressions
4. CLI interfaces. (Use `clap`?)

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
   http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in the work by you, as defined in the Apache-2.0
license, shall be dual licensed as above, without any additional terms or
conditions.
