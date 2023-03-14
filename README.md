# Vamp-IR

Vamp-IR is a language for specifying arithmetic circuits. The Vamp-IR compiler can transform an arithmetic circuit into a form that is compatible with any proving system backend.

- [Design rationale](https://specs.anoma.net/master/architecture/language/j-group/vampir/ir_design.html) / [Spec](https://specs.anoma.net/master/architecture/language/j-group/vampir/ir_spec.html)

VAMP-IR stands for:

**V**amp-IR
**A**liased
**M**ultivariate
**P**olynomial
**I**ntermediate
**R**epresentation

The Vamp-IR language consists of *alias definitions* and *expressions* defining an arithmetic circuit.

## Trying Vamp-IR

### Installation
```
git clone git@github.com:anoma/vamp-ir
cd vamp-ir
cargo build
```

### Setup
Creates public parameters for proving and verifying and saves to `params.pp`:
```
./target/debug/vamp-ir setup params.pp
```

### Compile
Compiles `./tests/alu.pir` to `alu.plonk` using the setup:
```
./target/debug/vamp-ir compile ./tests/alu.pir params.pp alu.plonk
```

### Prove
Create proof from `alu.plonk` and save to `proof.plonk`:
```
 ./target/debug/vamp-ir prove alu.plonk params.pp proof.plonk
```

### Verify
Verify `proof.plonk`:
```
./target/debug/vamp-ir verify alu.plonk params.pp proof.plonk
```

## Interface
The Vamp-IR compiler takes as input:
- A Vamp-IR document consisting of alias definitions and a circuit written with those aliases. (The alias definitions may come from Vamp-IR's standard library inwhich they don't need to included in the document itself, or they be written as custom aliases and included in the Vamp-IR document.)
- Target backend parameters, which may include a specification of the constraint system (R1CS, 3-Plonk, 4-Plonk, etc.), the order of the scalar field, elliptic curve parameters, and so on. These parameters help the Vamp-IR compiler transform the given circuit into an optimized form targetting the backend proving system and give important information that is needed to compute the values during prover runtime (for instance, field division requries knowledge of the order of the scalar field).
- (optionally) A precomputed prover input map which allows the Vamp-IR compiler to skip the prover runtime step.

The Vamp-IR compiler outputs:
- A portable, canonical form of the circuit as a `.vampir` document

Additionally, if connected to a backend proving system implementation that has a Vamp-IR -> Backend compiler and parameters for the backend, Vamp-IR can invoke the backend proving system and output:
- A proof in the backend proving system
- A verification of a proof provided in the backend proving system

## Components
Vamp-IR consists of a parser, compiler, and a prover runtime. 
- The parser parses the text input into an abstract structure tree. 
- The compiler transforms the data in the tree in various ways, eventually arriving at a form that is compatible with some backend proving system implementation, according to *target backend parameters* which are supplied alongside the text input.
- The prover runtime takes the circuit IR and the prover's private *initial inputs* and executes the circuit steps to produce the intermediate values of the computation. These are organized and sent to the backend prover in the *prover input map*.

## Vamp-IR language

Vamp-IR is a language for specifying arithmetic circuits. Circuits in Vamp-IR are written as a set of multivariable polynomial expressions. Vamp-IR allows a circuit writer to define their own custom, reusable circuits called *aliases*. Vamp-IR will also ship with a standard library of pre-defined aliases which are commonly needed. 

A Vamp-IR document consists of a set of *aliases* and a *circuit*.

## Circuit
A Vamp-IR *circuit* is a set of *expressions*.

### Expressions
An *expression*  in Vamp-IR is a multivariable polynomial constraint written with variables, constants, and the symbols `+`, `-`, `*`, `^`, `(`, `)`, and `=`. 

This constraint (which checks that $(x,y)$ is a point on a certain elliptic curve) is a Vamp-IR expression:

`y^2 = x^3 + 3`

Every expression is implicitly an equation in Vamp-IR. An expression written with no `=` is considered to be constrained to be equal to 0. Therefore, the equation above could be rewritten without the `=` as:

`y^2 - (x^3 + 3)` 

These two forms of expressions are equivalent in Vamp-IR. 

### An example circuit

Here is a small circuit written in Vamp-IR. This circuit checks that two twisted Edwards elliptic curve points $(x_1, y_1)$ and $(x_2, y_2)$ add to a third point $(x_3, y_3)$ on the curve. (This curve uses twisted Edwards parameters $A=3$ and $D=2$.)

```
(1 + 2*x1*x2*y1*y2)*x3 = x1*y2 + y1*x2
(1 - 2*x1*x2*y1*y2)*y3 = y1*y2 - 3*x1*x2
```

## Aliases
Since this circuit is likely to be used often, we can define an *alias* for it so it can be reused. An alias can include a *signature* which allows it to be used as a function by designating which internal wires can be considered inputs and outputs. The compiler then knows how to compose circuits together.

```
def alias_name[param1, param2, ...] input1 input2 ... -> output1 output2 ... {
  expression1
  expression2
  ...
}
```

Here is the circuit from earlier written as an alias:
```
def twisted_edwards_add[A, D] x1 y1 x2 y2 -> x3 y3 {
  (1 + D*x1*x2*y1*y2)*x3 = x1*y2 + y1*x2
  (1 - D*x1*x2*y1*y2)*y3 = y1*y2 - A*x1*x2
}
```

Then this alias can be called like a function inside a circuit:
```
twisted_edwards_add[3, 2] p1 q1 p2 q2
```

### Example with expansion
Here is a collection of aliases constraining $x$ to be a 1, 2, 4, or 8 bit integer. Then a circuit checks that a triple $(a,b,c)$ satisfies the Pythagorean theorem and that both $a$ and $b$ are bytes.

```
// aliases

def range[1] x {
  x*x - x
}

def range[2] x {
  range[1] hi
  range[1] lo
  2*hi + lo - x
}

def range[4] x {
  range[2] hi
  range[2] lo
  4*hi + lo - x
}

def range[8] x {
  range[4] hi
  range[4] lo
  16*hi + lo - x
}


// circuit

a^2 + b^2 = c^2
range[8] a
range[8] b
```


### Expanded form
The circuit above is expanded by the compiler into a set of polynomial constraints, using $w_k$ to stand in for auxiliary variables required by the invoked aliases.
```
a*a + b*b - c*c
w0*w0 - w0
w1*w1 - w1
2*w1 + w0 - w2
w3*w3 - w3
w4*w4 - w4
2*w4 + w3 - w5
4*w5 + w2 - w6
w7*w7 - w7
w8*w8 - w8
2*w8 + w7 - w9
w10*w10 - w10
w11*w11 - w11
2*w11 + w10 - w12
4*w12 + w9 - w13
8*w13 + w6 - a
w14*w14 - w14
w15*w15 - w15
2*w15 + w14 - w16
w17*w17 - w17
w18*w18 - w18
2*w18 + w17 - w19
4*w19 + w16 - w20
w21*w21 - w21
w22*w22 - w22
2*w22 + w21 - w23
w24*w24 - w24
w25*w25 - w25
2*w25 + w24 - w26
4*w26 + w23 - w27
8*w27 + w20 - b
```

### Prover Input Map
The compiler can use the expanded list of constraints to compute the values of all variables including the auxiliary variables, from the initial inputs $(a, b, c)$ during the *prover runtime* process.

```
  a: 3    // b00000011
  b: 4    // b00000100
  c: 5    // b00000101
 w0: 1
 w1: 1
 w2: 3
 w3: 0
 w4: 0
 w5: 0
 w6: 3
 w7: 0
 w8: 0
 w9: 0
w10: 0
w11: 0
w12: 0
w13: 0
w14: 1
w15: 0
w16: 1
w17: 1
w18: 0
w19: 1
w20: 5
w21: 0
w22: 0
w23: 0
w24: 0
w25: 0
w26: 0
w27: 0
```
## Benchmarks
These benchmarks are performed on a Lenovo ThinkPad X1 Carbon Gen 9 with 8.0 GiB RAM and 
an 11th Gen Intel® Core™ i5-1135G7 @ 2.40GHz × 8 unless stated otherwise
### Halo2 backend

#### SHA256 1 block message

|         | `Compile`  | `Prove`    | `Verify` |
|:--------|:-----------|:-----------|:---------|
| Vamp-IR | `172.05 s` | `26.72 s`  | `0.61 s` |
| Halo2   | //         | `161.05 s` | `1.06 s` |

#### SHA256 2 block message

|         | `Compile`  | `Prove`    | `Verify` |
|:--------|:-----------|:-----------|:---------|
| Vamp-IR | `353.76 s` | `46.91 s`  | `1.09 s` |
| Halo2   | //         | `160.03 s` | `1.05 s` |

#### SHA256 4 block message

|         | `Compile`  | `Prove`         | `Verify` |
|:--------|:-----------|:----------------|:---------|
| Vamp-IR | `729.47 s` | Memory Failiure | X        |
| Halo2   | //         | `160.36 s`      | `1.03 s` |

We re-run the with a device that has 128GB of RAM and these are the results:

|         | `Compile` | `Prove`    | `Verify` |
|:--------|:----------|:-----------|:---------|
| Vamp-IR | `60 s`    | `81.983 s` | `0.6 s ` |

### ZK-garage plonk backend

#### Blake2s

|           | `Compile` | `Prove`   | `Verify` |
|:----------|:----------|:----------|:---------|
| Vamp-IR   | `76.30 s` | `57.59 s` | `0.22 s` |
| ZK-Garage | //        | `32.48 s` | `0.10 s` |

#### Blake2s using only fan-in 2 gates
To have a more fair comparison between Vamp-IR and ZK-Garage, we can use only fan-in 2 gates in the Blake2s circuit.
This is because the current version of Vamp-IR does not uses fan-in 3 gates, as the ZK-Garage backend does, which in a 
speed-up.

|           | `Compile` | `Prove`    | `Verify` |
|:----------|:----------|:-----------|:---------|
| Vamp-IR   | `76.30 s` | `57.59 s`  | `0.22 s` |
| ZK-Garage | //        | `360.48 s` | `0.81 s` |

The version of Blake2 used for the latter benchmark can be found here:
https://github.com/heliaxdev/ark-plonk/blob/blake2s/examples/blake2s_circuit_fain2.rs


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
