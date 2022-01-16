# IR Design
## Generality
A circuit IR ought to capture the intention of the circuit only. It should be agnostic to the proving system features and also be agnostic to the specific constraint system it targets (so circuits written in the IR can compile to R1CS, vanilla Plonk, Plonk with lookups, Plonk with custom gates, or any other constraint system). This way a circuit can be written canonically in the IR and different backends with different features can interpret and execute the IR as they wish.

## Overview
An IR that can express every arithmetic circuit *and* indicate custom gates or lookup gates *and* be interpretable in R1CS or Plonkish constraints only needs two basic objects: 

- multivariate polynomial constraints
- aliases for sets of multivariate polynomial constraints (subcircuits)

Additional auxiliary objects will need to be provided alongside (but separate to) the circuit:
- input maps to assist in witness/public input generation

## Arithmetic Gates Are Unbounded Multivariate Polynomial Constraints
An arithmetic circuit is just a representation of a system of multivariable polynomial constraints. This system must be broken down into smaller low-degree constraints that "fit" into the constraint format of the target proving system.

If the IR is meant to capture a circuit only, agnostic of the target constraint system, then the IR should not be limited by R1CS or Plonkish formats. The IR should not force the circuit programmer to make arbitrary choices in how to break down the circuit to "fit" the circuit into the IR, either. It would be best to let the proving system backend choose how to break down the circuit in a way that is optimal for it's specific features.

For this reason, *unbounded* multivariable constraints should be used to represent all arithmetic gates.

### Optimizing Arithmetic Circuits
It is possible that if a polynomial constraint system is expressed appropriately then known algorithms for manipulating polynomials like Gröbner basis techniques *may* be able to do some optimization of arithmetic circuits automatically, but the degree of optimization is unknown.

There is also [CirC](https://eprint.iacr.org/2020/1586.pdf), an infrastructure for building compilers for *existentially quantified circuits* which include arithmetic circuits. CirC is designed to be a shared infrastructure that assists in the creation of front-end compilers by leveraging existing research on constraint languages. The CirC system produces its own IR which provides some optimization for arithmetic circuits and can assist in bug-finding.

### How Aztec Does Polynomial Constraints
Aztec has a DSL Noir, which compiles to their IR called ACIR, which was an inspiration for this IR design. ACIR uses *bounded* multivariable constraints which are limited to degree 2. This allows them to target R1CS and vanilla Plonkish constraints easily, but forces the circuit programmer to make choices which may not be optimal in order to fit larger degree constraints into the IR. 

Limiting the constraints to degree 2 obfuscates subcircuits that are degree 3 and up by forcing them to be split into pieces. This can make it more difficult for the backend proving system to recognize patterns in the circuit which could be used to optimize proving, especially if the subcircuits are broken down inconsistently.

Furthermore, it is quite feasible to add custom gates in Plonk which are higher than degree 2, but ACIR's degree limit makes this more difficult for the backend to do.

Despite this flaw, ACIR is a pretty good IR that could serve our purposes. Unfortunately there isn't a detailed spec for ACIR that I've been able to find. There is a [Noir book](https://noir-lang.github.io/book/index.html) which has a nice [diagram for compilation to ACIR](https://noir-lang.github.io/book/c_overview.html) but details of the exact format ACIR takes is not specified.

## Aliased Subcircuits
It is convenient to have aliases for subcircuits that are used frequently. Aliases for common subcircuits (like `range`, `pedersen`, `xor`, or `sha3`) can be included in a standard library. The IR should also allow the circuit writer to define their own subcircuits and give them aliases. Then a DSL can shift the majority (or entirety?) of the low-level circuit programming to the IR for a clean separation of concerns.

Aliased subcircuits are likely to be used repeatedly. Therefore they also serve as a signal to the proving system backend that an optimization ought to be applied to that subcircuit if it is possible.

## Aliases Can Be Custom Gates
A compiler for a particular backend should be able to decide to replace gates or sets of gates in a circuit with a custom gate if it has one available to add into its proving system. 

Custom gates in Plonk are single gates that *replace* a subcircuit. The polynomial constraints defining the subcircuit are moved out of the circuit and become integrated into the proving system.

The polynomial constraints that *check* the custom gate in the proving system are precisely the same polynomials that make up the subcircuit the custom gate replaces! This allows *aliases* to double as indicators for custom gates the backend may decide to incorporate.

For example, suppose while writing my circuit I notice that there are many repeated subcircuits with the same form, like this constraint that checks if `a,b,c` is a Pythagorean triple:
```
poly a^2 + b^2 - c^2
```
I can alias this gate for convenience *and* to indicate that it should become a custom gate if possible in the backend proving system.
```
// alias
def pyth a b c {a^2 + b^2 - c^2}
```

If the proving system lacks custom gate support then it will interpret the alias as just an alias and simply replace the aliased gates with the subcircuits they name as usual. If it has a modular proving system with custom gate support it will incorporate a polynomial `a(X)^2 + b(X)^2 - c(X)^2` into its proving system instead, making `pyth` a true custom gate defined in the IR circuit itself.

## Standard Lookup Gates and Tables
Similarly to custom gates, if a user specifies particular gates which are available as lookups, a backend compiler may decide to replace those gates with lookup gates and construct a lookup table automatically. Since most lookup use cases are for common operations like `xor` or `mod` the user won't have to define a lookup table in the circuit or use a `lookup` gate directly. This is in-line with the idea that the IR should express a circuit, not demand how it is to be executed. For this reason, lookup gates do not need to be expressed in the IR. 

To use standard lookups, the circuit programmer writes their circuit in the IR using a standard library of aliases for common subcircuits like `xor` and `mod`. If lookups are enabled in the proof system then tables for `xor` and `mod` will be created automatically (or retrieved from a library of precomputed tables) and the indicated aliased subcircuits are replaced with the correct lookup gates.

## Input Maps
Arithmetic circuits have "input wires" which accept the "initial inputs" to circuit. However, many intermediate wires need to be computed from these initial inputs during the proving process. This is because the ciruit must be broken down into smaller constraints to fit into the proving system, and the process of breaking circuits apart introduces new variables whose values need to be computed.

When the backend compiles the IR into a circuit, it needs to know all initial inputs to the circuit. The "intermediate" or "internal" values can be computed by the backend. For instance, in the Pythagorean example above, the Prover would need to supply `a,b,c` and the backend can compute the intermediate values `a^2,b^2,c^2` from these if it needs to. Since the circuit language of the IR does not include values at all, we need another way to get the values of `a,b,c` to the backend. 

We can use an "input map" for this. This is a simple auxilary file that lists the input variables and their values. A JSON dictionary object or similar key-value pair system would work well:
```
{
    "a": 5,
    "b": 12,
    "c": 13,
}
```

Input maps need to be provided for the initial input values of the Prover's witnesses as well as the public input values. The backend compiler will take the initial input values and the intermediate values it computes and assemble them into vectors of field elements required by the proving and verification algorithms.