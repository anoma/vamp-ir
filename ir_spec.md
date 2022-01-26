# IR Spec Working Draft

There are two possible approaches to naming: (1) constraint systems,
constraints, and variables, or (2) circuits, gates, and wires. Since `plonk` is
a proof system for zero-knowledge *circuits*, we stick with circuit-based
terminology.

## Wires

- A wire is a fundamental, atomic, immutable representation of data.
- Name: names of wires are strings that start with a letter, and can include underscores and digit characters.
- They are allocated at compile time as they appear.
- All wires are assigned values in $F_p$ (either at compile-time or at prover / verifier run-time), where $p$ is a globally fixed (usually ~256-bit) prime. TODO: should we add `#pragma` declarations for specifying $p$?

### Public wires
Public wires are public inputs into the circuit that are not known at compile-time. To allocate the variable `contract_public_key` as public we simply say:
```
pub contract_public_key
```
The variable `contract_public_key` can then be used in a polynomial constraint like any other variable.

### Private wires
Private wires are the Prover's secret witnesses. Any variable not explicitly made public is a private wire.

### Named vs. unnamed wires
Some wires are implicitly created and can be unnamed from the perspective of the IR.

## Constants (a.k.a scalars)
Fixed constants are strings of digits that are interpreted as decimal integers. They can be preceded by a "-" to indicate that they are negative.

## Expressions

An expression is any code block that returns one or more values encoded in unnamed wires.

### Polynomial Expressions

Polynomial expressions are algebraic expressions involving of wires and
constants ("multi-variable" polynomial where wires are "variables"). Below are
two polynomial expressions.
```
x^2 - 1
x^2 + y^2 - z^2
```
The polynomial ought to be expressed in a standard format that can easily be plugged into polynomial solvers and other tools.

### Gate expressions

Some gates provide output wires. It forms an expression when they are called without output wires in the arguments. For example, `add x 1`, `mul x y`.

## Constraint statements

A constraint statement constrains a set of wires. Named and unnamed internal
wires can be created as a consequence of a constraint statement. There are two
types of constraint statements, equality statements and gate statements.

### Equality statement

An equality statement constrains two expressions (left- and right-hand side) to be equal to each other.

Here is an equality statement for checking if three inputs form a Pythagorean triple:
```
0 = x^2 + y^2 - z^2
```
This gate represents a constraint. That is, this gate constrains the polynomial indicated to equal zero. The variables used in the polynomial that have not already been used earlier will be allocated when the polynomial is processed.

It is convenient sometimes to think of a polynomial expression as an "assignment". We can achieve this by allowing polynomial constraints to be written as an equation with a left and right side. These two polynomials are equivalent:

```
x_1^3 - 5x_2^2 - x_3
x_3 = x_1^3 - 5x_2^2
```

Since this is not *true* assignment, but merely a constraint, it is also valid to "assign" to scalars. These are equivalent:
```
0 = x_1 * x_2 - 42
42 = x_1 * x_2
```

### Gate statement

A gate constraint is formed when a gate is called with all input and output wires.

```
bool x
bool x * y
```

## Alias statement
An alias can be given to sets of constraint statements that are used often. Curly braces are used to indicate a set of constraints.
```
// definition of my_alias
def my_alias input_wires... -> output_wires... {
	constraint_statement_1
	constraint_statement_2

	.
	.
}

// calling my_alias can be done like this:
my_alias input_1 input_2 ... output_1 output_2 ...

// or like this:
output_1 output_2 ... = my_alias input_1 input_2 ...
// this is technically an equality statement where the rhs is a gate expression
```
In arithmetic constraints there really is no distinction between inputs and outputs. We make the distinction here to tell the backend compiler how to interpret a constraint written in "assignment" mode and how to compose constraints.

Since aliased subcircuits are likely to be used more than once, a backend compiler may decide to replace an aliased subcircuit with a lookup gate or custom gate if it has those features available.

### Examples
#### No-output Gate
A range gate is a commonly used gate that does not have a canonical output. A 2-bit range gate would be defined and called like this:

```
// definition
def range_2 x {
	b_0 = b_0^2
	b_1 = b_1^2
	x = 2b_1 + b_0
}

// call
range_2 x
```
The variable `x` is now constrained to be 0, 1, 2, or 3.

Because the gate is defined with no outputs, trying to call it like this should produce an error:
```
// invalid
y = range_2 x

// also invalid
x = range_2
```

#### Single-output Gate
```
// definition
add x y -> z = { poly x + y - z}

// call
add x y z

// or
z = add x y
```

#### Multiple Outputs
Aliased gates can give multiple outputs. Suppose we want to do integer division between two variables and want both the quotient and remainder as outputs (like `divmod` in Python).

```
def divmod x y -> q r { .... }

// then divmod can be called like this
divmod x y q r

// or this
q r = divmod x y
```
#### Composing Gates
Gates can be composed using assignment mode or prefix mode.

```
// definition
def curve_add x1 y1 x2 y2 -> x3 y3 { ... }

// assignment mode definition
def curve_add_four x1 y1 x2 y2 x3 y3 x4 y4 -> x5 y5 {
	mx1 my1 = curve_add x1 y1 x2 y2
	mx2 my2 = curve_add x3 y3 x4 y4
	x5 y5 = curve_add mx1 my1 mx2 my2
}

// prefix mode definition
def curve_add_four x1 y1 x2 y2 x3 y3 x4 y4 -> x5 y5 {
	curve_add (curve_add x1 y1 x2 y2) (curve_add x3 y3 x4 y4)
}
```

## A Partial List of Built-in Gates and (some of) their Definitions in Terms of Polynomial Gates
```
// x + y = z
def add x y -> z { poly x + y - z }

// x - y = z
def sub x y -> z { poly x - y - z }

// x*y = z
def mul x y -> z { poly x * y - z }

// inv x = x_inv
def inv x -> x_inv
// if x is 0, then x_inv is set to 0

// x < y
def less x y { ... }

// x = r mod q
def mod x q -> r { ... }

// bit decomp of x
def bits x -> b_0 ... b_k { ... }

// bit pack of b_0 ... b_k
def bitpack b_0 ... b_k -> x { bits x b_0 ... b_k }

// k = bit count of x
def bitcount x -> k { ... }

// x a bool
def bool x {range_1 x}

// x < 2^k 		k is an usize
def bit_range[k] x { ... }

// conditional / controlled selects
// out = opt_bit
def cselect bit opt_0 opt_1 -> out
// bit = 1 => out = val / bit = 0 => out = 1
def cselect_1 bit val -> out
// bit = 1 => out = 1 / bit = 0 => out = val
def cselect_0 bit val -> out

// k-bit xor and
def xor[k] x y -> z { ... }
def and[k] x y -> z { ... }

// Twister Edwards curve addition
def variable_base_add x1 y1 x2 y3 -> x3 y3
def fixed_base_add ...

// scalar mul potentially defined as internal alias using point addition and
bit decomposition
def variable_base_mul scalar x1 y1 -> x2 y2
def fixed_base_mul scalar -> x2 y2

// Blake2s(x) = y
def blake2s x .. -> y { ... }

// Blake2b(x) = y
def blake2b x .. -> y { ... }

// SHA-256(x) = y
def sha256 x .. -> y { ... }

etc.
```

### Built-in gates and output wires

For built-in gates, we insist the backend to know how output wires are computed
given all the input wires. As a result, when generating a proof, output wires
of built-in gates need not be specified.

## Prover and verifier run-time input specification

To generate a proof, we need to provide a file specifying a circuit (or compiled
output from it), as well as (1) a table `PI` mapping all public wires
to their values (in $F_p$) and (2) a table `W` mapping all non-output wires to
their values (again in $F_p$).

To verify a proof, we need to provide, besides the proof, a file specifying a
circuit (or compiled output from it), as well as a table `PI` (same as in proof
generation).

Tables `PI` and `W` are given as a JSON formatted table.
