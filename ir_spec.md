# An Initial Spec

## Polynomial Constraints
A polynomial constraint is just a multivariable polynomial expression. The polynomial ought to be expressed in a standard format that can easily be plugged into polynomial solvers and other tools.

## Variables
Variables are strings that start with a letter, and can include underscores and digit characters. They are allocated when they appear in a polynomial constraint.

## Fixed scalars
Fixed scalars are strings of digits that are interpreted as decimal integers. They can be preceeded by a "-" to indicate that they are negative.

## Private variables
Private variables are the Prover's witnesses. Any variable not explicity made public is a private variable.

## Public variables
Public variables are public inputs into the circuit that are not known at compile-time. To allocate the variable `contract_public_key` as public we simply say:
```
pub contract_public_key
```
The variable `contract_public_key` can then be used in a polynomial constraint like any other variable.

## Polynomial Gates
Here is a polynomial gate for checking if three inputs form a Pythagorean triple:
```
x^2 + y^2 - z^2
```
This gate represents a constraint. That is, this gate constrains the polynomial indicated to equal zero. The variables used in the polynomial that have not already been used earlier will be allocated when the polynomial is processed. 

It is convenient sometimes to think of an polynomial expression as an "assignment". We can achieve this by allowing polynomial constraints to be written as an equation with a left and right side. These two polynomials are equivalent:

```
x_1^3 - 5x_2^2 - x_3
x_3 = x_1^3 - 5x_2^2
```

Since this is not *true* assignment, but merely a constraint, it is also valid to "assign" to scalars. These are equivalent:
```
x_1 * x_2 - 42
42 = x_1 * x_2
```
## Aliases
An alias can be given to sets of constraints that are used often. Curly braces are used to indicate a set of constraints.
```
// definition of my_alias
def my_alias inputs... -> outputs... {
	poly_exp_1
	poly_exp_2
	
	.
	.
}

// calling my_alias can be done like this:
my_alias input_1 input_2 ... output_1 output_2 ...

// or like this:
output_1 output_2 ... = my_alias input_1 input_2 ...
```
In arithmetic constraints there really is no distinction between inputs and outputs. We make the distinction here to tell the backend compiler how to interpret a constraint written in "assignment" mode and how to compose constraints.

Since aliased subcircuits are likely to be used more than once, a backend compiler may decide to replace an aliased subcircuit with a lookup gate or custom gate if it has those features available.

## Examples
#### No-output Gate
A range gate is a commonly used gate that does not have a canonical output. A 2-bit range gate would be defined and called like this:

```
// definition
def range_2 x {
	poly b_0^2 - b_0
	poly b_1^2 - b_1
	poly 2b_1 + b_0 - x
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
	curve_add curve_add x1 y1 x2 y2 curve_add x3 y3 x4 y4
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

// x/y = z
def div x y -> z { mul y z x }

// bit decomp of x
def bits x -> b_0 ... b_k { ... }

// bit pack of b_0 ... b_k
def bitpack b_0 ... b_k -> x { bits x b_0 ... b_k }

// x < 2^k 		k an int 
def range_k x { ... }

// k = bit count of x
def bitcount x -> k { ... }

// x < y
def less x y { ... }

// x = r mod q
def mod x q -> r { ... }

// x a bool
def bool x {range_1 x}

// bits(x) xor bits(y) = bits(z)
def xor x y -> z { ... }

// Blake2s(x) = y
def blake2s x -> y { ... }

// Blake2b(x) = y
def blake2b x -> y { ... }

// SHA-256(x) = y
def sha256 x -> y { ... }

etc.
```