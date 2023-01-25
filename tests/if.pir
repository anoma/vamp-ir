/* And 'if' function, implementing the relation `(x=0) => (y=0). Run as follows:
   vamp-ir setup -o params.pp
   vamp-ir compile -u params.pp -s tests/if.pir -o circuit.plonk
   vamp-ir prove -u params.pp -c circuit.plonk -o proof.plonk
   vamp-ir verify -u params.pp -c circuit.plonk -p proof.plonk
*/

// Determine that a number is not 0
// a is non-zero iff there exists an a' s.t. a' * a = 1.
def nonZero a = {
    def ai = fresh (1|a);
    1 - ai * a
};

// Testing nonZero
nonZero 1 = 0;
nonZero 25 = 0;
nonZero 234534564 = 0;
nonZero 0 = 1;

// If x is 0, then y must also be 0
// If x is not 0, then y may be anything
def if x y = (nonZero x) * y;

// Testing if
if 0 0 = 0;
if 55 34 = 0;
if 55 0 = 0;
if 0 1 = 1;
if 0 20 = 20;

// User input
//if a b = 0;

// if x = 0 holds, then y must also = 0
// but if x /= 0, then z must = 0
def if_else x y z = if x y + x * z;

// Testing if_else
if_else 0 0 0 = 0;
if_else 0 0 20 = 0;
if_else 20 0 0 = 0;

if_else 20 0 5 = 100;
if_else 20 6 5 = 100;
if_else 0 6 5 = 6;








