/* And 'if' function, implementing the relation `(x=0) => (y=0). Run as follows:
   vamp-ir setup -o params.pp
   vamp-ir compile -u params.pp -s tests/if.pir -o circuit.plonk
   vamp-ir prove -u params.pp -c circuit.plonk -o proof.plonk
   vamp-ir verify -u params.pp -c circuit.plonk -p proof.plonk
*/

// Determine that a number is not 0
def nonZero a = {
    def ai = fresh (1|a);
    1 - ai * a
};

// Testing nonZero
nonZero 1 = 0;
nonZero 25 = 0;
nonZero 234534564 = 0;
//nonZero 0 = 0;

// If x is 0, then y must also be 0
// If x is not 0, then y may be anything
def if x y = (nonZero x) * y;

// Calling the function
if 0 0 = 0;
if 55 34 = 0;
if 55 0 = 0;
//if 0 1 = 0;

// User input
//if a b;




