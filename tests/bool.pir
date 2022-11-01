/* a must either be 0 or 1. Run as follows:
   vamp-ir setup -o params.pp
   vamp-ir compile -u params.pp -s tests/bool.pir -o circuit.plonk
   vamp-ir prove -u params.pp -c circuit.plonk -o proof.plonk
   vamp-ir verify -u params.pp -c circuit.plonk -p proof.plonk
*/
// defining a constant
def myval = 0;
// constraint expression
myval = 0;
// defining a function
def bool x = { x*(x-1) = 0 };
// calling a function
bool 1;
// solicit a user input
bool a;
