/* a must either be 0 or 1. Run as follows:
   vamp-ir setup params.pp
   vamp-ir compile tests/bool.pir params.pp circuit.plonk
   vamp-ir prove circuit.plonk params.pp proof.plonk
   vamp-ir verify circuit.plonk params.pp proof.plonk
*/
// defining a constant
def myval = 0;
// constraint expression
myval = 0;
// defining a function
def bool x { x*(x-1) = 0 };
// calling a function
bool 1;
// solicit a user input
bool a;
