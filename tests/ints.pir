/* An script to test out large integer arithmetic. x must be
   342342428479792353453543986 and c must be -32. Run
   as follows:
   vamp-ir setup params.pp
   vamp-ir compile tests/ints.pir params.pp circuit.plonk
   vamp-ir prove circuit.plonk params.pp proof.plonk
   vamp-ir verify circuit.plonk params.pp proof.plonk
*/

// Make a large number
def a = 342342428479792353453543987;

// Check if computations with it are correct;
a*a = 117198338337441742664441569861251354629164970143856169;
a+1 = 342342428479792353453543988;

// Check if large int prompts work
a = x+1;

// Check negative number functioning
def b = (-8);
b*4 = c;
