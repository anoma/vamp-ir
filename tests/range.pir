/* Any value 0 <= x < 32 such that the last two bits 1, 0 is valid. For example
   10 is valid since its bit string is ...1010. Run as follows:
   vamp-ir setup params.pp
   vamp-ir compile tests/range.pir params.pp circuit.plonk
   vamp-ir prove circuit.plonk params.pp proof.plonk
   vamp-ir verify circuit.plonk params.pp proof.plonk
*/
def (0,1,a,b,c) = range 5 x;
