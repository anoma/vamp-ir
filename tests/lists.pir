/* Testing out match expression. Run as follows:
   vamp-ir setup params.pp
   vamp-ir compile tests/lists.pir params.pp circuit.plonk
   vamp-ir prove circuit.plonk params.pp proof.plonk
   vamp-ir verify circuit.plonk params.pp proof.plonk
*/

def x = 5:(6:[]);
