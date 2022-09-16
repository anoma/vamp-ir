/* Any numbers a,b such that a^2+b^2=25 yield valid proofs. Run as follows:
   vamp-ir setup params.pp
   vamp-ir compile tests/pyt.pir params.pp circuit.plonk
   vamp-ir prove circuit.plonk params.pp proof.plonk
   vamp-ir verify circuit.plonk params.pp proof.plonk
   */
// computation + BIDMAS
def pyt a b { a^2 + b^2 };
// constrain computation
pyt x y = 25;
// let bindings
def a = 5;
a*5 = 25;