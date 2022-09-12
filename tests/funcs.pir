/* Any solution such that b=c, d=e, and f=g is valid. Run as follows:
   vamp-ir setup params.pp
   vamp-ir compile tests/funcs.pir params.pp circuit.plonk
   vamp-ir prove circuit.plonk params.pp proof.plonk
   vamp-ir verify circuit.plonk params.pp proof.plonk
   */
// defining a two parameter function
def myeq a b { a = b };
// solicit equal user inputs
myeq b c;
// currying example
(myeq d) e;
// currying with definition
def mycurry = myeq f;
// use defined curry
mycurry g;
