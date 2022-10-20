/* Any solution such that b=c, d=e, and f=g is valid. Run as follows:
   vamp-ir setup -m 10 -o params.pp
   vamp-ir compile -u params.pp -s tests/funcs.pir -o circuit.plonk
   vamp-ir prove -u params.pp -c circuit.plonk -o proof.plonk
   vamp-ir verify -u params.pp -c circuit.plonk -p proof.plonk
   */
// defining a two parameter function.
// function body contains two constraints.
def myeq a b { a = b; b = a };
// solicit equal user inputs
myeq b c;
// currying example
(myeq d) e;
// currying with definition
def mycurry = myeq f;
// use defined curry
mycurry g;
