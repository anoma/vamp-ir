/* Any numbers a,b such that a^2+b^2=25 yield valid proofs. Run as follows:
   vamp-ir setup -o params.pp
   vamp-ir compile -s test/pyt.pir -o pyt.circ
   vamp-ir synth -u params.pp -s pyt.circ -o circuit.plonk
   vamp-ir prove -u params.pp -c circuit.plonk -o proof.plonk
   vamp-ir verify -u params.pp -c circuit.plonk -p proof.plonk
   */
// computation + BIDMAS
def pyt a b = a^2 + b^2;
// constrain computation
pyt x y = 25;
// let bindings
def a = 5;
a*5 = 25;
