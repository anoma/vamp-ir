/* Run as follows:
   vamp-ir setup -o params.pp
   vamp-ir compile -u params.pp -s tests/propagate.pir -o circuit.plonk
   vamp-ir prove -u params.pp -c circuit.plonk -o proof.plonk
   vamp-ir verify -u params.pp -c circuit.plonk -p proof.plonk
*/
def y = 2;
def x = y ^ 5;
x = 5;
