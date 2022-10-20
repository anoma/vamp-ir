/* Any value 0 <= x < 32 such that the last two bits 1, 0 is valid. For example
   10 is valid since its bit string is ...1010. Run as follows:
   vamp-ir setup -m 10 -o params.pp
   vamp-ir compile -u params.pp -s tests/range.pir -o circuit.plonk
   vamp-ir prove -u params.pp -c circuit.plonk -o proof.plonk
   vamp-ir verify -u params.pp -c circuit.plonk -p proof.plonk
*/
def (0, 1, ar) = range 5 x;
