/* Any value 0 <= x < 32 such that the last two bits 1, 0 is valid. For example
   10 is valid since its bit string is ...1010. Run as follows:
   vamp-ir setup -o params.pp
   vamp-ir compile -u params.pp -s tests/fresh.pir -o circuit.plonk
   vamp-ir prove -u params.pp -c circuit.plonk -o proof.plonk
   vamp-ir verify -u params.pp -c circuit.plonk -p proof.plonk
*/

pub a;
pub b;
def x = a + b;

// Ensure that the given argument is 1 or 0, and returns it

def bool x = { x*(x-1) = 0; x };

// Extract the 8 bits from a number argument

def range5 a = {
    // Make an expression whose witness is fresh's argument
    def a0:a1:a2:a3:a4:[] =
        fresh (((a\1) % 2):((a\2) % 2):((a\4) % 2):((a\8) % 2):((a\16) % 2):[]);
    def a0 = bool a0;
    def a1 = bool a1;
    def a2 = bool a2;
    def a3 = bool a3;
    def a4 = bool a4;
    a = a0 + 2*a1 + 4*a2 + 8*a3 + 16*a4;
    (a0, a1, a2, a3, a4, ())
};

def (0, 1, ar) = range5 x;
