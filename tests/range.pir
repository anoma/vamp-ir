/* Any value 0 <= x < 32 such that the last two bits 1, 0 is valid. For example
   10 is valid since its bit string is ...1010. Run as follows:
   vamp-ir setup params.pp
   vamp-ir compile tests/range.pir params.pp circuit.plonk
   vamp-ir prove circuit.plonk params.pp proof.plonk
   vamp-ir verify circuit.plonk params.pp proof.plonk
*/

// Ensure that the given argument is 1 or 0

def bool x { x*(x-1) = 0 };

// Extract the 8 bits from a number argument

def range5 a {
    def a0 = fresh ((a\1) % 2);
    def a1 = fresh ((a\2) % 2);
    def a2 = fresh ((a\4) % 2);
    def a3 = fresh ((a\8) % 2);
    def a4 = fresh ((a\16) % 2);
    bool a0;
    bool a1;
    bool a2;
    bool a3;
    bool a4;
    a = a0 + 2*a1 + 4*a2 + 8*a3 + 16*a4;
    (a0, a1, a2, a3, a4, ())
};

def (0, 1, ar) = range5 x;
