/* Any value 0 <= x < 32 such that the last two bits 1, 0 is valid. For example
   10 is valid since its bit string is ...1010. Run as follows:
   vamp-ir setup -o params.pp
   vamp-ir compile -u params.pp -s tests/range.pir -o circuit.plonk
   vamp-ir prove -u params.pp -c circuit.plonk -o proof.plonk
   vamp-ir verify -u params.pp -c circuit.plonk -p proof.plonk
*/

pub x, pi;

// Ensure that the given argument is 1 or 0, and returns it

def bool x = { x*(x-1) = 0; x };

// Extract the 8 bits from a number argument

def range5 a = {
    def a0 = bool (fresh ((a\1) % 2));
    def a1 = bool (fresh ((a\2) % 2));
    def a2 = bool (fresh ((a\4) % 2));
    def a3 = bool (fresh ((a\8) % 2));
    def a4 = bool (fresh ((a\16) % 2));
    a = a0 + 2*a1 + 4*a2 + 8*a3 + 16*a4;
    (a0, a1, a2, a3, a4, ())
};

def (0, 1, ar) = range5 x;

o = 3*l*r + 5*l + 6*r + 17 + pi;

c = a*b;
a = 2*d + (1 + 2);
b = 5*e + 7;
f - 13 = 11*c;
g = f + pi;

h = 5*b*a;
i = 4*c - 3*h;

j = k + p;
p = 2*j;

s = 2*q;
t = 5*q;
u = 4*s;
v = 3*q;
w = u-v;
z = 0-w;
zz = 2 + 3;