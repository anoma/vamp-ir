/* 'if' and 'if_else' functions over 16 bits. Run as follows:
   vamp-ir setup -o params.pp
   vamp-ir compile -u params.pp -s tests/if16.pir -o circuit.plonk
   vamp-ir prove -u params.pp -c circuit.plonk -o proof.plonk
   vamp-ir verify -u params.pp -c circuit.plonk -p proof.plonk
*/

// Ensure that the given argument is 1 or 0, and returns it
def bool x = { x*(x-1) = 0; x };

// Extract the 16 bits from a number argument
def range16 a = {
    def a0 = bool (fresh ((a\1) % 2));
    def a1 = bool (fresh ((a\2) % 2));
    def a2 = bool (fresh ((a\4) % 2));
    def a3 = bool (fresh ((a\8) % 2));
    def a4 = bool (fresh ((a\16) % 2));
    def a5 = bool (fresh ((a\32) % 2));
    def a6 = bool (fresh ((a\64) % 2));
    def a7 = bool (fresh ((a\128) % 2));
    def a8 = bool (fresh ((a\256) % 2));
    def a9 = bool (fresh ((a\512) % 2));
    def a10 = bool (fresh ((a\1024) % 2));
    def a11 = bool (fresh ((a\2048) % 2));
    def a12 = bool (fresh ((a\4096) % 2));
    def a13 = bool (fresh ((a\8192) % 2));
    def a14 = bool (fresh ((a\16384) % 2));
    def a15 = bool (fresh ((a\32768) % 2));
    a = a0 + 2*a1 + 4*a2 + 8*a3 + 16*a4 + 32*a5 + 64*a6 + 128*a7 + 256*a8 + 512*a9 + 1024*a10 + 2048*a11 + 
    	4096*a12 + 8192*a13 + 16384*a14 + 32768*a15;
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, ())
};

// Map a function over a tuple of size 16
def map16 f (g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,gr) =
    (f g0, f g1, f g2, f g3, f g4, f g5, f g6, f g7, f g8, f g9, f g10, 
     f g11, f g12, f g13, f g14, f g15, ());

// Return 0 if any bit is 0
def or16 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,ar) =
    a0*a1*a2*a3*a4*a5*a6*a7*a8*a9*a10*a11*a12*a13*a14*a15;

// Check that a given 16-bit number is *not* 0
def nonZero16 a = or16 (map16 (fun x { 1-x }) (range16 a));

// Testing nonZero16
//nonZero16 1 = 0;
//nonZero16 25 = 0;
//nonZero16 4567 = 0;
//nonZero16 0 = 1;

// If x is 0, then y must also be 0
// If x is nonZero, then y may be anything
def if x y = (nonZero16 x) * y;

// Testing if
//if 0 0 = 0;
//if 55 34 = 0;
//if 55 0 = 0;
//if 0 1 = 1;
//if 0 20 = 20;

// if x = 0 holds, then y must also = 0
// but if x /= 0, then z must = 0
def if_else x y z = if x y + x * z;

// Testing if_else
// Note: all the tests together produce a TrimmingDegreeTooLarge error
if_else 0 0 0 = 0;
if_else 0 0 20 = 0;
if_else 20 0 0 = 0;
if_else 20 6 0 = 0;

if_else 20 0 5 = 100;
if_else 20 6 5 = 100;
if_else 0 6 5 = 6;



