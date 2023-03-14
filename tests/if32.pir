/* 'if' and 'if_else' functions over 32 bits. Run as follows:
   vamp-ir setup -o params.pp
   vamp-ir compile -u params.pp -s tests/if32.pir -o circuit.plonk
   vamp-ir prove -u params.pp -c circuit.plonk -o proof.plonk
   vamp-ir verify -u params.pp -c circuit.plonk -p proof.plonk
*/

// Ensure that the given argument is 1 or 0, and returns it
def bool x = { x*(x-1) = 0; x };

// Extract the 32 bits from a number argument
def range32 a = {
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
    def a16 = bool (fresh ((a\65536) % 2));
    def a17 = bool (fresh ((a\131072) % 2));
    def a18 = bool (fresh ((a\262144) % 2));
    def a19 = bool (fresh ((a\524288) % 2));
    def a20 = bool (fresh ((a\1048576) % 2));
    def a21 = bool (fresh ((a\2097152) % 2));
    def a22 = bool (fresh ((a\4194304) % 2));
    def a23 = bool (fresh ((a\8388608) % 2));
    def a24 = bool (fresh ((a\16777216) % 2));
    def a25 = bool (fresh ((a\33554432) % 2));
    def a26 = bool (fresh ((a\67108864) % 2));
    def a27 = bool (fresh ((a\134217728) % 2));
    def a28 = bool (fresh ((a\268435456) % 2));
    def a29 = bool (fresh ((a\536870912) % 2));
    def a30 = bool (fresh ((a\1073741824) % 2));
    def a31 = bool (fresh ((a\2147483648) % 2));
    a = a0 + 2*a1 + 4*a2 + 8*a3 + 16*a4 + 32*a5 + 64*a6 + 128*a7 + 256*a8 + 512*a9 + 1024*a10 + 2048*a11 + 
    	4096*a12 + 8192*a13 + 16384*a14 + 32768*a15 + 65536*a16 + 131072*a17 + 262144*a18 + 524288*a19 + 
    	1048576*a20 + 2097152*a21 + 4194304*a22 + 8388608*a23 + 16777216*a24 + 33554432*a25 + 67108864*a26 + 
    	134217728*a27 + 268435456*a28 + 536870912*a29 + 1073741824*a30 + 2147483648*a31;
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, ())
};

// Map a function over a tuple of size 32
def map32 f (g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,g27,g28,g29,g30,g31,gr) =
    (f g0, f g1, f g2, f g3, f g4, f g5, f g6, f g7, f g8, f g9, f g10, 
     f g11, f g12, f g13, f g14, f g15, f g16, f g17, f g18, f g19, f g20, 
     f g21, f g22, f g23, f g24, f g25, f g26, f g27, f g28, f g29, f g30, f g31, ());

// Return 0 if any bit is 0
def or32 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) =
    a0*a1*a2*a3*a4*a5*a6*a7*a8*a9*a10*a11*a12*a13*a14*a15*a16*a17*a18*a19*a20*a21*a22*a23*a24*a25*a26*a27*a28*a29*a30*a31;

// Check that a given 32-bit number is *not* 0
def nonZero32 a = or32 (map32 (fun x { 1-x }) (range32 a));

// Testing nonZero32
//nonZero32 1 = 0;
//nonZero32 25 = 0;
//nonZero32 234534564 = 0;
//nonZero32 0 = 1;

// If x is 0, then y must also be 0
// If x is nonZero, then y may be anything
def if x y = (nonZero32 x) * y;

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
//if_else 0 0 0 = 0;
//if_else 0 0 20 = 0;
//if_else 20 0 0 = 0;
//if_else 20 6 0 = 0;
// Note: all the tests together produce a TrimmingDegreeTooLarge error

if_else 20 0 5 = 100;
if_else 20 6 5 = 100;
if_else 0 6 5 = 6;



