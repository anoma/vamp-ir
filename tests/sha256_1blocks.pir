/* An implementation of standard arithmetic logic unit operations in vampir. Run
   as follows:
   vamp-ir setup params.pp
   vamp-ir compile tests/alu.pir params.pp circuit.plonk
   vamp-ir prove circuit.plonk params.pp proof.plonk
   vamp-ir verify circuit.plonk params.pp proof.plonk
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
    a = a0 + 2*a1 + 4*a2 + 8*a3 + 16*a4 + 32*a5 + 64*a6 + 128*a7 + 256*a8 + 512*a9 + 1024*a10 + 2048*a11 + 4096*a12 + 8192*a13 + 16384*a14 + 32768*a15 + 65536*a16 + 131072*a17 + 262144*a18 + 524288*a19 + 1048576*a20 + 2097152*a21 + 4194304*a22 + 8388608*a23 + 16777216*a24 + 33554432*a25 + 67108864*a26 + 134217728*a27 + 268435456*a28 + 536870912*a29 + 1073741824*a30 + 2147483648*a31;
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, ())
};

// Pair up corresponding elements of each tuple
def zip32 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) (b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23,b24,b25,b26,b27,b28,b29,b30,b31,br) = {
    ((a0,b0),(a1,b1),(a2,b2),(a3,b3),(a4,b4),(a5,b5),(a6,b6),(a7,b7),(a8,b8),(a9,b9),(a10,b10),(a11,b11),(a12,b12),(a13,b13),(a14,b14),(a15,b15),(a16,b16),(a17,b17),(a18,b18),(a19,b19),(a20,b20),(a21,b21),(a22,b22),(a23,b23),(a24,b24),(a25,b25),(a26,b26),(a27,b27),(a28,b28),(a29,b29),(a30,b30),(a31,b31),())
};

// Apply function to each element of tuple
def map f (g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,g27,g28,g29,g30,g31,gr) = {
    (f g0, f g1, f g2, f g3, f g4, f g5, f g6, f g7, f g8, f g9, f g10, f g11, f g12, f g13, f g14, f g15, f g16, f g17, f g18, f g19, f g20, f g21, f g22, f g23, f g24, f g25, f g26, f g27, f g28, f g29, f g30, f g31, ())
};

// Multiply each tuple element by corresponding unit
def combine32 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31, ar) = {
    a0 + 2*a1 + 4*a2 + 8*a3 + 16*a4 + 32*a5 + 64*a6 + 128*a7 + 256*a8 + 512*a9 + 1024*a10 + 2048*a11 + 4096*a12 + 8192*a13 + 16384*a14 + 32768*a15 + 65536*a16 + 131072*a17 + 262144*a18 + 524288*a19 + 1048576*a20 + 2097152*a21 + 4194304*a22 + 8388608*a23 + 16777216*a24 + 33554432*a25 + 67108864*a26 + 134217728*a27 + 268435456*a28 + 536870912*a29 + 1073741824*a30 + 2147483648*a31
};


// Apply given function to corresponding bit-pairs in bit representation
def bitwise32 g a b = {
    def zipped = zip32 (range32 a) (range32 b);
    def new_bits = map g zipped;
    combine32 new_bits
};

def bitwise_single g a = {
    def a_bits = range32 a;
    def new_bits = map g a_bits;
    combine32 new_bits
};

// Definition of xor for domain {0, 1}^2
def bit_xor (a,b) = { a*(1-b)+(1-a)*b };

// Definition of and for domain {0, 1}^2
def bit_and (a,b) = { a*b };

// Definition of not for domain {0, 1}
def bit_not a = { 1-a };

// Definition of bitwise xor for 32 bit values
def xor32 = bitwise32 bit_xor;

// Definition of bitwise and for 32 bit values
def and32 = bitwise32 bit_and;

// Definition of bitwise not for 32 bit values
def not32 = bitwise_single bit_not;
// 3284400724 = not32 1010566571;

// Reminder operation
def rem32 x = {
    def q = fresh(x \ 4294967296);
    def r = fresh(x % 4294967296);
    x = q * 4294967296 + r;
    range32 q;
    r
};

def rem2 x = {
    def q = fresh(x \ 2);
    def r = fresh(x % 2);
    x = q * 4294967296 + r;
    range32 q;
    r
};


// SHA256 constants
def K0 = 0x428a2f98;
def K1 = 0x71374491;
def K2 = 0xb5c0fbcf;
def K3 = 0xe9b5dba5;
def K4 = 0x3956c25b;
def K5 = 0x59f111f1;
def K6 = 0x923f82a4;
def K7 = 0xab1c5ed5;
def K8 = 0xd807aa98;
def K9 = 0x12835b01;
def K10 = 0x243185be;
def K11 = 0x550c7dc3;
def K12 = 0x72be5d74;
def K13 = 0x80deb1fe;
def K14 = 0x9bdc06a7;
def K15 = 0xc19bf174;
def K16 = 0xe49b69c1;
def K17 = 0xefbe4786;
def K18 = 0x0fc19dc6;
def K19 = 0x240ca1cc;
def K20 = 0x2de92c6f;
def K21 = 0x4a7484aa;
def K22 = 0x5cb0a9dc;
def K23 = 0x76f988da;
def K24 = 0x983e5152;
def K25 = 0xa831c66d;
def K26 = 0xb00327c8;
def K27 = 0xbf597fc7;
def K28 = 0xc6e00bf3;
def K29 = 0xd5a79147;
def K30 = 0x06ca6351;
def K31 = 0x14292967;
def K32 = 0x27b70a85;
def K33 = 0x2e1b2138;
def K34 = 0x4d2c6dfc;
def K35 = 0x53380d13;
def K36 = 0x650a7354;
def K37 = 0x766a0abb;
def K38 = 0x81c2c92e;
def K39 = 0x92722c85;
def K40 = 0xa2bfe8a1;
def K41 = 0xa81a664b;
def K42 = 0xc24b8b70;
def K43 = 0xc76c51a3;
def K44 = 0xd192e819;
def K45 = 0xd6990624;
def K46 = 0xf40e3585;
def K47 = 0x106aa070;
def K48 = 0x19a4c116;
def K49 = 0x1e376c08;
def K50 = 0x2748774c;
def K51 = 0x34b0bcb5;
def K52 = 0x391c0cb3;
def K53 = 0x4ed8aa4a;
def K54 = 0x5b9cca4f;
def K55 = 0x682e6ff3;
def K56 = 0x748f82ee;
def K57 = 0x78a5636f;
def K58 = 0x84c87814;
def K59 = 0x8cc70208;
def K60 = 0x90befffa;
def K61 = 0xa4506ceb;
def K62 = 0xbef9a3f7;
def K63 = 0xc67178f2;

// Sigma 0
def sigma0 x = {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range32 x;
    def (x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,xr) = (a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a0,a1,a2,a3,a4,a5,a6,());
    def (y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17,y18,y19,y20,y21,y22,y23,y24,y25,y26,y27,y28,y29,y30,y31,yr) = (a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,());
    def (z0,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,z11,z12,z13,z14,z15,z16,z17,z18,z19,z20,z21,z22,z23,z24,z25,z26,z27,z28,z29,z30,z31,zr) = (a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,0,0,0,());
    combine32 (rem2((x0+y0+z0)%2), rem2((x1+y1+z1)%2), rem2((x2+y2+z2)%2), rem2((x3+y3+z3)%2), rem2((x4+y4+z4)%2), rem2((x5+y5+z5)%2), rem2((x6+y6+z6)%2), rem2((x7+y7+z7)%2), rem2((x8+y8+z8)%2), rem2((x9+y9+z9)%2), rem2((x10+y10+z10)%2), rem2((x11+y11+z11)%2), rem2((x12+y12+z12)%2), rem2((x13+y13+z13)%2), rem2((x14+y14+z14)%2), rem2((x15+y15+z15)%2), rem2((x16+y16+z16)%2), rem2((x17+y17+z17)%2), rem2((x18+y18+z18)%2), rem2((x19+y19+z19)%2), rem2((x20+y20+z20)%2), rem2((x21+y21+z21)%2), rem2((x22+y22+z22)%2), rem2((x23+y23+z23)%2), rem2((x24+y24+z24)%2), rem2((x25+y25+z25)%2), rem2((x26+y26+z26)%2), rem2((x27+y27+z27)%2), rem2((x28+y28+z28)%2), rem2((x29+y29+z29)%2), rem2((x30+y30+z30)%2), rem2((x31+y31+z31)%2), ())
    };

def sigma1 x = {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range32 x;
    def (x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,xr) = (a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,());
    def (y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17,y18,y19,y20,y21,y22,y23,y24,y25,y26,y27,y28,y29,y30,y31,yr) = (a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,());
    def (z0,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,z11,z12,z13,z14,z15,z16,z17,z18,z19,z20,z21,z22,z23,z24,z25,z26,z27,z28,z29,z30,z31,zr) = (a10, a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,0,0,0,0,0,0,0,0,0,0,());
    combine32 (rem2((x0+y0+z0)%2), rem2((x1+y1+z1)%2), rem2((x2+y2+z2)%2), rem2((x3+y3+z3)%2), rem2((x4+y4+z4)%2), rem2((x5+y5+z5)%2), rem2((x6+y6+z6)%2), rem2((x7+y7+z7)%2), rem2((x8+y8+z8)%2), rem2((x9+y9+z9)%2), rem2((x10+y10+z10)%2), rem2((x11+y11+z11)%2), rem2((x12+y12+z12)%2), rem2((x13+y13+z13)%2), rem2((x14+y14+z14)%2), rem2((x15+y15+z15)%2), rem2((x16+y16+z16)%2), rem2((x17+y17+z17)%2), rem2((x18+y18+z18)%2), rem2((x19+y19+z19)%2), rem2((x20+y20+z20)%2), rem2((x21+y21+z21)%2), rem2((x22+y22+z22)%2), rem2((x23+y23+z23)%2), rem2((x24+y24+z24)%2), rem2((x25+y25+z25)%2), rem2((x26+y26+z26)%2), rem2((x27+y27+z27)%2), rem2((x28+y28+z28)%2), rem2((x29+y29+z29)%2), rem2((x30+y30+z30)%2), rem2((x31+y31+z31)%2), ())
    };

def SIGMA0 x = {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range32 x;
    def (x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,xr) = (a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a0,a1,());
    def (y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17,y18,y19,y20,y21,y22,y23,y24,y25,y26,y27,y28,y29,y30,y31,yr) = (a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,());
    def (z0,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,z11,z12,z13,z14,z15,z16,z17,z18,z19,z20,z21,z22,z23,z24,z25,z26,z27,z28,z29,z30,z31,zr) = (a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,());
    combine32 (rem2((x0+y0+z0)%2), rem2((x1+y1+z1)%2), rem2((x2+y2+z2)%2), rem2((x3+y3+z3)%2), rem2((x4+y4+z4)%2), rem2((x5+y5+z5)%2), rem2((x6+y6+z6)%2), rem2((x7+y7+z7)%2), rem2((x8+y8+z8)%2), rem2((x9+y9+z9)%2), rem2((x10+y10+z10)%2), rem2((x11+y11+z11)%2), rem2((x12+y12+z12)%2), rem2((x13+y13+z13)%2), rem2((x14+y14+z14)%2), rem2((x15+y15+z15)%2), rem2((x16+y16+z16)%2), rem2((x17+y17+z17)%2), rem2((x18+y18+z18)%2), rem2((x19+y19+z19)%2), rem2((x20+y20+z20)%2), rem2((x21+y21+z21)%2), rem2((x22+y22+z22)%2), rem2((x23+y23+z23)%2), rem2((x24+y24+z24)%2), rem2((x25+y25+z25)%2), rem2((x26+y26+z26)%2), rem2((x27+y27+z27)%2), rem2((x28+y28+z28)%2), rem2((x29+y29+z29)%2), rem2((x30+y30+z30)%2), rem2((x31+y31+z31)%2), ())
    };

def SIGMA1 x = {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range32 x;
    def (x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,xr) = (a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a0,a1,a2,a3,a4,a5,());
    def (y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17,y18,y19,y20,y21,y22,y23,y24,y25,y26,y27,y28,y29,y30,y31,yr) = (a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,());
    def (z0,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,z11,z12,z13,z14,z15,z16,z17,z18,z19,z20,z21,z22,z23,z24,z25,z26,z27,z28,z29,z30,z31,zr) = (a25,a26,a27,a28,a29,a30,a31,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,());
    combine32 (rem2((x0+y0+z0)%2), rem2((x1+y1+z1)%2), rem2((x2+y2+z2)%2), rem2((x3+y3+z3)%2), rem2((x4+y4+z4)%2), rem2((x5+y5+z5)%2), rem2((x6+y6+z6)%2), rem2((x7+y7+z7)%2), rem2((x8+y8+z8)%2), rem2((x9+y9+z9)%2), rem2((x10+y10+z10)%2), rem2((x11+y11+z11)%2), rem2((x12+y12+z12)%2), rem2((x13+y13+z13)%2), rem2((x14+y14+z14)%2), rem2((x15+y15+z15)%2), rem2((x16+y16+z16)%2), rem2((x17+y17+z17)%2), rem2((x18+y18+z18)%2), rem2((x19+y19+z19)%2), rem2((x20+y20+z20)%2), rem2((x21+y21+z21)%2), rem2((x22+y22+z22)%2), rem2((x23+y23+z23)%2), rem2((x24+y24+z24)%2), rem2((x25+y25+z25)%2), rem2((x26+y26+z26)%2), rem2((x27+y27+z27)%2), rem2((x28+y28+z28)%2), rem2((x29+y29+z29)%2), rem2((x30+y30+z30)%2), rem2((x31+y31+z31)%2), ())
    };


// Choose function
def ch x y z = {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range32 x;
    def (b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23,b24,b25,b26,b27,b28,b29,b30,b31,br) = range32 y;
    def (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,c31,cr) = range32 z;
    combine32 (rem2((a0*b0)+(1-a0)*(c0)), rem2((a1*b1)+(1-a1)*(c1)), rem2((a2*b2)+(1-a2)*(c2)), rem2((a3*b3)+(1-a3)*(c3)), rem2((a4*b4)+(1-a4)*(c4)), rem2((a5*b5)+(1-a5)*(c5)), rem2((a6*b6)+(1-a6)*(c6)), rem2((a7*b7)+(1-a7)*(c7)), rem2((a8*b8)+(1-a8)*(c8)), rem2((a9*b9)+(1-a9)*(c9)), rem2((a10*b10)+(1-a10)*(c10)), rem2((a11*b11)+(1-a11)*(c11)), rem2((a12*b12)+(1-a12)*(c12)), rem2((a13*b13)+(1-a13)*(c13)), rem2((a14*b14)+(1-a14)*(c14)), rem2((a15*b15)+(1-a15)*(c15)), rem2((a16*b16)+(1-a16)*(c16)), rem2((a17*b17)+(1-a17)*(c17)), rem2((a18*b18)+(1-a18)*(c18)), rem2((a19*b19)+(1-a19)*(c19)), rem2((a20*b20)+(1-a20)*(c20)), rem2((a21*b21)+(1-a21)*(c21)), rem2((a22*b22)+(1-a22)*(c22)), rem2((a23*b23)+(1-a23)*(c23)), rem2((a24*b24)+(1-a24)*(c24)), rem2((a25*b25)+(1-a25)*(c25)), rem2((a26*b26)+(1-a26)*(c26)), rem2((a27*b27)+(1-a27)*(c27)), rem2((a28*b28)+(1-a28)*(c28)), rem2((a29*b29)+(1-a29)*(c29)), rem2((a30*b30)+(1-a30)*(c30)), rem2((a31*b31)+(1-a31)*(c31)), ())
    };
// 528861580 = ch 0x510e527f 0x9b05688c 0x1f83d9ab;

// Majority function
def maj x y z = {
    xor32 (xor32 (and32 x y) (and32 x z)) (and32 y z)
    };
// 269374079 = maj 0x510e527f 0x2a0e527f 0x149fbc13;


// Convert input in pure binary form. The message we are going to hash is "abc".
// The binary representation of "abc" is 01100001 01100010 01100011.
// We need to order in rows of for elements so that we have rows of 32 bits.


// Preprocessing
// The input needs to be a multiple of 512, and at the moment is 24.
// We are going to round up to 512.
// The "abc" message we are going to use is:
def H0 = 0x6a09e667;
def H1 = 0xbb67ae85;
def H2 = 0x3c6ef372;
def H3 = 0xa54ff53a;
def H4 = 0x510e527f;
def H5 = 0x9b05688c;
def H6 = 0x1f83d9ab;
def H7 = 0x5be0cd19;

// PARSING THE MESSAGE
// We need to parse the message into 32 bit words.
def parsing m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 = {
    // from 0 to 15 wi = mi
    def w0 = m0;
    def w1 = m1;
    def w2 = m2;
    def w3 = m3;
    def w4 = m4;
    def w5 = m5;
    def w6 = m6;
    def w7 = m7;
    def w8 = m8;
    def w9 = m9;
    def w10 = m10;
    def w11 = m11;
    def w12 = m12;
    def w13 = m13;
    def w14 = m14;
    def w15 = m15;
// from 16 to 63 wi = sigma1(wi-2) + wi-7 + sigma0(wi-15) + wi-16
def w16 = rem32( (sigma1 w14 + w9 + sigma0 w1 + w0));
def w17 = rem32( (sigma1 w15 + w10 + sigma0 w2 + w1));
def w18 = rem32( (sigma1 w16 + w11 + sigma0 w3 + w2));
def w19 = rem32( (sigma1 w17 + w12 + sigma0 w4 + w3));
def w20 = rem32( (sigma1 w18 + w13 + sigma0 w5 + w4));
def w21 = rem32( (sigma1 w19 + w14 + sigma0 w6 + w5));
def w22 = rem32( (sigma1 w20 + w15 + sigma0 w7 + w6));
def w23 = rem32( (sigma1 w21 + w16 + sigma0 w8 + w7));
def w24 = rem32( (sigma1 w22 + w17 + sigma0 w9 + w8));
def w25 = rem32( (sigma1 w23 + w18 + sigma0 w10 + w9));
def w26 = rem32( (sigma1 w24 + w19 + sigma0 w11 + w10));
def w27 = rem32( (sigma1 w25 + w20 + sigma0 w12 + w11));
def w28 = rem32( (sigma1 w26 + w21 + sigma0 w13 + w12));
def w29 = rem32( (sigma1 w27 + w22 + sigma0 w14 + w13));
def w30 = rem32( (sigma1 w28 + w23 + sigma0 w15 + w14));
def w31 = rem32( (sigma1 w29 + w24 + sigma0 w16 + w15));
def w32 = rem32( (sigma1 w30 + w25 + sigma0 w17 + w16));
def w33 = rem32( (sigma1 w31 + w26 + sigma0 w18 + w17));
def w34 = rem32( (sigma1 w32 + w27 + sigma0 w19 + w18));
def w35 = rem32( (sigma1 w33 + w28 + sigma0 w20 + w19));
def w36 = rem32( (sigma1 w34 + w29 + sigma0 w21 + w20));
def w37 = rem32( (sigma1 w35 + w30 + sigma0 w22 + w21));
def w38 = rem32( (sigma1 w36 + w31 + sigma0 w23 + w22));
def w39 = rem32( (sigma1 w37 + w32 + sigma0 w24 + w23));
def w40 = rem32( (sigma1 w38 + w33 + sigma0 w25 + w24));
def w41 = rem32( (sigma1 w39 + w34 + sigma0 w26 + w25));
def w42 = rem32( (sigma1 w40 + w35 + sigma0 w27 + w26));
def w43 = rem32( (sigma1 w41 + w36 + sigma0 w28 + w27));
def w44 = rem32( (sigma1 w42 + w37 + sigma0 w29 + w28));
def w45 = rem32( (sigma1 w43 + w38 + sigma0 w30 + w29));
def w46 = rem32( (sigma1 w44 + w39 + sigma0 w31 + w30));
def w47 = rem32( (sigma1 w45 + w40 + sigma0 w32 + w31));
def w48 = rem32( (sigma1 w46 + w41 + sigma0 w33 + w32));
def w49 = rem32( (sigma1 w47 + w42 + sigma0 w34 + w33));
def w50 = rem32( (sigma1 w48 + w43 + sigma0 w35 + w34));
def w51 = rem32( (sigma1 w49 + w44 + sigma0 w36 + w35));
def w52 = rem32( (sigma1 w50 + w45 + sigma0 w37 + w36));
def w53 = rem32( (sigma1 w51 + w46 + sigma0 w38 + w37));
def w54 = rem32( (sigma1 w52 + w47 + sigma0 w39 + w38));
def w55 = rem32( (sigma1 w53 + w48 + sigma0 w40 + w39));
def w56 = rem32( (sigma1 w54 + w49 + sigma0 w41 + w40));
def w57 = rem32( (sigma1 w55 + w50 + sigma0 w42 + w41));
def w58 = rem32( (sigma1 w56 + w51 + sigma0 w43 + w42));
def w59 = rem32( (sigma1 w57 + w52 + sigma0 w44 + w43));
def w60 = rem32( (sigma1 w58 + w53 + sigma0 w45 + w44));
def w61 = rem32( (sigma1 w59 + w54 + sigma0 w46 + w45));
def w62 = rem32( (sigma1 w60 + w55 + sigma0 w47 + w46));
def w63 = rem32( (sigma1 w61 + w56 + sigma0 w48 + w47));

def H0 = 0x6a09e667;
def H1 = 0xbb67ae85;
def H2 = 0x3c6ef372;
def H3 = 0xa54ff53a;
def H4 = 0x510e527f;
def H5 = 0x9b05688c;
def H6 = 0x1f83d9ab;
def H7 = 0x5be0cd19;

def step a b c d e f g h K w = {
    def t1 = rem32 ( h + (SIGMA1 e) + (ch e f g) + K + w);
    def t2 = rem32 (SIGMA0 a + (maj a b c));
    def h = g;
    def g = f;
    def f = e;
    def e = rem32 (d + t1);
    def d = c;
    def c = b;
    def b = a;
    def a = rem32 (t1 + t2);
    (a, b, c, d, e, f, g, h)
};

def process_block H0 H1 H2 H3 H4 H5 H6 H7 m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 = {
    def (w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18,w19,w20,w21,w22,w23,w24,w25,w26,w27,w28,w29,w30,w31,w32,w33,w34,w35,w36,w37,w38,w39,w40,w41,w42,w43,w44,w45,w46,w47,w48,w49,w50,w51,w52,w53,w54,w55,w56,w57,w58,w59,w60,w61,w62,w63) = parsing m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15;
    def (a_0, b_0, c_0, d_0, e_0, f_0, g_0, h_0) = step H0 H1 H2 H3 H4 H5 H6 H7 K0 w0;
    def (a_1, b_1, c_1, d_1, e_1, f_1, g_1, h_1) = step a_0 b_0 c_0 d_0 e_0 f_0 g_0 h_0 K1 w1;
    def (a_2, b_2, c_2, d_2, e_2, f_2, g_2, h_2) = step a_1 b_1 c_1 d_1 e_1 f_1 g_1 h_1 K2 w2;
    def (a_3, b_3, c_3, d_3, e_3, f_3, g_3, h_3) = step a_2 b_2 c_2 d_2 e_2 f_2 g_2 h_2 K3 w3;
    def (a_4, b_4, c_4, d_4, e_4, f_4, g_4, h_4) = step a_3 b_3 c_3 d_3 e_3 f_3 g_3 h_3 K4 w4;
    def (a_5, b_5, c_5, d_5, e_5, f_5, g_5, h_5) = step a_4 b_4 c_4 d_4 e_4 f_4 g_4 h_4 K5 w5;
    def (a_6, b_6, c_6, d_6, e_6, f_6, g_6, h_6) = step a_5 b_5 c_5 d_5 e_5 f_5 g_5 h_5 K6 w6;
    def (a_7, b_7, c_7, d_7, e_7, f_7, g_7, h_7) = step a_6 b_6 c_6 d_6 e_6 f_6 g_6 h_6 K7 w7;
    def (a_8, b_8, c_8, d_8, e_8, f_8, g_8, h_8) = step a_7 b_7 c_7 d_7 e_7 f_7 g_7 h_7 K8 w8;
    def (a_9, b_9, c_9, d_9, e_9, f_9, g_9, h_9) = step a_8 b_8 c_8 d_8 e_8 f_8 g_8 h_8 K9 w9;
    def (a_10, b_10, c_10, d_10, e_10, f_10, g_10, h_10) = step a_9 b_9 c_9 d_9 e_9 f_9 g_9 h_9 K10 w10;
    def (a_11, b_11, c_11, d_11, e_11, f_11, g_11, h_11) = step a_10 b_10 c_10 d_10 e_10 f_10 g_10 h_10 K11 w11;
    def (a_12, b_12, c_12, d_12, e_12, f_12, g_12, h_12) = step a_11 b_11 c_11 d_11 e_11 f_11 g_11 h_11 K12 w12;
    def (a_13, b_13, c_13, d_13, e_13, f_13, g_13, h_13) = step a_12 b_12 c_12 d_12 e_12 f_12 g_12 h_12 K13 w13;
    def (a_14, b_14, c_14, d_14, e_14, f_14, g_14, h_14) = step a_13 b_13 c_13 d_13 e_13 f_13 g_13 h_13 K14 w14;
    def (a_15, b_15, c_15, d_15, e_15, f_15, g_15, h_15) = step a_14 b_14 c_14 d_14 e_14 f_14 g_14 h_14 K15 w15;
    def (a_16, b_16, c_16, d_16, e_16, f_16, g_16, h_16) = step a_15 b_15 c_15 d_15 e_15 f_15 g_15 h_15 K16 w16;
    def (a_17, b_17, c_17, d_17, e_17, f_17, g_17, h_17) = step a_16 b_16 c_16 d_16 e_16 f_16 g_16 h_16 K17 w17;
    def (a_18, b_18, c_18, d_18, e_18, f_18, g_18, h_18) = step a_17 b_17 c_17 d_17 e_17 f_17 g_17 h_17 K18 w18;
    def (a_19, b_19, c_19, d_19, e_19, f_19, g_19, h_19) = step a_18 b_18 c_18 d_18 e_18 f_18 g_18 h_18 K19 w19;
    def (a_20, b_20, c_20, d_20, e_20, f_20, g_20, h_20) = step a_19 b_19 c_19 d_19 e_19 f_19 g_19 h_19 K20 w20;
    def (a_21, b_21, c_21, d_21, e_21, f_21, g_21, h_21) = step a_20 b_20 c_20 d_20 e_20 f_20 g_20 h_20 K21 w21;
    def (a_22, b_22, c_22, d_22, e_22, f_22, g_22, h_22) = step a_21 b_21 c_21 d_21 e_21 f_21 g_21 h_21 K22 w22;
    def (a_23, b_23, c_23, d_23, e_23, f_23, g_23, h_23) = step a_22 b_22 c_22 d_22 e_22 f_22 g_22 h_22 K23 w23;
    def (a_24, b_24, c_24, d_24, e_24, f_24, g_24, h_24) = step a_23 b_23 c_23 d_23 e_23 f_23 g_23 h_23 K24 w24;
    def (a_25, b_25, c_25, d_25, e_25, f_25, g_25, h_25) = step a_24 b_24 c_24 d_24 e_24 f_24 g_24 h_24 K25 w25;
    def (a_26, b_26, c_26, d_26, e_26, f_26, g_26, h_26) = step a_25 b_25 c_25 d_25 e_25 f_25 g_25 h_25 K26 w26;
    def (a_27, b_27, c_27, d_27, e_27, f_27, g_27, h_27) = step a_26 b_26 c_26 d_26 e_26 f_26 g_26 h_26 K27 w27;
    def (a_28, b_28, c_28, d_28, e_28, f_28, g_28, h_28) = step a_27 b_27 c_27 d_27 e_27 f_27 g_27 h_27 K28 w28;
    def (a_29, b_29, c_29, d_29, e_29, f_29, g_29, h_29) = step a_28 b_28 c_28 d_28 e_28 f_28 g_28 h_28 K29 w29;
    def (a_30, b_30, c_30, d_30, e_30, f_30, g_30, h_30) = step a_29 b_29 c_29 d_29 e_29 f_29 g_29 h_29 K30 w30;
    def (a_31, b_31, c_31, d_31, e_31, f_31, g_31, h_31) = step a_30 b_30 c_30 d_30 e_30 f_30 g_30 h_30 K31 w31;
    def (a_32, b_32, c_32, d_32, e_32, f_32, g_32, h_32) = step a_31 b_31 c_31 d_31 e_31 f_31 g_31 h_31 K32 w32;
    def (a_33, b_33, c_33, d_33, e_33, f_33, g_33, h_33) = step a_32 b_32 c_32 d_32 e_32 f_32 g_32 h_32 K33 w33;
    def (a_34, b_34, c_34, d_34, e_34, f_34, g_34, h_34) = step a_33 b_33 c_33 d_33 e_33 f_33 g_33 h_33 K34 w34;
    def (a_35, b_35, c_35, d_35, e_35, f_35, g_35, h_35) = step a_34 b_34 c_34 d_34 e_34 f_34 g_34 h_34 K35 w35;
    def (a_36, b_36, c_36, d_36, e_36, f_36, g_36, h_36) = step a_35 b_35 c_35 d_35 e_35 f_35 g_35 h_35 K36 w36;
    def (a_37, b_37, c_37, d_37, e_37, f_37, g_37, h_37) = step a_36 b_36 c_36 d_36 e_36 f_36 g_36 h_36 K37 w37;
    def (a_38, b_38, c_38, d_38, e_38, f_38, g_38, h_38) = step a_37 b_37 c_37 d_37 e_37 f_37 g_37 h_37 K38 w38;
    def (a_39, b_39, c_39, d_39, e_39, f_39, g_39, h_39) = step a_38 b_38 c_38 d_38 e_38 f_38 g_38 h_38 K39 w39;
    def (a_40, b_40, c_40, d_40, e_40, f_40, g_40, h_40) = step a_39 b_39 c_39 d_39 e_39 f_39 g_39 h_39 K40 w40;
    def (a_41, b_41, c_41, d_41, e_41, f_41, g_41, h_41) = step a_40 b_40 c_40 d_40 e_40 f_40 g_40 h_40 K41 w41;
    def (a_42, b_42, c_42, d_42, e_42, f_42, g_42, h_42) = step a_41 b_41 c_41 d_41 e_41 f_41 g_41 h_41 K42 w42;
    def (a_43, b_43, c_43, d_43, e_43, f_43, g_43, h_43) = step a_42 b_42 c_42 d_42 e_42 f_42 g_42 h_42 K43 w43;
    def (a_44, b_44, c_44, d_44, e_44, f_44, g_44, h_44) = step a_43 b_43 c_43 d_43 e_43 f_43 g_43 h_43 K44 w44;
    def (a_45, b_45, c_45, d_45, e_45, f_45, g_45, h_45) = step a_44 b_44 c_44 d_44 e_44 f_44 g_44 h_44 K45 w45;
    def (a_46, b_46, c_46, d_46, e_46, f_46, g_46, h_46) = step a_45 b_45 c_45 d_45 e_45 f_45 g_45 h_45 K46 w46;
    def (a_47, b_47, c_47, d_47, e_47, f_47, g_47, h_47) = step a_46 b_46 c_46 d_46 e_46 f_46 g_46 h_46 K47 w47;
    def (a_48, b_48, c_48, d_48, e_48, f_48, g_48, h_48) = step a_47 b_47 c_47 d_47 e_47 f_47 g_47 h_47 K48 w48;
    def (a_49, b_49, c_49, d_49, e_49, f_49, g_49, h_49) = step a_48 b_48 c_48 d_48 e_48 f_48 g_48 h_48 K49 w49;
    def (a_50, b_50, c_50, d_50, e_50, f_50, g_50, h_50) = step a_49 b_49 c_49 d_49 e_49 f_49 g_49 h_49 K50 w50;
    def (a_51, b_51, c_51, d_51, e_51, f_51, g_51, h_51) = step a_50 b_50 c_50 d_50 e_50 f_50 g_50 h_50 K51 w51;
    def (a_52, b_52, c_52, d_52, e_52, f_52, g_52, h_52) = step a_51 b_51 c_51 d_51 e_51 f_51 g_51 h_51 K52 w52;
    def (a_53, b_53, c_53, d_53, e_53, f_53, g_53, h_53) = step a_52 b_52 c_52 d_52 e_52 f_52 g_52 h_52 K53 w53;
    def (a_54, b_54, c_54, d_54, e_54, f_54, g_54, h_54) = step a_53 b_53 c_53 d_53 e_53 f_53 g_53 h_53 K54 w54;
    def (a_55, b_55, c_55, d_55, e_55, f_55, g_55, h_55) = step a_54 b_54 c_54 d_54 e_54 f_54 g_54 h_54 K55 w55;
    def (a_56, b_56, c_56, d_56, e_56, f_56, g_56, h_56) = step a_55 b_55 c_55 d_55 e_55 f_55 g_55 h_55 K56 w56;
    def (a_57, b_57, c_57, d_57, e_57, f_57, g_57, h_57) = step a_56 b_56 c_56 d_56 e_56 f_56 g_56 h_56 K57 w57;
    def (a_58, b_58, c_58, d_58, e_58, f_58, g_58, h_58) = step a_57 b_57 c_57 d_57 e_57 f_57 g_57 h_57 K58 w58;
    def (a_59, b_59, c_59, d_59, e_59, f_59, g_59, h_59) = step a_58 b_58 c_58 d_58 e_58 f_58 g_58 h_58 K59 w59;
    def (a_60, b_60, c_60, d_60, e_60, f_60, g_60, h_60) = step a_59 b_59 c_59 d_59 e_59 f_59 g_59 h_59 K60 w60;
    def (a_61, b_61, c_61, d_61, e_61, f_61, g_61, h_61) = step a_60 b_60 c_60 d_60 e_60 f_60 g_60 h_60 K61 w61;
    def (a_62, b_62, c_62, d_62, e_62, f_62, g_62, h_62) = step a_61 b_61 c_61 d_61 e_61 f_61 g_61 h_61 K62 w62;
    def (a_63, b_63, c_63, d_63, e_63, f_63, g_63, h_63) = step a_62 b_62 c_62 d_62 e_62 f_62 g_62 h_62 K63 w63;
    def H0_f = rem32 ((H0 + a_63));
    def H1_f = rem32 ((H1 + b_63));
    def H2_f = rem32 ((H2 + c_63));
    def H3_f = rem32 ((H3 + d_63));
    def H4_f = rem32 ((H4 + e_63));
    def H5_f = rem32 ((H5 + f_63));
    def H6_f = rem32 ((H6 + g_63));
    def H7_f = rem32 ((H7 + h_63));
    (H0_f, H1_f, H2_f, H3_f, H4_f, H5_f, H6_f, H7_f)
};
def m0 = 0x00000000;
def m1 = 0x00000000;
def m2 = 0x00000000;
def m3 = 0x00000000;
def m4 = 0x00000000;
def m5 = 0x00000000;
def m6 = 0x00000000;
def m7 = 0x00000000;
def m8 = 0x00000000;
def m9 = 0x00000000;
def m10 = 0x00000000;
def m11 = 0x00000000;
def m12 = 0x00000000;
def m13 = 0x00000000;
def m14 = 0x00000000;
def m15 = 0x00000000;
def (H0_0, H1_0, H2_0, H3_0, H4_0, H5_0, H6_0, H7_0) = process_block H0 H1 H2 H3 H4 H5 H6 H7 m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15;
