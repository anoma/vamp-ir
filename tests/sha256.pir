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
    combine32 (fresh((x0+y0+z0)%2), fresh((x1+y1+z1)%2), fresh((x2+y2+z2)%2), fresh((x3+y3+z3)%2), fresh((x4+y4+z4)%2), fresh((x5+y5+z5)%2), fresh((x6+y6+z6)%2), fresh((x7+y7+z7)%2), fresh((x8+y8+z8)%2), fresh((x9+y9+z9)%2), fresh((x10+y10+z10)%2), fresh((x11+y11+z11)%2), fresh((x12+y12+z12)%2), fresh((x13+y13+z13)%2), fresh((x14+y14+z14)%2), fresh((x15+y15+z15)%2), fresh((x16+y16+z16)%2), fresh((x17+y17+z17)%2), fresh((x18+y18+z18)%2), fresh((x19+y19+z19)%2), fresh((x20+y20+z20)%2), fresh((x21+y21+z21)%2), fresh((x22+y22+z22)%2), fresh((x23+y23+z23)%2), fresh((x24+y24+z24)%2), fresh((x25+y25+z25)%2), fresh((x26+y26+z26)%2), fresh((x27+y27+z27)%2), fresh((x28+y28+z28)%2), fresh((x29+y29+z29)%2), fresh((x30+y30+z30)%2), fresh((x31+y31+z31)%2), ())
    };

def sigma1 x = {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range32 x;
    def (x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,xr) = (a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,());
    def (y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17,y18,y19,y20,y21,y22,y23,y24,y25,y26,y27,y28,y29,y30,y31,yr) = (a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,());
    def (z0,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,z11,z12,z13,z14,z15,z16,z17,z18,z19,z20,z21,z22,z23,z24,z25,z26,z27,z28,z29,z30,z31,zr) = (a10, a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,0,0,0,0,0,0,0,0,0,0,());
    combine32 (fresh((x0+y0+z0)%2), fresh((x1+y1+z1)%2), fresh((x2+y2+z2)%2), fresh((x3+y3+z3)%2), fresh((x4+y4+z4)%2), fresh((x5+y5+z5)%2), fresh((x6+y6+z6)%2), fresh((x7+y7+z7)%2), fresh((x8+y8+z8)%2), fresh((x9+y9+z9)%2), fresh((x10+y10+z10)%2), fresh((x11+y11+z11)%2), fresh((x12+y12+z12)%2), fresh((x13+y13+z13)%2), fresh((x14+y14+z14)%2), fresh((x15+y15+z15)%2), fresh((x16+y16+z16)%2), fresh((x17+y17+z17)%2), fresh((x18+y18+z18)%2), fresh((x19+y19+z19)%2), fresh((x20+y20+z20)%2), fresh((x21+y21+z21)%2), fresh((x22+y22+z22)%2), fresh((x23+y23+z23)%2), fresh((x24+y24+z24)%2), fresh((x25+y25+z25)%2), fresh((x26+y26+z26)%2), fresh((x27+y27+z27)%2), fresh((x28+y28+z28)%2), fresh((x29+y29+z29)%2), fresh((x30+y30+z30)%2), fresh((x31+y31+z31)%2), ())
    };

def SIGMA0 x = {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range32 x;
    def (x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,xr) = (a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a0,a1,());
    def (y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17,y18,y19,y20,y21,y22,y23,y24,y25,y26,y27,y28,y29,y30,y31,yr) = (a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,());
    def (z0,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,z11,z12,z13,z14,z15,z16,z17,z18,z19,z20,z21,z22,z23,z24,z25,z26,z27,z28,z29,z30,z31,zr) = (a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,());
    combine32 (fresh((x0+y0+z0)%2), fresh((x1+y1+z1)%2), fresh((x2+y2+z2)%2), fresh((x3+y3+z3)%2), fresh((x4+y4+z4)%2), fresh((x5+y5+z5)%2), fresh((x6+y6+z6)%2), fresh((x7+y7+z7)%2), fresh((x8+y8+z8)%2), fresh((x9+y9+z9)%2), fresh((x10+y10+z10)%2), fresh((x11+y11+z11)%2), fresh((x12+y12+z12)%2), fresh((x13+y13+z13)%2), fresh((x14+y14+z14)%2), fresh((x15+y15+z15)%2), fresh((x16+y16+z16)%2), fresh((x17+y17+z17)%2), fresh((x18+y18+z18)%2), fresh((x19+y19+z19)%2), fresh((x20+y20+z20)%2), fresh((x21+y21+z21)%2), fresh((x22+y22+z22)%2), fresh((x23+y23+z23)%2), fresh((x24+y24+z24)%2), fresh((x25+y25+z25)%2), fresh((x26+y26+z26)%2), fresh((x27+y27+z27)%2), fresh((x28+y28+z28)%2), fresh((x29+y29+z29)%2), fresh((x30+y30+z30)%2), fresh((x31+y31+z31)%2), ())
    };

def SIGMA1 x = {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range32 x;
    def (x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,xr) = (a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a0,a1,a2,a3,a4,a5,());
    def (y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17,y18,y19,y20,y21,y22,y23,y24,y25,y26,y27,y28,y29,y30,y31,yr) = (a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,());
    def (z0,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,z11,z12,z13,z14,z15,z16,z17,z18,z19,z20,z21,z22,z23,z24,z25,z26,z27,z28,z29,z30,z31,zr) = (a25,a26,a27,a28,a29,a30,a31,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,());
    combine32 (fresh((x0+y0+z0)%2), fresh((x1+y1+z1)%2), fresh((x2+y2+z2)%2), fresh((x3+y3+z3)%2), fresh((x4+y4+z4)%2), fresh((x5+y5+z5)%2), fresh((x6+y6+z6)%2), fresh((x7+y7+z7)%2), fresh((x8+y8+z8)%2), fresh((x9+y9+z9)%2), fresh((x10+y10+z10)%2), fresh((x11+y11+z11)%2), fresh((x12+y12+z12)%2), fresh((x13+y13+z13)%2), fresh((x14+y14+z14)%2), fresh((x15+y15+z15)%2), fresh((x16+y16+z16)%2), fresh((x17+y17+z17)%2), fresh((x18+y18+z18)%2), fresh((x19+y19+z19)%2), fresh((x20+y20+z20)%2), fresh((x21+y21+z21)%2), fresh((x22+y22+z22)%2), fresh((x23+y23+z23)%2), fresh((x24+y24+z24)%2), fresh((x25+y25+z25)%2), fresh((x26+y26+z26)%2), fresh((x27+y27+z27)%2), fresh((x28+y28+z28)%2), fresh((x29+y29+z29)%2), fresh((x30+y30+z30)%2), fresh((x31+y31+z31)%2), ())
    };


// Choose function
def ch x y z = {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range32 x;
    def (b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23,b24,b25,b26,b27,b28,b29,b30,b31,br) = range32 y;
    def (c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,c31,cr) = range32 z;
    combine32 (fresh((a0*b0)+(1-a0)*(c0)), fresh((a1*b1)+(1-a1)*(c1)), fresh((a2*b2)+(1-a2)*(c2)), fresh((a3*b3)+(1-a3)*(c3)), fresh((a4*b4)+(1-a4)*(c4)), fresh((a5*b5)+(1-a5)*(c5)), fresh((a6*b6)+(1-a6)*(c6)), fresh((a7*b7)+(1-a7)*(c7)), fresh((a8*b8)+(1-a8)*(c8)), fresh((a9*b9)+(1-a9)*(c9)), fresh((a10*b10)+(1-a10)*(c10)), fresh((a11*b11)+(1-a11)*(c11)), fresh((a12*b12)+(1-a12)*(c12)), fresh((a13*b13)+(1-a13)*(c13)), fresh((a14*b14)+(1-a14)*(c14)), fresh((a15*b15)+(1-a15)*(c15)), fresh((a16*b16)+(1-a16)*(c16)), fresh((a17*b17)+(1-a17)*(c17)), fresh((a18*b18)+(1-a18)*(c18)), fresh((a19*b19)+(1-a19)*(c19)), fresh((a20*b20)+(1-a20)*(c20)), fresh((a21*b21)+(1-a21)*(c21)), fresh((a22*b22)+(1-a22)*(c22)), fresh((a23*b23)+(1-a23)*(c23)), fresh((a24*b24)+(1-a24)*(c24)), fresh((a25*b25)+(1-a25)*(c25)), fresh((a26*b26)+(1-a26)*(c26)), fresh((a27*b27)+(1-a27)*(c27)), fresh((a28*b28)+(1-a28)*(c28)), fresh((a29*b29)+(1-a29)*(c29)), fresh((a30*b30)+(1-a30)*(c30)), fresh((a31*b31)+(1-a31)*(c31)), ())
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

def m0 = 0x61626380;
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
def m15 = 0x00000018;

// PARSING THE MESSAGE
// We need to parse the message into 32 bit words.

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
def w16 = fresh( (sigma1 w14 + w9 + sigma0 w1 + w0) % 4294967296 );
def w17 = fresh( (sigma1 w15 + w10 + sigma0 w2 + w1) % 4294967296 );
def w18 = fresh( (sigma1 w16 + w11 + sigma0 w3 + w2) % 4294967296 );
def w19 = fresh( (sigma1 w17 + w12 + sigma0 w4 + w3) % 4294967296 );
def w20 = fresh( (sigma1 w18 + w13 + sigma0 w5 + w4) % 4294967296 );
def w21 = fresh( (sigma1 w19 + w14 + sigma0 w6 + w5) % 4294967296 );
def w22 = fresh( (sigma1 w20 + w15 + sigma0 w7 + w6) % 4294967296 );
def w23 = fresh( (sigma1 w21 + w16 + sigma0 w8 + w7) % 4294967296 );
def w24 = fresh( (sigma1 w22 + w17 + sigma0 w9 + w8) % 4294967296 );
def w25 = fresh( (sigma1 w23 + w18 + sigma0 w10 + w9) % 4294967296 );
def w26 = fresh( (sigma1 w24 + w19 + sigma0 w11 + w10) % 4294967296 );
def w27 = fresh( (sigma1 w25 + w20 + sigma0 w12 + w11) % 4294967296 );
def w28 = fresh( (sigma1 w26 + w21 + sigma0 w13 + w12) % 4294967296 );
def w29 = fresh( (sigma1 w27 + w22 + sigma0 w14 + w13) % 4294967296 );
def w30 = fresh( (sigma1 w28 + w23 + sigma0 w15 + w14) % 4294967296 );
def w31 = fresh( (sigma1 w29 + w24 + sigma0 w16 + w15) % 4294967296 );
def w32 = fresh( (sigma1 w30 + w25 + sigma0 w17 + w16) % 4294967296 );
def w33 = fresh( (sigma1 w31 + w26 + sigma0 w18 + w17) % 4294967296 );
def w34 = fresh( (sigma1 w32 + w27 + sigma0 w19 + w18) % 4294967296 );
def w35 = fresh( (sigma1 w33 + w28 + sigma0 w20 + w19) % 4294967296 );
def w36 = fresh( (sigma1 w34 + w29 + sigma0 w21 + w20) % 4294967296 );
def w37 = fresh( (sigma1 w35 + w30 + sigma0 w22 + w21) % 4294967296 );
def w38 = fresh( (sigma1 w36 + w31 + sigma0 w23 + w22) % 4294967296 );
def w39 = fresh( (sigma1 w37 + w32 + sigma0 w24 + w23) % 4294967296 );
def w40 = fresh( (sigma1 w38 + w33 + sigma0 w25 + w24) % 4294967296 );
def w41 = fresh( (sigma1 w39 + w34 + sigma0 w26 + w25) % 4294967296 );
def w42 = fresh( (sigma1 w40 + w35 + sigma0 w27 + w26) % 4294967296 );
def w43 = fresh( (sigma1 w41 + w36 + sigma0 w28 + w27) % 4294967296 );
def w44 = fresh( (sigma1 w42 + w37 + sigma0 w29 + w28) % 4294967296 );
def w45 = fresh( (sigma1 w43 + w38 + sigma0 w30 + w29) % 4294967296 );
def w46 = fresh( (sigma1 w44 + w39 + sigma0 w31 + w30) % 4294967296 );
def w47 = fresh( (sigma1 w45 + w40 + sigma0 w32 + w31) % 4294967296 );
def w48 = fresh( (sigma1 w46 + w41 + sigma0 w33 + w32) % 4294967296 );
def w49 = fresh( (sigma1 w47 + w42 + sigma0 w34 + w33) % 4294967296 );
def w50 = fresh( (sigma1 w48 + w43 + sigma0 w35 + w34) % 4294967296 );
def w51 = fresh( (sigma1 w49 + w44 + sigma0 w36 + w35) % 4294967296 );
def w52 = fresh( (sigma1 w50 + w45 + sigma0 w37 + w36) % 4294967296 );
def w53 = fresh( (sigma1 w51 + w46 + sigma0 w38 + w37) % 4294967296 );
def w54 = fresh( (sigma1 w52 + w47 + sigma0 w39 + w38) % 4294967296 );
def w55 = fresh( (sigma1 w53 + w48 + sigma0 w40 + w39) % 4294967296 );
def w56 = fresh( (sigma1 w54 + w49 + sigma0 w41 + w40) % 4294967296 );
def w57 = fresh( (sigma1 w55 + w50 + sigma0 w42 + w41) % 4294967296 );
def w58 = fresh( (sigma1 w56 + w51 + sigma0 w43 + w42) % 4294967296 );
def w59 = fresh( (sigma1 w57 + w52 + sigma0 w44 + w43) % 4294967296 );
def w60 = fresh( (sigma1 w58 + w53 + sigma0 w45 + w44) % 4294967296 );
def w61 = fresh( (sigma1 w59 + w54 + sigma0 w46 + w45) % 4294967296 );
def w62 = fresh( (sigma1 w60 + w55 + sigma0 w47 + w46) % 4294967296 );
def w63 = fresh( (sigma1 w61 + w56 + sigma0 w48 + w47) % 4294967296 );

def H0 = 0x6a09e667;
def H1 = 0xbb67ae85;
def H2 = 0x3c6ef372;
def H3 = 0xa54ff53a;
def H4 = 0x510e527f;
def H5 = 0x9b05688c;
def H6 = 0x1f83d9ab;
def H7 = 0x5be0cd19;

def a = H0;
def b = H1;
def c = H2;
def d = H3;
def e = H4;
def f = H5;
def g = H6;
def h = H7;

def t1_0 = fresh (( h + (SIGMA1 e) + (ch e f g) + K0 + w0) % 4294967296);
def t2_0 = fresh ((SIGMA0 a + (maj a b c)) % 4294967296);
def h_0 = g;
def g_0 = f;
def f_0 = e;
def e_0 = fresh ((d + t1_0) % 4294967296);
def d_0 = c;
def c_0 = b;
def b_0 = a;
def a_0 = fresh ((t1_0 + t2_0) % 4294967296);

def t1_1 = fresh (( h_0 + (SIGMA1 e_0) + (ch e_0 f_0 g_0) + K1 + w1) % 4294967296);
def t2_1 = fresh ((SIGMA0 a_0 + (maj a_0 b_0 c_0)) % 4294967296);
def h_1 = g_0;
def g_1 = f_0;
def f_1 = e_0;
def e_1 = fresh ((d_0 + t1_1) % 4294967296);
def d_1 = c_0;
def c_1 = b_0;
def b_1 = a_0;
def a_1 = fresh ((t1_1 + t2_1) % 4294967296);

def t1_2 = fresh (( h_1 + (SIGMA1 e_1) + (ch e_1 f_1 g_1) + K2 + w2) % 4294967296);
def t2_2 = fresh ((SIGMA0 a_1 + (maj a_1 b_1 c_1)) % 4294967296);
def h_2 = g_1;
def g_2 = f_1;
def f_2 = e_1;
def e_2 = fresh ((d_1 + t1_2) % 4294967296);
def d_2 = c_1;
def c_2 = b_1;
def b_2 = a_1;
def a_2 = fresh ((t1_2 + t2_2) % 4294967296);

def t1_3 = fresh (( h_2 + (SIGMA1 e_2) + (ch e_2 f_2 g_2) + K3 + w3) % 4294967296);
def t2_3 = fresh ((SIGMA0 a_2 + (maj a_2 b_2 c_2)) % 4294967296);
def h_3 = g_2;
def g_3 = f_2;
def f_3 = e_2;
def e_3 = fresh ((d_2 + t1_3) % 4294967296);
def d_3 = c_2;
def c_3 = b_2;
def b_3 = a_2;
def a_3 = fresh ((t1_3 + t2_3) % 4294967296);

def t1_4 = fresh (( h_3 + (SIGMA1 e_3) + (ch e_3 f_3 g_3) + K4 + w4) % 4294967296);
def t2_4 = fresh ((SIGMA0 a_3 + (maj a_3 b_3 c_3)) % 4294967296);
def h_4 = g_3;
def g_4 = f_3;
def f_4 = e_3;
def e_4 = fresh ((d_3 + t1_4) % 4294967296);
def d_4 = c_3;
def c_4 = b_3;
def b_4 = a_3;
def a_4 = fresh ((t1_4 + t2_4) % 4294967296);

def t1_5 = fresh (( h_4 + (SIGMA1 e_4) + (ch e_4 f_4 g_4) + K5 + w5) % 4294967296);
def t2_5 = fresh ((SIGMA0 a_4 + (maj a_4 b_4 c_4)) % 4294967296);
def h_5 = g_4;
def g_5 = f_4;
def f_5 = e_4;
def e_5 = fresh ((d_4 + t1_5) % 4294967296);
def d_5 = c_4;
def c_5 = b_4;
def b_5 = a_4;
def a_5 = fresh ((t1_5 + t2_5) % 4294967296);

def t1_6 = fresh (( h_5 + (SIGMA1 e_5) + (ch e_5 f_5 g_5) + K6 + w6) % 4294967296);
def t2_6 = fresh ((SIGMA0 a_5 + (maj a_5 b_5 c_5)) % 4294967296);
def h_6 = g_5;
def g_6 = f_5;
def f_6 = e_5;
def e_6 = fresh ((d_5 + t1_6) % 4294967296);
def d_6 = c_5;
def c_6 = b_5;
def b_6 = a_5;
def a_6 = fresh ((t1_6 + t2_6) % 4294967296);

def t1_7 = fresh (( h_6 + (SIGMA1 e_6) + (ch e_6 f_6 g_6) + K7 + w7) % 4294967296);
def t2_7 = fresh ((SIGMA0 a_6 + (maj a_6 b_6 c_6)) % 4294967296);
def h_7 = g_6;
def g_7 = f_6;
def f_7 = e_6;
def e_7 = fresh ((d_6 + t1_7) % 4294967296);
def d_7 = c_6;
def c_7 = b_6;
def b_7 = a_6;
def a_7 = fresh ((t1_7 + t2_7) % 4294967296);

def t1_8 = fresh (( h_7 + (SIGMA1 e_7) + (ch e_7 f_7 g_7) + K8 + w8) % 4294967296);
def t2_8 = fresh ((SIGMA0 a_7 + (maj a_7 b_7 c_7)) % 4294967296);
def h_8 = g_7;
def g_8 = f_7;
def f_8 = e_7;
def e_8 = fresh ((d_7 + t1_8) % 4294967296);
def d_8 = c_7;
def c_8 = b_7;
def b_8 = a_7;
def a_8 = fresh ((t1_8 + t2_8) % 4294967296);

def t1_9 = fresh (( h_8 + (SIGMA1 e_8) + (ch e_8 f_8 g_8) + K9 + w9) % 4294967296);
def t2_9 = fresh ((SIGMA0 a_8 + (maj a_8 b_8 c_8)) % 4294967296);
def h_9 = g_8;
def g_9 = f_8;
def f_9 = e_8;
def e_9 = fresh ((d_8 + t1_9) % 4294967296);
def d_9 = c_8;
def c_9 = b_8;
def b_9 = a_8;
def a_9 = fresh ((t1_9 + t2_9) % 4294967296);

def t1_10 = fresh (( h_9 + (SIGMA1 e_9) + (ch e_9 f_9 g_9) + K10 + w10) % 4294967296);
def t2_10 = fresh ((SIGMA0 a_9 + (maj a_9 b_9 c_9)) % 4294967296);
def h_10 = g_9;
def g_10 = f_9;
def f_10 = e_9;
def e_10 = fresh ((d_9 + t1_10) % 4294967296);
def d_10 = c_9;
def c_10 = b_9;
def b_10 = a_9;
def a_10 = fresh ((t1_10 + t2_10) % 4294967296);

def t1_11 = fresh (( h_10 + (SIGMA1 e_10) + (ch e_10 f_10 g_10) + K11 + w11) % 4294967296);
def t2_11 = fresh ((SIGMA0 a_10 + (maj a_10 b_10 c_10)) % 4294967296);
def h_11 = g_10;
def g_11 = f_10;
def f_11 = e_10;
def e_11 = fresh ((d_10 + t1_11) % 4294967296);
def d_11 = c_10;
def c_11 = b_10;
def b_11 = a_10;
def a_11 = fresh ((t1_11 + t2_11) % 4294967296);

def t1_12 = fresh (( h_11 + (SIGMA1 e_11) + (ch e_11 f_11 g_11) + K12 + w12) % 4294967296);
def t2_12 = fresh ((SIGMA0 a_11 + (maj a_11 b_11 c_11)) % 4294967296);
def h_12 = g_11;
def g_12 = f_11;
def f_12 = e_11;
def e_12 = fresh ((d_11 + t1_12) % 4294967296);
def d_12 = c_11;
def c_12 = b_11;
def b_12 = a_11;
def a_12 = fresh ((t1_12 + t2_12) % 4294967296);

def t1_13 = fresh (( h_12 + (SIGMA1 e_12) + (ch e_12 f_12 g_12) + K13 + w13) % 4294967296);
def t2_13 = fresh ((SIGMA0 a_12 + (maj a_12 b_12 c_12)) % 4294967296);
def h_13 = g_12;
def g_13 = f_12;
def f_13 = e_12;
def e_13 = fresh ((d_12 + t1_13) % 4294967296);
def d_13 = c_12;
def c_13 = b_12;
def b_13 = a_12;
def a_13 = fresh ((t1_13 + t2_13) % 4294967296);

def t1_14 = fresh (( h_13 + (SIGMA1 e_13) + (ch e_13 f_13 g_13) + K14 + w14) % 4294967296);
def t2_14 = fresh ((SIGMA0 a_13 + (maj a_13 b_13 c_13)) % 4294967296);
def h_14 = g_13;
def g_14 = f_13;
def f_14 = e_13;
def e_14 = fresh ((d_13 + t1_14) % 4294967296);
def d_14 = c_13;
def c_14 = b_13;
def b_14 = a_13;
def a_14 = fresh ((t1_14 + t2_14) % 4294967296);

def t1_15 = fresh (( h_14 + (SIGMA1 e_14) + (ch e_14 f_14 g_14) + K15 + w15) % 4294967296);
def t2_15 = fresh ((SIGMA0 a_14 + (maj a_14 b_14 c_14)) % 4294967296);
def h_15 = g_14;
def g_15 = f_14;
def f_15 = e_14;
def e_15 = fresh ((d_14 + t1_15) % 4294967296);
def d_15 = c_14;
def c_15 = b_14;
def b_15 = a_14;
def a_15 = fresh ((t1_15 + t2_15) % 4294967296);

def t1_16 = fresh (( h_15 + (SIGMA1 e_15) + (ch e_15 f_15 g_15) + K16 + w16) % 4294967296);
def t2_16 = fresh ((SIGMA0 a_15 + (maj a_15 b_15 c_15)) % 4294967296);
def h_16 = g_15;
def g_16 = f_15;
def f_16 = e_15;
def e_16 = fresh ((d_15 + t1_16) % 4294967296);
def d_16 = c_15;
def c_16 = b_15;
def b_16 = a_15;
def a_16 = fresh ((t1_16 + t2_16) % 4294967296);

def t1_17 = fresh (( h_16 + (SIGMA1 e_16) + (ch e_16 f_16 g_16) + K17 + w17) % 4294967296);
def t2_17 = fresh ((SIGMA0 a_16 + (maj a_16 b_16 c_16)) % 4294967296);
def h_17 = g_16;
def g_17 = f_16;
def f_17 = e_16;
def e_17 = fresh ((d_16 + t1_17) % 4294967296);
def d_17 = c_16;
def c_17 = b_16;
def b_17 = a_16;
def a_17 = fresh ((t1_17 + t2_17) % 4294967296);

def t1_18 = fresh (( h_17 + (SIGMA1 e_17) + (ch e_17 f_17 g_17) + K18 + w18) % 4294967296);
def t2_18 = fresh ((SIGMA0 a_17 + (maj a_17 b_17 c_17)) % 4294967296);
def h_18 = g_17;
def g_18 = f_17;
def f_18 = e_17;
def e_18 = fresh ((d_17 + t1_18) % 4294967296);
def d_18 = c_17;
def c_18 = b_17;
def b_18 = a_17;
def a_18 = fresh ((t1_18 + t2_18) % 4294967296);

def t1_19 = fresh (( h_18 + (SIGMA1 e_18) + (ch e_18 f_18 g_18) + K19 + w19) % 4294967296);
def t2_19 = fresh ((SIGMA0 a_18 + (maj a_18 b_18 c_18)) % 4294967296);
def h_19 = g_18;
def g_19 = f_18;
def f_19 = e_18;
def e_19 = fresh ((d_18 + t1_19) % 4294967296);
def d_19 = c_18;
def c_19 = b_18;
def b_19 = a_18;
def a_19 = fresh ((t1_19 + t2_19) % 4294967296);

def t1_20 = fresh (( h_19 + (SIGMA1 e_19) + (ch e_19 f_19 g_19) + K20 + w20) % 4294967296);
def t2_20 = fresh ((SIGMA0 a_19 + (maj a_19 b_19 c_19)) % 4294967296);
def h_20 = g_19;
def g_20 = f_19;
def f_20 = e_19;
def e_20 = fresh ((d_19 + t1_20) % 4294967296);
def d_20 = c_19;
def c_20 = b_19;
def b_20 = a_19;
def a_20 = fresh ((t1_20 + t2_20) % 4294967296);

def t1_21 = fresh (( h_20 + (SIGMA1 e_20) + (ch e_20 f_20 g_20) + K21 + w21) % 4294967296);
def t2_21 = fresh ((SIGMA0 a_20 + (maj a_20 b_20 c_20)) % 4294967296);
def h_21 = g_20;
def g_21 = f_20;
def f_21 = e_20;
def e_21 = fresh ((d_20 + t1_21) % 4294967296);
def d_21 = c_20;
def c_21 = b_20;
def b_21 = a_20;
def a_21 = fresh ((t1_21 + t2_21) % 4294967296);

def t1_22 = fresh (( h_21 + (SIGMA1 e_21) + (ch e_21 f_21 g_21) + K22 + w22) % 4294967296);
def t2_22 = fresh ((SIGMA0 a_21 + (maj a_21 b_21 c_21)) % 4294967296);
def h_22 = g_21;
def g_22 = f_21;
def f_22 = e_21;
def e_22 = fresh ((d_21 + t1_22) % 4294967296);
def d_22 = c_21;
def c_22 = b_21;
def b_22 = a_21;
def a_22 = fresh ((t1_22 + t2_22) % 4294967296);

def t1_23 = fresh (( h_22 + (SIGMA1 e_22) + (ch e_22 f_22 g_22) + K23 + w23) % 4294967296);
def t2_23 = fresh ((SIGMA0 a_22 + (maj a_22 b_22 c_22)) % 4294967296);
def h_23 = g_22;
def g_23 = f_22;
def f_23 = e_22;
def e_23 = fresh ((d_22 + t1_23) % 4294967296);
def d_23 = c_22;
def c_23 = b_22;
def b_23 = a_22;
def a_23 = fresh ((t1_23 + t2_23) % 4294967296);

def t1_24 = fresh (( h_23 + (SIGMA1 e_23) + (ch e_23 f_23 g_23) + K24 + w24) % 4294967296);
def t2_24 = fresh ((SIGMA0 a_23 + (maj a_23 b_23 c_23)) % 4294967296);
def h_24 = g_23;
def g_24 = f_23;
def f_24 = e_23;
def e_24 = fresh ((d_23 + t1_24) % 4294967296);
def d_24 = c_23;
def c_24 = b_23;
def b_24 = a_23;
def a_24 = fresh ((t1_24 + t2_24) % 4294967296);

def t1_25 = fresh (( h_24 + (SIGMA1 e_24) + (ch e_24 f_24 g_24) + K25 + w25) % 4294967296);
def t2_25 = fresh ((SIGMA0 a_24 + (maj a_24 b_24 c_24)) % 4294967296);
def h_25 = g_24;
def g_25 = f_24;
def f_25 = e_24;
def e_25 = fresh ((d_24 + t1_25) % 4294967296);
def d_25 = c_24;
def c_25 = b_24;
def b_25 = a_24;
def a_25 = fresh ((t1_25 + t2_25) % 4294967296);

def t1_26 = fresh (( h_25 + (SIGMA1 e_25) + (ch e_25 f_25 g_25) + K26 + w26) % 4294967296);
def t2_26 = fresh ((SIGMA0 a_25 + (maj a_25 b_25 c_25)) % 4294967296);
def h_26 = g_25;
def g_26 = f_25;
def f_26 = e_25;
def e_26 = fresh ((d_25 + t1_26) % 4294967296);
def d_26 = c_25;
def c_26 = b_25;
def b_26 = a_25;
def a_26 = fresh ((t1_26 + t2_26) % 4294967296);

def t1_27 = fresh (( h_26 + (SIGMA1 e_26) + (ch e_26 f_26 g_26) + K27 + w27) % 4294967296);
def t2_27 = fresh ((SIGMA0 a_26 + (maj a_26 b_26 c_26)) % 4294967296);
def h_27 = g_26;
def g_27 = f_26;
def f_27 = e_26;
def e_27 = fresh ((d_26 + t1_27) % 4294967296);
def d_27 = c_26;
def c_27 = b_26;
def b_27 = a_26;
def a_27 = fresh ((t1_27 + t2_27) % 4294967296);

def t1_28 = fresh (( h_27 + (SIGMA1 e_27) + (ch e_27 f_27 g_27) + K28 + w28) % 4294967296);
def t2_28 = fresh ((SIGMA0 a_27 + (maj a_27 b_27 c_27)) % 4294967296);
def h_28 = g_27;
def g_28 = f_27;
def f_28 = e_27;
def e_28 = fresh ((d_27 + t1_28) % 4294967296);
def d_28 = c_27;
def c_28 = b_27;
def b_28 = a_27;
def a_28 = fresh ((t1_28 + t2_28) % 4294967296);

def t1_29 = fresh (( h_28 + (SIGMA1 e_28) + (ch e_28 f_28 g_28) + K29 + w29) % 4294967296);
def t2_29 = fresh ((SIGMA0 a_28 + (maj a_28 b_28 c_28)) % 4294967296);
def h_29 = g_28;
def g_29 = f_28;
def f_29 = e_28;
def e_29 = fresh ((d_28 + t1_29) % 4294967296);
def d_29 = c_28;
def c_29 = b_28;
def b_29 = a_28;
def a_29 = fresh ((t1_29 + t2_29) % 4294967296);

def t1_30 = fresh (( h_29 + (SIGMA1 e_29) + (ch e_29 f_29 g_29) + K30 + w30) % 4294967296);
def t2_30 = fresh ((SIGMA0 a_29 + (maj a_29 b_29 c_29)) % 4294967296);
def h_30 = g_29;
def g_30 = f_29;
def f_30 = e_29;
def e_30 = fresh ((d_29 + t1_30) % 4294967296);
def d_30 = c_29;
def c_30 = b_29;
def b_30 = a_29;
def a_30 = fresh ((t1_30 + t2_30) % 4294967296);

def t1_31 = fresh (( h_30 + (SIGMA1 e_30) + (ch e_30 f_30 g_30) + K31 + w31) % 4294967296);
def t2_31 = fresh ((SIGMA0 a_30 + (maj a_30 b_30 c_30)) % 4294967296);
def h_31 = g_30;
def g_31 = f_30;
def f_31 = e_30;
def e_31 = fresh ((d_30 + t1_31) % 4294967296);
def d_31 = c_30;
def c_31 = b_30;
def b_31 = a_30;
def a_31 = fresh ((t1_31 + t2_31) % 4294967296);

def t1_32 = fresh (( h_31 + (SIGMA1 e_31) + (ch e_31 f_31 g_31) + K32 + w32) % 4294967296);
def t2_32 = fresh ((SIGMA0 a_31 + (maj a_31 b_31 c_31)) % 4294967296);
def h_32 = g_31;
def g_32 = f_31;
def f_32 = e_31;
def e_32 = fresh ((d_31 + t1_32) % 4294967296);
def d_32 = c_31;
def c_32 = b_31;
def b_32 = a_31;
def a_32 = fresh ((t1_32 + t2_32) % 4294967296);

def t1_33 = fresh (( h_32 + (SIGMA1 e_32) + (ch e_32 f_32 g_32) + K33 + w33) % 4294967296);
def t2_33 = fresh ((SIGMA0 a_32 + (maj a_32 b_32 c_32)) % 4294967296);
def h_33 = g_32;
def g_33 = f_32;
def f_33 = e_32;
def e_33 = fresh ((d_32 + t1_33) % 4294967296);
def d_33 = c_32;
def c_33 = b_32;
def b_33 = a_32;
def a_33 = fresh ((t1_33 + t2_33) % 4294967296);

def t1_34 = fresh (( h_33 + (SIGMA1 e_33) + (ch e_33 f_33 g_33) + K34 + w34) % 4294967296);
def t2_34 = fresh ((SIGMA0 a_33 + (maj a_33 b_33 c_33)) % 4294967296);
def h_34 = g_33;
def g_34 = f_33;
def f_34 = e_33;
def e_34 = fresh ((d_33 + t1_34) % 4294967296);
def d_34 = c_33;
def c_34 = b_33;
def b_34 = a_33;
def a_34 = fresh ((t1_34 + t2_34) % 4294967296);

def t1_35 = fresh (( h_34 + (SIGMA1 e_34) + (ch e_34 f_34 g_34) + K35 + w35) % 4294967296);
def t2_35 = fresh ((SIGMA0 a_34 + (maj a_34 b_34 c_34)) % 4294967296);
def h_35 = g_34;
def g_35 = f_34;
def f_35 = e_34;
def e_35 = fresh ((d_34 + t1_35) % 4294967296);
def d_35 = c_34;
def c_35 = b_34;
def b_35 = a_34;
def a_35 = fresh ((t1_35 + t2_35) % 4294967296);

def t1_36 = fresh (( h_35 + (SIGMA1 e_35) + (ch e_35 f_35 g_35) + K36 + w36) % 4294967296);
def t2_36 = fresh ((SIGMA0 a_35 + (maj a_35 b_35 c_35)) % 4294967296);
def h_36 = g_35;
def g_36 = f_35;
def f_36 = e_35;
def e_36 = fresh ((d_35 + t1_36) % 4294967296);
def d_36 = c_35;
def c_36 = b_35;
def b_36 = a_35;
def a_36 = fresh ((t1_36 + t2_36) % 4294967296);

def t1_37 = fresh (( h_36 + (SIGMA1 e_36) + (ch e_36 f_36 g_36) + K37 + w37) % 4294967296);
def t2_37 = fresh ((SIGMA0 a_36 + (maj a_36 b_36 c_36)) % 4294967296);
def h_37 = g_36;
def g_37 = f_36;
def f_37 = e_36;
def e_37 = fresh ((d_36 + t1_37) % 4294967296);
def d_37 = c_36;
def c_37 = b_36;
def b_37 = a_36;
def a_37 = fresh ((t1_37 + t2_37) % 4294967296);

def t1_38 = fresh (( h_37 + (SIGMA1 e_37) + (ch e_37 f_37 g_37) + K38 + w38) % 4294967296);
def t2_38 = fresh ((SIGMA0 a_37 + (maj a_37 b_37 c_37)) % 4294967296);
def h_38 = g_37;
def g_38 = f_37;
def f_38 = e_37;
def e_38 = fresh ((d_37 + t1_38) % 4294967296);
def d_38 = c_37;
def c_38 = b_37;
def b_38 = a_37;
def a_38 = fresh ((t1_38 + t2_38) % 4294967296);

def t1_39 = fresh (( h_38 + (SIGMA1 e_38) + (ch e_38 f_38 g_38) + K39 + w39) % 4294967296);
def t2_39 = fresh ((SIGMA0 a_38 + (maj a_38 b_38 c_38)) % 4294967296);
def h_39 = g_38;
def g_39 = f_38;
def f_39 = e_38;
def e_39 = fresh ((d_38 + t1_39) % 4294967296);
def d_39 = c_38;
def c_39 = b_38;
def b_39 = a_38;
def a_39 = fresh ((t1_39 + t2_39) % 4294967296);

def t1_40 = fresh (( h_39 + (SIGMA1 e_39) + (ch e_39 f_39 g_39) + K40 + w40) % 4294967296);
def t2_40 = fresh ((SIGMA0 a_39 + (maj a_39 b_39 c_39)) % 4294967296);
def h_40 = g_39;
def g_40 = f_39;
def f_40 = e_39;
def e_40 = fresh ((d_39 + t1_40) % 4294967296);
def d_40 = c_39;
def c_40 = b_39;
def b_40 = a_39;
def a_40 = fresh ((t1_40 + t2_40) % 4294967296);

def t1_41 = fresh (( h_40 + (SIGMA1 e_40) + (ch e_40 f_40 g_40) + K41 + w41) % 4294967296);
def t2_41 = fresh ((SIGMA0 a_40 + (maj a_40 b_40 c_40)) % 4294967296);
def h_41 = g_40;
def g_41 = f_40;
def f_41 = e_40;
def e_41 = fresh ((d_40 + t1_41) % 4294967296);
def d_41 = c_40;
def c_41 = b_40;
def b_41 = a_40;
def a_41 = fresh ((t1_41 + t2_41) % 4294967296);

def t1_42 = fresh (( h_41 + (SIGMA1 e_41) + (ch e_41 f_41 g_41) + K42 + w42) % 4294967296);
def t2_42 = fresh ((SIGMA0 a_41 + (maj a_41 b_41 c_41)) % 4294967296);
def h_42 = g_41;
def g_42 = f_41;
def f_42 = e_41;
def e_42 = fresh ((d_41 + t1_42) % 4294967296);
def d_42 = c_41;
def c_42 = b_41;
def b_42 = a_41;
def a_42 = fresh ((t1_42 + t2_42) % 4294967296);

def t1_43 = fresh (( h_42 + (SIGMA1 e_42) + (ch e_42 f_42 g_42) + K43 + w43) % 4294967296);
def t2_43 = fresh ((SIGMA0 a_42 + (maj a_42 b_42 c_42)) % 4294967296);
def h_43 = g_42;
def g_43 = f_42;
def f_43 = e_42;
def e_43 = fresh ((d_42 + t1_43) % 4294967296);
def d_43 = c_42;
def c_43 = b_42;
def b_43 = a_42;
def a_43 = fresh ((t1_43 + t2_43) % 4294967296);

def t1_44 = fresh (( h_43 + (SIGMA1 e_43) + (ch e_43 f_43 g_43) + K44 + w44) % 4294967296);
def t2_44 = fresh ((SIGMA0 a_43 + (maj a_43 b_43 c_43)) % 4294967296);
def h_44 = g_43;
def g_44 = f_43;
def f_44 = e_43;
def e_44 = fresh ((d_43 + t1_44) % 4294967296);
def d_44 = c_43;
def c_44 = b_43;
def b_44 = a_43;
def a_44 = fresh ((t1_44 + t2_44) % 4294967296);

def t1_45 = fresh (( h_44 + (SIGMA1 e_44) + (ch e_44 f_44 g_44) + K45 + w45) % 4294967296);
def t2_45 = fresh ((SIGMA0 a_44 + (maj a_44 b_44 c_44)) % 4294967296);
def h_45 = g_44;
def g_45 = f_44;
def f_45 = e_44;
def e_45 = fresh ((d_44 + t1_45) % 4294967296);
def d_45 = c_44;
def c_45 = b_44;
def b_45 = a_44;
def a_45 = fresh ((t1_45 + t2_45) % 4294967296);

def t1_46 = fresh (( h_45 + (SIGMA1 e_45) + (ch e_45 f_45 g_45) + K46 + w46) % 4294967296);
def t2_46 = fresh ((SIGMA0 a_45 + (maj a_45 b_45 c_45)) % 4294967296);
def h_46 = g_45;
def g_46 = f_45;
def f_46 = e_45;
def e_46 = fresh ((d_45 + t1_46) % 4294967296);
def d_46 = c_45;
def c_46 = b_45;
def b_46 = a_45;
def a_46 = fresh ((t1_46 + t2_46) % 4294967296);

def t1_47 = fresh (( h_46 + (SIGMA1 e_46) + (ch e_46 f_46 g_46) + K47 + w47) % 4294967296);
def t2_47 = fresh ((SIGMA0 a_46 + (maj a_46 b_46 c_46)) % 4294967296);
def h_47 = g_46;
def g_47 = f_46;
def f_47 = e_46;
def e_47 = fresh ((d_46 + t1_47) % 4294967296);
def d_47 = c_46;
def c_47 = b_46;
def b_47 = a_46;
def a_47 = fresh ((t1_47 + t2_47) % 4294967296);

def t1_48 = fresh (( h_47 + (SIGMA1 e_47) + (ch e_47 f_47 g_47) + K48 + w48) % 4294967296);
def t2_48 = fresh ((SIGMA0 a_47 + (maj a_47 b_47 c_47)) % 4294967296);
def h_48 = g_47;
def g_48 = f_47;
def f_48 = e_47;
def e_48 = fresh ((d_47 + t1_48) % 4294967296);
def d_48 = c_47;
def c_48 = b_47;
def b_48 = a_47;
def a_48 = fresh ((t1_48 + t2_48) % 4294967296);

def t1_49 = fresh (( h_48 + (SIGMA1 e_48) + (ch e_48 f_48 g_48) + K49 + w49) % 4294967296);
def t2_49 = fresh ((SIGMA0 a_48 + (maj a_48 b_48 c_48)) % 4294967296);
def h_49 = g_48;
def g_49 = f_48;
def f_49 = e_48;
def e_49 = fresh ((d_48 + t1_49) % 4294967296);
def d_49 = c_48;
def c_49 = b_48;
def b_49 = a_48;
def a_49 = fresh ((t1_49 + t2_49) % 4294967296);

def t1_50 = fresh (( h_49 + (SIGMA1 e_49) + (ch e_49 f_49 g_49) + K50 + w50) % 4294967296);
def t2_50 = fresh ((SIGMA0 a_49 + (maj a_49 b_49 c_49)) % 4294967296);
def h_50 = g_49;
def g_50 = f_49;
def f_50 = e_49;
def e_50 = fresh ((d_49 + t1_50) % 4294967296);
def d_50 = c_49;
def c_50 = b_49;
def b_50 = a_49;
def a_50 = fresh ((t1_50 + t2_50) % 4294967296);

def t1_51 = fresh (( h_50 + (SIGMA1 e_50) + (ch e_50 f_50 g_50) + K51 + w51) % 4294967296);
def t2_51 = fresh ((SIGMA0 a_50 + (maj a_50 b_50 c_50)) % 4294967296);
def h_51 = g_50;
def g_51 = f_50;
def f_51 = e_50;
def e_51 = fresh ((d_50 + t1_51) % 4294967296);
def d_51 = c_50;
def c_51 = b_50;
def b_51 = a_50;
def a_51 = fresh ((t1_51 + t2_51) % 4294967296);

def t1_52 = fresh (( h_51 + (SIGMA1 e_51) + (ch e_51 f_51 g_51) + K52 + w52) % 4294967296);
def t2_52 = fresh ((SIGMA0 a_51 + (maj a_51 b_51 c_51)) % 4294967296);
def h_52 = g_51;
def g_52 = f_51;
def f_52 = e_51;
def e_52 = fresh ((d_51 + t1_52) % 4294967296);
def d_52 = c_51;
def c_52 = b_51;
def b_52 = a_51;
def a_52 = fresh ((t1_52 + t2_52) % 4294967296);

def t1_53 = fresh (( h_52 + (SIGMA1 e_52) + (ch e_52 f_52 g_52) + K53 + w53) % 4294967296);
def t2_53 = fresh ((SIGMA0 a_52 + (maj a_52 b_52 c_52)) % 4294967296);
def h_53 = g_52;
def g_53 = f_52;
def f_53 = e_52;
def e_53 = fresh ((d_52 + t1_53) % 4294967296);
def d_53 = c_52;
def c_53 = b_52;
def b_53 = a_52;
def a_53 = fresh ((t1_53 + t2_53) % 4294967296);

def t1_54 = fresh (( h_53 + (SIGMA1 e_53) + (ch e_53 f_53 g_53) + K54 + w54) % 4294967296);
def t2_54 = fresh ((SIGMA0 a_53 + (maj a_53 b_53 c_53)) % 4294967296);
def h_54 = g_53;
def g_54 = f_53;
def f_54 = e_53;
def e_54 = fresh ((d_53 + t1_54) % 4294967296);
def d_54 = c_53;
def c_54 = b_53;
def b_54 = a_53;
def a_54 = fresh ((t1_54 + t2_54) % 4294967296);

def t1_55 = fresh (( h_54 + (SIGMA1 e_54) + (ch e_54 f_54 g_54) + K55 + w55) % 4294967296);
def t2_55 = fresh ((SIGMA0 a_54 + (maj a_54 b_54 c_54)) % 4294967296);
def h_55 = g_54;
def g_55 = f_54;
def f_55 = e_54;
def e_55 = fresh ((d_54 + t1_55) % 4294967296);
def d_55 = c_54;
def c_55 = b_54;
def b_55 = a_54;
def a_55 = fresh ((t1_55 + t2_55) % 4294967296);

def t1_56 = fresh (( h_55 + (SIGMA1 e_55) + (ch e_55 f_55 g_55) + K56 + w56) % 4294967296);
def t2_56 = fresh ((SIGMA0 a_55 + (maj a_55 b_55 c_55)) % 4294967296);
def h_56 = g_55;
def g_56 = f_55;
def f_56 = e_55;
def e_56 = fresh ((d_55 + t1_56) % 4294967296);
def d_56 = c_55;
def c_56 = b_55;
def b_56 = a_55;
def a_56 = fresh ((t1_56 + t2_56) % 4294967296);

def t1_57 = fresh (( h_56 + (SIGMA1 e_56) + (ch e_56 f_56 g_56) + K57 + w57) % 4294967296);
def t2_57 = fresh ((SIGMA0 a_56 + (maj a_56 b_56 c_56)) % 4294967296);
def h_57 = g_56;
def g_57 = f_56;
def f_57 = e_56;
def e_57 = fresh ((d_56 + t1_57) % 4294967296);
def d_57 = c_56;
def c_57 = b_56;
def b_57 = a_56;
def a_57 = fresh ((t1_57 + t2_57) % 4294967296);

def t1_58 = fresh (( h_57 + (SIGMA1 e_57) + (ch e_57 f_57 g_57) + K58 + w58) % 4294967296);
def t2_58 = fresh ((SIGMA0 a_57 + (maj a_57 b_57 c_57)) % 4294967296);
def h_58 = g_57;
def g_58 = f_57;
def f_58 = e_57;
def e_58 = fresh ((d_57 + t1_58) % 4294967296);
def d_58 = c_57;
def c_58 = b_57;
def b_58 = a_57;
def a_58 = fresh ((t1_58 + t2_58) % 4294967296);

def t1_59 = fresh (( h_58 + (SIGMA1 e_58) + (ch e_58 f_58 g_58) + K59 + w59) % 4294967296);
def t2_59 = fresh ((SIGMA0 a_58 + (maj a_58 b_58 c_58)) % 4294967296);
def h_59 = g_58;
def g_59 = f_58;
def f_59 = e_58;
def e_59 = fresh ((d_58 + t1_59) % 4294967296);
def d_59 = c_58;
def c_59 = b_58;
def b_59 = a_58;
def a_59 = fresh ((t1_59 + t2_59) % 4294967296);

def t1_60 = fresh (( h_59 + (SIGMA1 e_59) + (ch e_59 f_59 g_59) + K60 + w60) % 4294967296);
def t2_60 = fresh ((SIGMA0 a_59 + (maj a_59 b_59 c_59)) % 4294967296);
def h_60 = g_59;
def g_60 = f_59;
def f_60 = e_59;
def e_60 = fresh ((d_59 + t1_60) % 4294967296);
def d_60 = c_59;
def c_60 = b_59;
def b_60 = a_59;
def a_60 = fresh ((t1_60 + t2_60) % 4294967296);

def t1_61 = fresh (( h_60 + (SIGMA1 e_60) + (ch e_60 f_60 g_60) + K61 + w61) % 4294967296);
def t2_61 = fresh ((SIGMA0 a_60 + (maj a_60 b_60 c_60)) % 4294967296);
def h_61 = g_60;
def g_61 = f_60;
def f_61 = e_60;
def e_61 = fresh ((d_60 + t1_61) % 4294967296);
def d_61 = c_60;
def c_61 = b_60;
def b_61 = a_60;
def a_61 = fresh ((t1_61 + t2_61) % 4294967296);

def t1_62 = fresh (( h_61 + (SIGMA1 e_61) + (ch e_61 f_61 g_61) + K62 + w62) % 4294967296);
def t2_62 = fresh ((SIGMA0 a_61 + (maj a_61 b_61 c_61)) % 4294967296);
def h_62 = g_61;
def g_62 = f_61;
def f_62 = e_61;
def e_62 = fresh ((d_61 + t1_62) % 4294967296);
def d_62 = c_61;
def c_62 = b_61;
def b_62 = a_61;
def a_62 = fresh ((t1_62 + t2_62) % 4294967296);

def t1_63 = fresh (( h_62 + (SIGMA1 e_62) + (ch e_62 f_62 g_62) + K63 + w63) % 4294967296);
def t2_63 = fresh ((SIGMA0 a_62 + (maj a_62 b_62 c_62)) % 4294967296);
def h_63 = g_62;
def g_63 = f_62;
def f_63 = e_62;
def e_63 = fresh ((d_62 + t1_63) % 4294967296);
def d_63 = c_62;
def c_63 = b_62;
def b_63 = a_62;
def a_63 = fresh ((t1_63 + t2_63) % 4294967296);

// Final step
def H0_f = fresh ((H0 + a_63) % 4294967296);
def H1_f = fresh ((H1 + b_63) % 4294967296);
def H2_f = fresh ((H2 + c_63) % 4294967296);
def H3_f = fresh ((H3 + d_63) % 4294967296);
def H4_f = fresh ((H4 + e_63) % 4294967296);
def H5_f = fresh ((H5 + f_63) % 4294967296);
def H6_f = fresh ((H6 + g_63) % 4294967296);
def H7_f = fresh ((H7 + h_63) % 4294967296);

(0xba7816bf, 0x8f01cfea, 0x414140de, 0x5dae2223, 0xb00361a3, 0x96177a9c, 0xb410ff61, 0xf20015ad) = (H0_f, H1_f, H2_f, H3_f, H4_f, H5_f, H6_f, H7_f);