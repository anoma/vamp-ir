// nothing function: returns a value
def nothing x {
    x + 2^3
};
15 = nothing 7;

def sum x y x{
    x + y + z
}

// combine32. combines 32 bits + a null type (ar) into and int.
def combine32 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) {
    a0 + 2*a1 + 2^2*a2 + 2^3*a3 + 2^4*a4 + 2^5*a5 + 2^6*a6 + 2^7*a7 + 2^8*a8 + 2^9*a9 + 2^10*a10 + 2^11*a11 + 2^12*a12 + 2^13*a13 + 2^14*a14 + 2^15*a15 + 2^16*a16 + 2^17*a17 + 2^18*a18 + 2^19*a19 + 2^20*a20 + 2^21*a21 + 2^22*a22 + 2^23*a23 + 2^24*a24 + 2^25*a25 + 2^26*a26 + 2^27*a27 + 2^28*a28 + 2^29*a29 + 2^30*a30 + 2^31*a31
};

// range 32 returns the bitwise decomposition of an in in 32 bits. a0 is going to be the least significant bit.
// example: 5435434 = 00000000010100101111000000101010 --> (a31, a30, ..., a0)
def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar)  = range 32 5435434;
a31 = 0;
a30 = 0;
a29 = 0;
a28 = 0;
a27 = 0;
a26 = 0;
a25 = 0;
a24 = 0;
a23 = 0;
a22 = 1;
a21 = 0;
a20 = 1;
a19 = 0;
a18 = 0;
a17 = 1;
a16 = 0;
a15 = 1;
a14 = 1;
a13 = 1;
a12 = 1;
a11 = 0;
a10 = 0;
a9 = 0;
a8 = 0;
a7 = 0;
a6 = 0;
a5 = 1;
a4 = 0;
a3 = 1;
a2 = 0;
a1 = 1;
a0 = 0;

// recombining the bits above, and adding a null type to the int
5435434 = combine32 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,());

// having a function returning a tuple:

def tuple_modifier (a0, a1, a2, a3, a4) {(a0+3, a1-1, a2^2, a4+a3+a2+a1, a4)};

def addition a b c d {
    (a+b+c+d, a, b, c, d)
};

def (r0, r1, r2, r3, r4) = addition 5 4 2 1;
r0 = 12;

(12, 5, 4, 2, 1) = addition 5 4 2 1;