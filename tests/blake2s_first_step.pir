/* An implementation of standard arithmetic logic unit operations in vampir. Run
   as follows:
   vamp-ir setup params.pp
   vamp-ir compile tests/alu.pir params.pp circuit.plonk
   vamp-ir prove circuit.plonk params.pp proof.plonk
   vamp-ir verify circuit.plonk params.pp proof.plonk
*/
/* An implementation of standard arithmetic logic unit operations in vampir. Run
   as follows:
   vamp-ir setup params.pp
   vamp-ir compile tests/alu.pir params.pp circuit.plonk
   vamp-ir prove circuit.plonk params.pp proof.plonk
   vamp-ir verify circuit.plonk params.pp proof.plonk
*/

// Pair up corresponding elements of each tuple

def zip32 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) (b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23,b24,b25,b26,b27,b28,b29,b30,b31,br) {
    ((a0,b0),(a1,b1),(a2,b2),(a3,b3),(a4,b4),(a5,b5),(a6,b6),(a7,b7),(a9,b8),(a9,b9),(a10,b10),(a11,b11),(a12,b12),(a13,b13),(a14,b14),(a15,b15),(a16,b16),(a17,b17),(a18,b18),(a19,b19),(a20,b20),(a21,b21),(a22,b22),(a23,b23),(a24,b24),(a25,b25),(a26,b26),(a27,b27),(a28,b28),(a29,b29),(a30,b30),(a31,b31),())
};

// Apply function to each element of tuple

def map f (g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,g27,g28,g29,g30,g31,gr) {
    (f g0, f g1, f g2, f g3, f g4, f g5, f g6, f g7, f g8, f g9, f g10, f g11, f g12, f g13, f g14, f g15, f g16, f g17, f g18, f g19, f g20, f g21, f g22, f g23, f g24, f g25, f g26, f g27, f g28, f g29, f g30, f g31, ())
};


// Multiply each tuple element by corresponding unit

def combine32 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) {
    a0 + 2*a1 + 2^2*a2 + 2^3*a3 + 2^4*a4 + 2^5*a5 + 2^6*a6 + 2^7*a7 + 2^8*a8 + 2^9*a9 + 2^10*a10 + 2^11*a11 + 2^12*a12 + 2^13*a13 + 2^14*a14 + 2^15*a15 + 2^16*a16 + 2^17*a17 + 2^18*a18 + 2^19*a19 + 2^20*a20 + 2^21*a21 + 2^22*a22 + 2^23*a23 + 2^24*a24 + 2^25*a25 + 2^26*a26 + 2^27*a27 + 2^28*a28 + 2^29*a29 + 2^30*a30 + 2^31*a31
};


// Apply given function to corresponding bit-pairs in bit representation

def bitwise32 g a b {
    def zipped = zip32 (range 32 a) (range 32 b);
    def new_bits = map g zipped;
    combine32 new_bits
};

/*

// Definition of xor for domain {0, 1}^2

def bit_xor (a,b) { a*(1-b)+(1-a)*b };

// Definition of bitwise xor for 32 bit values

def xor32 = bitwise32 bit_xor;

// BLAKE 2 rotations

def R1 x {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) = ragne 32 x;
    combine32 (a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
};

def R2 x {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) = ragne 32 x;
    combine32 (a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
    };

def R3 x {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) = ragne 32 x;
    combine32 (a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a0, a1, a2, a3, a4, a5, a6, a7)
};

def R4 x {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) = ragne 32 x;
    combine32 (a7, a8, a8, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a0, a1, a2, a3, a4, a5, a6)
};

def sigma_0 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)}
def sigma_1 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x14, x10, x4, x8, x9, x15, x13, x6, x1, x12, x0, x2, x11, x7, x5, x3)}
def sigma_2 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x11, x8, x12, x0, x5, x2, x15, x13, x10, x14, x3, x6, x7, x1, x9, x4)}
def sigma_3 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x7, x9, x3, x1, x13, x12, x11, x14, x2, x6, x5, x10, x4, x0, x15, x8)}
def sigma_4 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x9, x0, x5, x7, x2, x4, x10, x15, x14, x1, x11, x12, x6, x8, x3, x13)}
def sigma_5 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x2, x12, x6, x10, x0, x11, x8, x3, x4, x13, x7, x5, x15, x14, x1, x9)}
def sigma_6 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x12, x5, x1, x15, x14, x13, x4, x10, x0, x7, x6, x3 x9 x2 x8 x11)}
def sigma_7 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x13, x11, x7 x14 x12 x1 x3 x9 x5 x0 x15, x4, x8, x6, x2, x10)}
def sigma_8 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x6, x15, x14, x9, x11, x3, x0, x8, x12, x2, x13, x7, x1, x4 x10 x5)}
def sigma_9 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x10, x2, x8, x4, x7, x6, x1, x5, x15, x11, x9, x14, x3, x12, x13, x0)}

def G v_a v_b v_c v_d x y {

	w_at = v_a + v_b + x;
	range 32 w_at;

	w_dt = R1 xor32 v_d  v_a;
	range32 w_dt;

	w_ct = v_c + v_d;
	range 32 w_ct;

	w_bt = R2 xor32 v_b v_c;
	range 32 w_bt;

	w_a = w_at + w_bt + y;
	range 32 w_a;

	w_d = R3 xor32 w_dt w_at;
	range 32 w_d

	w_c = mod (w_ct + w_dt) 2^32;
    range 32 w_c;

	w_b = R4 xor32 w_bt w_ct;
	range 32 w_b;
}

/// int 'abc' = (00, 99, 98, 97) (00, 00, 00, 00) (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00)

/*
|   h[0..7] := IV[0..7]                          // Initialization Vector.
*/

// def H = (1779033703, 3144134277 , 1013904242, 2773480762, 1359893119, 2600822924, 528734635, 1541459225);

/*
|   // Parameter block p[0]
|   h[0] := h[0] ^ 0x01010000 ^ (kk << 8) ^ nn
*/
// 0x01010000 = 16842752
// kk = 'secret key'
// In id-blake2s256 nn = 32

def (h0, h1, h2, h3, h4, h5, h6, h7)= (1795810919, 3144134277 , 1013904242, 2773480762, 1359893119, 2600822924, 528734635, 1541459225);

def (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15) = (h0, h1, h2, h3, h4, h5, h6, h7, 1779033703, 3144134277 , 1013904242, 2773480762, 1359893119, 2600822924, 528734635, 1541459225);


def (m0, m1, m2, m3, m4, m5, m6, m7) = (999897, 0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000);

// Coulumns
v0 v4 v8 v12 = G v0 v4 v8 v12 m0 m1;
v1 v5 v9 v13 = G v1 v5 v9 v13 m2 m3;
v2 v6 v10 v14 = G v2 v6 v9 v13 m4 m5;
v3 v7 v11 v15 = G v3 v7 v11 v15 m6 m7;

// Diagonals
v0 v4 v8 v12 = G v0 v4 v12 v16 m8 m9;
v1 v5 v9 v13 = G v1 v5 v8 v11 m10 m11;
v2 v6 v10 v14 = G v2 v6 v9 v12 m12 m13;
v3 v7 v11 v15 = G v3 v7 v11 v14 m14 m15;


def V_0_ref = (106923010324218920172, 18710317413313220216759, 6011024311425414824843, 1657924558952954241, 811482127173230130209, 1555104140436210831, 3113121717125165189107, 91224205251912633121, 10692301032431882018, 18710317413313220216759, 6011024311425414824843, 1657924558952954241, 811482127173230130210, 1555104140436210831, 2241243884419066148, 91224205251912633121);
def v_0_vamp = (v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15);
V_0_ref = v_0_vamp;