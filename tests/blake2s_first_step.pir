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

def combine32 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31, ar) {
    a0 + 2*a1 + 2^2*a2 + 2^3*a3 + 2^4*a4 + 2^5*a5 + 2^6*a6 + 2^7*a7 + 2^8*a8 + 2^9*a9 + 2^10*a10 + 2^11*a11 + 2^12*a12 + 2^13*a13 + 2^14*a14 + 2^15*a15 + 2^16*a16 + 2^17*a17 + 2^18*a18 + 2^19*a19 + 2^20*a20 + 2^21*a21 + 2^22*a22 + 2^23*a23 + 2^24*a24 + 2^25*a25 + 2^26*a26 + 2^27*a27 + 2^28*a28 + 2^29*a29 + 2^30*a30 + 2^31*a31
};


// Apply given function to corresponding bit-pairs in bit representation

def bitwise32 g a b {
    def zipped = zip32 (range 32 a) (range 32 b);
    def new_bits = map g zipped;
    combine32 new_bits
};

// Definition of xor for domain {0, 1}^2

def bit_xor (a,b) { a*(1-b)+(1-a)*b };

// Definition of bitwise xor for 32 bit values

def xor32 = bitwise32 bit_xor;

// BLAKE 2 rotations

def r1 x {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range 32 x;
    combine32 (a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, ())
    };

def r2 x {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range 32 x;
    combine32 (a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, ())
    };

def r3 x {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range 32 x;
    combine32 (a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a0, a1, a2, a3, a4, a5, a6, a7, ())
};

def r4 x {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range 32 x;
    combine32 (a7, a8, a8, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a0, a1, a2, a3, a4, a5, a6, ())
};

def g_mixer (va,vb,vc,vd,x,y) {

	def va1 = va + vb + x;
	def vd1 = r1 (xor32 vd va);
	def vc1 = vc + vd;
	def vb1 = r2 (xor32 vb vc);

	def va2 = va1 + vb1 + y;
	def vd2 = r3 (xor32 vd1 va1);
	def vc2 = vc1 + vd1;
	def vb2 = r4 (xor32 vb1 vc1);
	
	(va1, vb, vc, vd)
};
// int 'abc' = (00, 99, 98, 97) (00, 00, 00, 00) (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00)

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

def (h0, h1, h2, h3, h4, h5, h6, h7) = (1795810919, 3144134277 , 1013904242, 2773480762, 1359893119, 2600822924, 528734635, 1541459225);

def (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15) = (h0, h1, h2, h3, h4, h5, h6, h7, 1779033703, 3144134277 , 1013904242, 2773480762, 1359893119, 2600822924, 528734635, 1541459225);

def (m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15) = (999897, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

// Coulumns

def (v0, v4, v8, v12) = g_mixer(v0,v4,v8,v12,m0,m1);
def (v1, v5, v9, v13) = g_mixer(v1,v5,v9,v13,m2,m3);
def (v2, v6, v10, v14) = g_mixer(v2,v6,v9,v13,m4,m5);
def (v3, v7, v11, v15) = g_mixer(v3,v7,v11,v15,m6,m7);

// Diagonals
def (v0, v4, v8, v12) = g_mixer(v0,v4,v12,v16,m8,m9);
def (v1, v5, v9, v13) = g_mixer(v1,v5,v8,v11,m10,m11);
def (v2, v6, v10, v14) = g_mixer(v2,v6,v9,v12,m12,m13);
def (v3, v7, v11, v15) = g_mixer(v3,v7,v11,v14,m14,m15);

def V_0_ref = (106923010324218920172, 18710317413313220216759, 6011024311425414824843, 1657924558952954241, 811482127173230130209, 1555104140436210831, 3113121717125165189107, 91224205251912633121, 10692301032431882018, 18710317413313220216759, 6011024311425414824843, 1657924558952954241, 811482127173230130210, 1555104140436210831, 2241243884419066148, 91224205251912633121);
def v_0_vamp = (v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15);
V_0_ref = v_0_vamp;