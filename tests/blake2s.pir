/* An implementation of standard arithmetic logic unit operations in vampir. Run
   as follows:
   vamp-ir setup params.pp
   vamp-ir compile tests/alu.pir params.pp circuit.plonk
   vamp-ir prove circuit.plonk params.pp proof.plonk
   vamp-ir verify circuit.plonk params.pp proof.plonk
*/

// Ensure that the given argument is 1 or 0, and returns it
def bool x { x*(x-1) = 0; x };

// Extract the 32 bits from a number argument
def range32 a {
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
def zip32 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) (b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23,b24,b25,b26,b27,b28,b29,b30,b31,br) {
    ((a0,b0),(a1,b1),(a2,b2),(a3,b3),(a4,b4),(a5,b5),(a6,b6),(a7,b7),(a8,b8),(a9,b9),(a10,b10),(a11,b11),(a12,b12),(a13,b13),(a14,b14),(a15,b15),(a16,b16),(a17,b17),(a18,b18),(a19,b19),(a20,b20),(a21,b21),(a22,b22),(a23,b23),(a24,b24),(a25,b25),(a26,b26),(a27,b27),(a28,b28),(a29,b29),(a30,b30),(a31,b31),())
};
// Apply function to each element of tuple
def map f (g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,g27,g28,g29,g30,g31,gr) {
    (f g0, f g1, f g2, f g3, f g4, f g5, f g6, f g7, f g8, f g9, f g10, f g11, f g12, f g13, f g14, f g15, f g16, f g17, f g18, f g19, f g20, f g21, f g22, f g23, f g24, f g25, f g26, f g27, f g28, f g29, f g30, f g31, ())
};

// Multiply each tuple element by corresponding unit
def combine32 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31, ar) {
    a0 + 2*a1 + 4*a2 + 8*a3 + 16*a4 + 32*a5 + 64*a6 + 128*a7 + 256*a8 + 512*a9 + 1024*a10 + 2048*a11 + 4096*a12 + 8192*a13 + 16384*a14 + 32768*a15 + 65536*a16 + 131072*a17 + 262144*a18 + 524288*a19 + 1048576*a20 + 2097152*a21 + 4194304*a22 + 8388608*a23 + 16777216*a24 + 33554432*a25 + 67108864*a26 + 134217728*a27 + 268435456*a28 + 536870912*a29 + 1073741824*a30 + 2147483648*a31
};
def combine32_little (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31, ar) {
    a31 + 2*a30 + 4*a29 + 8*a28 + 16*a27 + 32*a26 + 64*a25 + 128*a24 + 256*a23 + 512*a22 + 1024*a21 + 2048*a20 + 4096*a19 + 8192*a18 + 16384*a17 + 32768*a16 + 65536*a15 + 131072*a14 + 262144*a13 + 524288*a12 + 1048576*a11 + 2097152*a10 + 4194304*a9 + 8388608*a8 + 16777216*a7 + 33554432*a6 + 67108864*a5 + 134217728*a4 + 268435456*a3 + 536870912*a2 + 1073741824*a1 + 2147483648*a0
};

// Apply given function to corresponding bit-pairs in bit representation
def bitwise32 g a b {
    def zipped = zip32 (range32 a) (range32 b);
    def new_bits = map g zipped;
    combine32 new_bits
};

// Definition of xor for domain {0, 1}^2
def bit_xor (a,b) { a*(1-b)+(1-a)*b };

// Definition of bitwise xor for 32 bit values
def xor32 = bitwise32 bit_xor;

// BLAKE 2 rotations
def r1 x {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range32 x;
    combine32 (a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, ())
    };
def r2 x {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range32 x;
    combine32 (a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, ())
    };
def r3 x {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range32 x;
    combine32 (a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a0, a1, a2, a3, a4, a5, a6, a7, ())
};
def r4 x {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,ar) = range32 x;
    combine32 (a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a0, a1, a2, a3, a4, a5, a6, ())
};

// BLAKE 2 mixer function
def g_mixer (va,vb,vc,vd,x,y) {
    def va1 = fresh((va + vb + x) % 4294967296);
    def vd1 = r1 (xor32 vd va1);
    def vc1 = fresh((vc + vd1) % 4294967296);
    def vb1 = r2 (xor32 vb vc1);
    def va2 = fresh((va1 + vb1 + y) % 4294967296);
    def vd2 = r3 (xor32 vd1 va2);
    def vc2 = fresh((vc1 + vd2) % 4294967296);
    def vb2 = r4 (xor32 vb1 vc2);
    (va2, vb2, vc2, vd2)
};

// BLAKE 2

// !! INPUT MESSAGE !!
def (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) = (6513249, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

// IV = (1779033703, 3144134277 , 1013904242, 2773480762, 1359893119, 2600822924, 528734635, 1541459225);
// // Parameter block p[0]
// h[0] := h[0] ^ 0x01010000 ^ (kk << 8) ^ nn
//
// 0x01010000 = 16842752
// kk = 'secret key' = 0
// In id-blake2s256 nn = 32
// 1779033703 ^ 16842752 ^ (0 << 8)= 1795745383

// First H vector, is equal to the IV
def (h0, h1, h2, h3, h4, h5, h6, h7) = (1779033703, 3144134277 , 1013904242, 2773480762, 1359893119, 2600822924, 528734635, 1541459225);

// Second H vector has first value modified
def h0 = xor32 (xor32 h0 16842752) 32;

def (h0_0, h1_0, h2_0, h3_0, h4_0, h5_0, h6_0, h7_0) = (h0, 3144134277 , 1013904242, 2773480762, 1359893119, 2600822924, 528734635, 1541459225);

// In our scenario, dd = 1. So we go straight to the final block:
// h := F( h, d[dd - 1], ll + bb, TRUE ) = F(h, m, ll, TRUE)
// h is the one from above.
// len(d) = 1, d[0] = (m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15)
// ll: Input bytes; bb: block bytes.
// ll = 3; bb = 64

def ll = 3;

// ################ F ################ //

// Inizialize working vector v, first half from the state h, second half from IV.

def (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15) = (h0_0, h1_0, h2_0, h3_0, h4_0, h5_0, h6_0, h7_0, 1779033703, 3144134277 , 1013904242, 2773480762, 1359893119, 2600822924, 528734635, 1541459225);
def v12 = xor32 v12 (fresh(ll % 4294967296));
def v14 = xor32 v14 4294967295;
def (v0_0,v1_0,v2_0,v3_0,v4_0,v5_0,v6_0,v7_0,v8_0,v9_0,v10_0,v11_0,v12_0,v13_0,v14_0,v15_0) = (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15);
// first check with reference
// (v0_0,v1_0,v2_0,v3_0,v4_0,v5_0,v6_0,v7_0,v8_0,v9_0,v10_0,v11_0,v12_0,v13_0,v14_0,v15_0) = (1795745351,3144134277,1013904242,2773480762,1359893119,2600822924,528734635,1541459225,1779033703,3144134277,1013904242,2773480762,1359893116,2600822924,3766232660,1541459225);

// mixer functions aggregation function. First act on the columns then on the diagnoals of the 4x4 v_ij matrix
def g_total (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15) (m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15){
    // Columns
    def (v0_1, v4_1, v8_1, v12_1) = g_mixer(v0, v4, v8, v12, m0, m1);
    def (v1_1, v5_1, v9_1, v13_1) = g_mixer(v1, v5, v9, v13, m2, m3);
    def (v2_1, v6_1, v10_1, v14_1) = g_mixer(v2, v6, v10, v14, m4, m5);
    def (v3_1, v7_1, v11_1, v15_1) = g_mixer(v3, v7, v11, v15, m6, m7);

    // Diagonals
    def (v0_2, v5_2, v10_2, v15_2) = g_mixer(v0_1, v5_1, v10_1, v15_1, m8, m9);
    def (v1_2, v6_2, v11_2, v12_2) = g_mixer(v1_1, v6_1, v11_1, v12_1, m10, m11);
    def (v2_2, v7_2, v8_2, v13_2) = g_mixer(v2_1, v7_1, v8_1, v13_1, m12, m13);
    def (v3_2, v4_2, v9_2, v14_2) = g_mixer(v3_1, v4_1, v9_1, v14_1, m14, m15);

    (v0_2, v1_2, v2_2, v3_2, v4_2, v5_2, v6_2, v7_2, v8_2, v9_2, v10_2, v11_2, v12_2, v13_2, v14_2, v15_2)
};

// Sigma permutations from reference
def sigma0 (s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15) {(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15)};
def sigma1 (s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15) {(s14, s10, s4, s8, s9, s15, s13, s6, s1, s12, s0, s2, s11, s7, s5, s3)};
def sigma2 (s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15) {(s11, s8, s12, s0, s5, s2, s15, s13, s10, s14, s3, s6, s7, s1, s9, s4)};
def sigma3 (s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15) {(s7, s9, s3, s1, s13, s12, s11, s14, s2, s6, s5, s10, s4, s0, s15, s8)};
def sigma4 (s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15) {(s9, s0, s5, s7, s2, s4, s10, s15, s14, s1, s11, s12, s6, s8, s3, s13)};
def sigma5 (s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15) {(s2, s12, s6, s10, s0, s11, s8, s3, s4, s13, s7, s5, s15, s14, s1, s9)};
def sigma6 (s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15) {(s12, s5, s1, s15, s14, s13, s4, s10, s0, s7, s6, s3, s9, s2, s8, s11)};
def sigma7 (s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15) {(s13, s11, s7, s14, s12, s1, s3, s9, s5, s0, s15, s4, s8, s6, s2, s10)};
def sigma8 (s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15) {(s6, s15, s14, s9, s11, s3, s0, s8, s12, s2, s13, s7, s1, s4, s10, s5)};
def sigma9 (s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15) {(s10, s2, s8, s4, s7, s6, s1, s5, s15, s11, s9, s14, s3, s12, s13, s0)};

def (m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15) = sigma0  (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15);
def (v0_1,v1_1,v2_1,v3_1,v4_1,v5_1,v6_1,v7_1,v8_1,v9_1,v10_1,v11_1,v12_1,v13_1,v14_1,v15_1) = g_total (v0_0,v1_0,v2_0,v3_0,v4_0,v5_0,v6_0,v7_0,v8_0,v9_0,v10_0,v11_0,v12_0,v13_0,v14_0,v15_0)(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15);
// check from reference:
// (v0_1,v1_1,v2_1,v3_1,v4_1,v5_1,v6_1,v7_1,v8_1,v9_1,v10_1,v11_1,v12_1,v13_1,v14_1,v15_1) = (379790382,3619021368,3465339467,2457529825,2813604057,2477039950,2756607025,1104442779,2512335827,2593767809,1619671659,3060152382,2056115471,3191312087,893198054,1001671787);

def (m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15) = sigma1 (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15);
def (v0_2,v1_2,v2_2,v3_2,v4_2,v5_2,v6_2,v7_2,v8_2,v9_2,v10_2,v11_2,v12_2,v13_2,v14_2,v15_2) = g_total (v0_1,v1_1,v2_1,v3_1,v4_1,v5_1,v6_1,v7_1,v8_1,v9_1,v10_1,v11_1,v12_1,v13_1,v14_1,v15_1)(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15);
// check from reference:
// (v0_2,v1_2,v2_2,v3_2,v4_2,v5_2,v6_2,v7_2,v8_2,v9_2,v10_2,v11_2,v12_2,v13_2,v14_2,v15_2) = (987959267,159557995,3900802484,1043569430,4064491725,241619750,3758484748,3583091319,403382074,4243863828,819684054,1212646172,2130757006,4219762822,3309627446,1380626658);

def (m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15) = sigma2 (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15);
def (v0_3,v1_3,v2_3,v3_3,v4_3,v5_3,v6_3,v7_3,v8_3,v9_3,v10_3,v11_3,v12_3,v13_3,v14_3,v15_3) = g_total (v0_2,v1_2,v2_2,v3_2,v4_2,v5_2,v6_2,v7_2,v8_2,v9_2,v10_2,v11_2,v12_2,v13_2,v14_2,v15_2)(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15);

def (m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15) = sigma3 (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15);
def (v0_4,v1_4,v2_4,v3_4,v4_4,v5_4,v6_4,v7_4,v8_4,v9_4,v10_4,v11_4,v12_4,v13_4,v14_4,v15_4) = g_total (v0_3,v1_3,v2_3,v3_3,v4_3,v5_3,v6_3,v7_3,v8_3,v9_3,v10_3,v11_3,v12_3,v13_3,v14_3,v15_3)(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15);

def (m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15) = sigma4 (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15);
def (v0_5,v1_5,v2_5,v3_5,v4_5,v5_5,v6_5,v7_5,v8_5,v9_5,v10_5,v11_5,v12_5,v13_5,v14_5,v15_5) = g_total (v0_4,v1_4,v2_4,v3_4,v4_4,v5_4,v6_4,v7_4,v8_4,v9_4,v10_4,v11_4,v12_4,v13_4,v14_4,v15_4)(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15);

def (m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15) = sigma5 (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15);
def (v0_6,v1_6,v2_6,v3_6,v4_6,v5_6,v6_6,v7_6,v8_6,v9_6,v10_6,v11_6,v12_6,v13_6,v14_6,v15_6) = g_total (v0_5,v1_5,v2_5,v3_5,v4_5,v5_5,v6_5,v7_5,v8_5,v9_5,v10_5,v11_5,v12_5,v13_5,v14_5,v15_5)(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15);

def (m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15) = sigma6 (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15);
def (v0_7,v1_7,v2_7,v3_7,v4_7,v5_7,v6_7,v7_7,v8_7,v9_7,v10_7,v11_7,v12_7,v13_7,v14_7,v15_7) = g_total (v0_6,v1_6,v2_6,v3_6,v4_6,v5_6,v6_6,v7_6,v8_6,v9_6,v10_6,v11_6,v12_6,v13_6,v14_6,v15_6)(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15);

def (m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15) = sigma7 (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15);
def (v0_8,v1_8,v2_8,v3_8,v4_8,v5_8,v6_8,v7_8,v8_8,v9_8,v10_8,v11_8,v12_8,v13_8,v14_8,v15_8) = g_total (v0_7,v1_7,v2_7,v3_7,v4_7,v5_7,v6_7,v7_7,v8_7,v9_7,v10_7,v11_7,v12_7,v13_7,v14_7,v15_7)(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15);

def (m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15) = sigma8 (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15);
def (v0_9,v1_9,v2_9,v3_9,v4_9,v5_9,v6_9,v7_9,v8_9,v9_9,v10_9,v11_9,v12_9,v13_9,v14_9,v15_9) = g_total (v0_8,v1_8,v2_8,v3_8,v4_8,v5_8,v6_8,v7_8,v8_8,v9_8,v10_8,v11_8,v12_8,v13_8,v14_8,v15_8)(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15);

def (m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15) = sigma9 (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15);
def (v0_10,v1_10,v2_10,v3_10,v4_10,v5_10,v6_10,v7_10,v8_10,v9_10,v10_10,v11_10,v12_10,v13_10,v14_10,v15_10) = g_total (v0_9,v1_9,v2_9,v3_9,v4_9,v5_9,v6_9,v7_9,v8_9,v9_9,v10_9,v11_9,v12_9,v13_9,v14_9,v15_9)(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15);
// check from reference
// (v0_10,v1_10,v2_10,v3_10,v4_10,v5_10,v6_10,v7_10,v8_10,v9_10,v10_10,v11_10,v12_10,v13_10,v14_10,v15_10) = (3653866666,3488365222,1879902898,741893902,2942967654,486686451,496943741,2488489893,1050672829,2527062033,4014497313,2788325754,3740207150,2940043489,1317438107,1307245882);

// last step of F function: xor halves and create new h vector
def xorhalves (h0,h1,h2,h3,h4,h5,h6,h7) (v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15){
    def h0_ = xor32 (xor32 h0 v0) v8;
    def h1_ = xor32 (xor32 h1 v1) v9;
    def h2_ = xor32 (xor32 h2 v2) v10;
    def h3_ = xor32 (xor32 h3 v3) v11;
    def h4_ = xor32 (xor32 h4 v4) v12;
    def h5_ = xor32 (xor32 h5 v5) v13;
    def h6_ = xor32 (xor32 h6 v6) v14;
    def h7_ = xor32 (xor32 h7 v7) v15;
    (h0_,h1_,h2_,h3_,h4_,h5_,h6_,h7_)
};

def (h0_1,h1_1,h2_1,h3_1,h4_1,h5_1,h6_1,h7_1) = xorhalves (h0_0, h1_0, h2_0, h3_0, h4_0, h5_0, h6_0, h7_0)(v0_10,v1_10,v2_10,v3_10,v4_10,v5_10,v6_10,v7_10,v8_10,v9_10,v10_10,v11_10,v12_10,v13_10,v14_10,v15_10);
// check from reference:
// (2355006544,3792993330,2737547233,793111374,545998135,691721886,1285265741,2186897286) = (h0_1,h1_1,h2_1,h3_1,h4_1,h5_1,h6_1,h7_1) ;
