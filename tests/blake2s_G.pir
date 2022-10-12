/* An implementation of standard arithmetic logic unit operations in vampir. Run
   as follows:
   vamp-ir setup params.pp
   vamp-ir compile tests/alu.pir params.pp circuit.plonk
   vamp-ir prove circuit.plonk params.pp proof.plonk
   vamp-ir verify circuit.plonk params.pp proof.plonk
*/

// Pair up corresponding elements of each tuple

def zip a b i { (a i, b i) };

def zip8 (a0,a1,a2,a3,a4,a5,a6,a7,ar) (b0,b1,b2,b3,b4,b5,b6,b7,br) {
    ((a0,b0),(a1,b1),(a2,b2),(a3,b3),(a4,b4),(a5,b5),(a6,b6),(a7,b7),())
};

// Apply function to each element of tuple

def map f g i { f (g i) };

def combine32 a {
    (a 0) + 2*(a 1) + 2^2*(a 2) + 2^3*(a 3) + 2^4*(a 4) + 2^5*(a 5) + 2^6*(a 6) + 2^7*(a 7) + 2^8*(a 8) + 2^9*(a 9) + 2^10*(a 10) + 2^11*(a 11) + 2^12*(a 12) + 2^13*(a 13) + 2^14*(a 14) + 2^15*(a 15) + 2^16*(a 16) + 2^17*(a 17) + 2^18*(a 18) + 2^19*(a 19) + 2^20*(a 20) + 2^21*(a 21) + 2^22*(a 22) + 2^23*(a 23) +2^24*(a 24) +2^25*(a 25) +2^26*(a 26) +2^27*(a 27) + 2^28*(a 28) + 2^29*(a 29) + 2^30*(a 30) + 2^31*(a 31)
};

def combine8t (a0,a1,a2,a3,a4,a5,a6,a7) {
    a7 + 2*a6 + 4*a5 + 8*a4 + 16*a3 + 32*a2 + 64*a1 + 128*a0
};

def combine32t (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) {
    a31+ 2*a30 + 2^2*a29 + 2^3*a28 + 2^4*a27 + 2^5*a26 + 2^6*a25 + 2^7*a24 + 2^8*a23 + 2^9*a22 + 2^10*a21 + 2^11*a20 + 2^12*a19 + 2^13*a18 + 2^14*a17 + 2^15*a16 + 2^16*a15 + 2^17*a14 + 2^18*a13 + 2^19*a12 + 2^20*a11 + 2^21*a10 + 2^22*a9 + 2^23*a8 +2^24*a7 +2^25*a6 +2^26*a5 +2^27*a4 + 2^28*a3 + 2^29*a2 + 2^30*a1 + 2^31*a0
};

// Apply given function to corresponding bit-pairs in bit representation

def bitwise8 g a b {
    def zipped = zip (range 8 a) (range 8 b);
    def new_bits = map g zipped;
    combine8 new_bits
};

def bitwise32 g a b {
    def zipped = zip (range 32 a) (range 32 b);
    def new_bits = map g zipped;
    combine32 new_bits
};

// Definition of xor for domain {0, 1}^2

def bit_xor (a,b) { a*(1-b)+(1-a)*b };

// Definition of bitwise xor for 8 bit values

def xor8 = bitwise8 bit_xor;

// Definition of bitwise xor for 32 bit values

def xor32 = bitwise32 bit_xor;

// Definition of or for domain {0, 1}^2

def bit_or (a,b) { a + b - a*b };

// Definition of bitwise or for 8 bit values

def or8 = bitwise8 bit_or;

// Definition of bitwise or for 32 bit values

def or32 = bitwise32 bit_or;

// Definition of and for domain {0, 1}^2

def bit_and (a,b) { a*b };

// Definition of bitwise and for 8 bit values

def and8 = bitwise8 bit_and;

// Definition of bitwise and for 32 bit values

def and32 = bitwise32 bit_and;

// Definition of bitwise not for 8 bit values

def not8 y { combine8 (map (fun x { 1-x }) (range 8 y)) };

// Definition of bitwise not for 32 bit values

def not32 { combine32 (map (fun x { 1-x }) (range 32 y)) };

// Repeated function applications, useful for big-step rotations/shifts

def apply0 f x { x };

def apply1 f x { f (apply0 f x) };

def apply2 f x { f (apply1 f x) };

def apply3 f x { f (apply2 f x) };

def apply4 f x { f (apply3 f x) };

def apply5 f x { f (apply4 f x) };

def apply6 f x { f (apply5 f x) };

def apply7 f x { f (apply6 f x) };

// Arithmetic shift right for 8 bit values

def ashr8 x {
    def a = range 8 x;
    combine8t (a 1, a 2, a 3, a 4, a 5, a 6, a 7, a 7)
};

// Arithmetic shift right for 32 bit values

def ashr32 x {
    def a = range 32 x;
    combine32t (a 1, a 2, a 3, a 4, a 5, a 6, a 7, a 8, a 9, a 10, a 11, a 12, a 13, a 14, a 15, a 16, a 17, a 18, a 19, a 20, a 21, a 22, a 23, a 24, a 25, a 26, a 27, a 28, a 29, a 30, a 31, a 31)
};

// Logical shift right for 8 bit values

def lshr8 x {
    def a = range 8 x;
    combine8t (a 1, a 2, a 3, a 4, a 5, a 6, a 7, 0)
};

def lshr32 x {
    def a = range 32 x;
    combine32t (a 1, a 2, a 3, a 4, a 5, a 6, a 7, a 8, a 9, a 10, a 11, a 12, a 13, a 14, a 15, a 16, a 17, a 18, a 19, a 20, a 21, a 22, a 23, a 24, a 25, a 26, a 27, a 28, a 29, a 30, a 31, a 0)
};

// Shift left for 8 bit values

def shl8 x {
    def a = range 8 x;
    combine8t (0, a 0, a 1, a 2, a 3, a 4, a 5, a 6)
};

// Shift left for 32 bit values

def shl32 x {
    def a = range 32 x;
    combine8t (0, a 0, a 1, a 2, a 3, a 4, a 5, a 6, a 7, a 8, a 9, a 10, a 11, a 12, a 13, a 14, a 15, a 16, a 17, a 18, a 19, a 20, a 21, a 22, a 23, a 24, a 25, a 26, a 27, a 28, a 29, a 30)
};

// Rotate right for 8 bit values

def ror8 x {
    def a = range 8 x;
    combine8t (a 1, a 2, a 3, a 4, a 5, a 6, a 7, a 0)
};

// Rotate right for 32 bit values

def ror32 x{
    def a = range 32 x;
    combine32t (a 1, a 2, a 3, a 4, a 5, a 6, a 7, a 8, a 9, a 10, a 11, a 12, a 13, a 14, a 15, a 16, a 17, a 18, a 19, a 20, a 21, a 22, a 23, a 24, a 25, a 26, a 27, a 28, a 29, a 30, a 31, a 0)
};

// Rotate left for 8 bit values

def rol8 x {
    def a = range 8 x;
    combine8t (a 7, a 0, a 1, a 2, a 3, a 4, a 5, a 6)
};

// Rotate left for 32 bits values

def rol32 x {
    def a = range 32 x;
    combine8t (a 1, a 2, a 3, a 4, a 5, a 6, a 7, a 8, a 9, a 10, a 11, a 12, a 13, a 14, a 15, a 16, a 17, a 18, a 19, a 20, a 21, a 22, a 23, a 24, a 25, a 26, a 27, a 28, a 29, a 30, a 31, a 0)
};


// Add two 8-bit values modulo 8

def add8 a b {
    def a = range 9 (a+b);
    combine8t (a 0, a 1, a 2, a 3, a 4, a 5, a 6, a 7)
};

// Subtract two 8-bit values modulo 8

def sub8 a b {
    def a = range 9 (a+256-b);// Definition of bitwise and for 32 bit values

def and32 = bitwise32 bit_and;
    combine8t (a 0, a 1, a 2, a 3, a 4, a 5, a 6, a 7)
};

// Unsigned less than or equal to for 8 bits. 1 if true, 0 otherwise

def ule8 a b { range 9 (256+b-a) 8 };

// Unsigned less than for 8 bits

def ult8 a b { ule8 a (b-1) };

// Signed less than or equal to for 8 bits

def slt8 a b { range 9 (a+256-b) 7 };

// Signed less than for 8 bits

def sle8 a b { slt8 a (b+1) };

// Extract the first element of any supplied pair

def fst (a,b) { a };
def combine8 a {
    (a 0) + 2*(a 1) + 4*(a 2) + 8*(a 3) + 16*(a 4) + 32*(a 5) + 64*(a 6) + 128*(a 7)
};

// Extract the second element of any supplied pair

def snd (a,b) { b };

// 4 bit unsigned Euclidean division

def divrem a3 b0 {
    def b1 = b0*2;
    def b2 = b1*2;
    def b3 = b2*2;
    def c3 = ult8 b3 a3;
    def a2 = a3 - c3*b3;
    def c2 = ult8 b2 a2;
    def a1 = a2 - c2*b2;
    def c1 = ult8 b1 a1;
    def a0 = a1 - c1*b1;
    def c0 = ult8 b0 a0;
    def rem = a0 - c0*b0;
    (combine8t (c0, c1, c2, c3, 0, 0, 0, 0), rem)
};

def div a b { fst (divrem a b) };

def rem a b { snd (divrem a b) };

// BLAKE 2 rotations

def R1 x {
    def a = ragne32 x;
    combine32t (a 16, a 17, a 18, a 19, a 20, a 21, a 22, a 23, a 24, a 25, a 26, a 27, a 28, a 29, a 30, a 31, a 0, a 1, a 2, a 3, a 4, a 5, a 6, a 7, a 8, a 9, a 10, a 11, a 12, a 13, a 14, a 15)
};

def R2 x {
    def a = ragne32 x;
    combine32t (a 12, a 13, a 14, a 15, a 16, a 17, a 18, a 19, a 20, a 21, a 22, a 23, a 24, a 25, a 26, a 27, a 28, a 29, a 30, a 31, a 0, a 1, a 2, a 3, a 4, a 5, a 6, a 7, a 8, a 9, a 10, a 11)
    };

def R3 x {
    def a = range32 x;
    combine32t (a 8, a 9, a 10, a 11, a 12, a 13, a 14, a 15, a 16, a 17, a 18, a 19, a 20, a 21, a 22, a 23, a 24, a 25, a 26, a 27, a 28, a 29, a 30, a 31, a 0, a 1, a 2, a 3, a 4, a 5, a 6, a 7)
};

def R4 x {
    def a = range32 x;
    combine32t (a 7, a 8, a 8, a 10, a 11, a 12, a 13, a 14, a 15, a 16, a 17, a 18, a 19, a 20, a 21, a 22, a 23, a 24, a 25, a 26, a 27, a 28, a 29, a 30, a 31, a 0, a 1, a 2, a 3, a 4, a 5, a 6)
};

def sigma_0 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)}
/*
def sigma_1 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x14, x10, x4, x8, x9, x15, x13, x6, x1, x12, x0, x2, x11, x7, x5, x3)}
def sigma_2 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x11, x8, x12, x0, x5, x2, x15, x13, x10, x14, x3, x6, x7, x1, x9, x4)}
def sigma_3 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x7, x9, x3, x1, x13, x12, x11, x14, x2, x6, x5, x10, x4, x0, x15, x8)}
def sigma_4 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x9, x0, x5, x7, x2, x4, x10, x15, x14, x1, x11, x12, x6, x8, x3, x13)}
def sigma_5 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x2, x12, x6, x10, x0, x11, x8, x3, x4, x13, x7, x5, x15, x14, x1, x9)}
def sigma_6 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x12, x5, x1, x15, x14, x13, x4, x10, x0, x7, x6, x3 x9 x2 x8 x11)}
def sigma_7 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x13, x11, x7 x14 x12 x1 x3 x9 x5 x0 x15, x4, x8, x6, x2, x10)}
def sigma_8 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x6, x15, x14, x9, x11, x3, x0, x8, x12, x2, x13, x7, x1, x4 x10 x5)}
def sigma_9 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) {(x10, x2, x8, x4, x7, x6, x1, x5, x15, x11, x9, x14, x3, x12, x13, x0)}
*/

def G v_a v_b v_c v_d x y {

	// do I have to check if w_at is actually a 32-bit world?
	w_at = v_a + v_b + x;
	range32 w_at;

	w_dt = R1 xor32 v_d  v_a;
	range32 w_dt;

	w_ct = v_c + v_d;
	range32 w_ct;

	w_bt = R2 xor32 v_b v_c;
	range32 w_bt;

	w_a = w_at + w_bt + y;
	range32 w_a;

	w_d = R3 xor32 w_dt w_at;
	range32 w_d

	w_c = mod (w_ct + w_dt) 2^32;
    range32 w_c;

	w_b = R4 xor32 w_bt w_ct;
	range32 w_b;// Pair up corresponding elements of each tuple

def zip a b i { (a i, b i) };

def zip8 (a0,a1,a2,a3,a4,a5,a6,a7,ar) (b0,b1,b2,b3,b4,b5,b6,b7,br) {
    ((a0,b0),(a1,b1),(a2,b2),(a3,b3),(a4,b4),(a5,b5),(a6,b6),(a7,b7),())
};

// Apply function to each element of tuple

def map f g i { f (g i) };

def combine32 a {
    (a 0) + 2*(a 1) + 2^2*(a 2) + 2^3*(a 3) + 2^4*(a 4) + 2^5*(a 5) + 2^6*(a 6) + 2^7*(a 7) + 2^8*(a 8) + 2^9*(a 9) + 2^10*(a 10) + 2^11*(a 11) + 2^12*(a 12) + 2^13*(a 13) + 2^14*(a 14) + 2^15*(a 15) + 2^16*(a 16) + 2^17*(a 17) + 2^18*(a 18) + 2^19*(a 19) + 2^20*(a 20) + 2^21*(a 21) + 2^22*(a 22) + 2^23*(a 23) +2^24*(a 24) +2^25*(a 25) +2^26*(a 26) +2^27*(a 27) + 2^28*(a 28) + 2^29*(a 29) + 2^30*(a 30) + 2^31*(a 31)
};

def combine8t (a0,a1,a2,a3,a4,a5,a6,a7) {
    a7 + 2*a6 + 4*a5 + 8*a4 + 16*a3 + 32*a2 + 64*a1 + 128*a0
};

def combine32t (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31) {
    a31+ 2*a30 + 2^2*a29 + 2^3*a28 + 2^4*a27 + 2^5*a26 + 2^6*a25 + 2^7*a24 + 2^8*a23 + 2^9*a22 + 2^10*a21 + 2^11*a20 + 2^12*a19 + 2^13*a18 + 2^14*a17 + 2^15*a16 + 2^16*a15 + 2^17*a14 + 2^18*a13 + 2^19*a12 + 2^20*a11 + 2^21*a10 + 2^22*a9 + 2^23*a8 +2^24*a7 +2^25*a6 +2^26*a5 +2^27*a4 + 2^28*a3 + 2^29*a2 + 2^30*a1 + 2^31*a0
};

	(w_a, w_b, w_c, w_d)
}

/// hex 'abc' = (00, 63, 62, 61) (00, 00, 00, 00) (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00)
/// int 'abc' = (00, 99, 98, 97) (00, 00, 00, 00) (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00)
/// bin 'abc' = (00, 01100011, 01100010, 01100001) (00, 00, 00, 00) (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00)  (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00) (00, 00, 00, 00)

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

def H = (1795810919, 3144134277 , 1013904242, 2773480762, 1359893119, 2600822924, 528734635, 1541459225);

def V = (1795810919, 3144134277 , 1013904242, 2773480762, 1359893119, 2600822924, 528734635, 1541459225, 1779033703, 3144134277 , 1013904242, 2773480762, 1359893119, 2600822924, 528734635, 1541459225);

/*
|   v[12] := v[12] ^ (t mod 2**w)                // Low word of the offset.
|   v[13] := v[13] ^ (t >> w)                    // High word.
*/

// t = FALSE

def M = (999897, 0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000);

// Coulumns
v0 v4 v8 v12 = G V 0 V 4 V 8 V 12 M 0 M 1;
v1 v5 v9 v13 = G V 1 V 5 V 9 V 13 M 2 M 3;
v2 v6 v10 v14 = G V 2 V 6 V 9 V 13 M 4 M 5;
v3 v7 v11 v15 = G V 3 V 7 V 11 V 15 M 6 M 7;

// Diagonals
v0 v4 v8 v12 = G V 0 V 4 V 12 V 16 M 8 M 9;
v1 v5 v9 v13 = G V 1 V 5 V 8 V 11 M 10 M 11;
v2 v6 v10 v14 = G V 2 V 6 V 9 V 12 M 12 M 13;
v3 v7 v11 v15 = G V 3 V 7 V 11 V 14 M 14 M 15;


def V_0_ref = (106923010324218920172, 18710317413313220216759, 6011024311425414824843, 1657924558952954241, 811482127173230130209, 1555104140436210831, 3113121717125165189107, 91224205251912633121, 10692301032431882018, 18710317413313220216759, 6011024311425414824843, 1657924558952954241, 811482127173230130210, 1555104140436210831, 2241243884419066148, 91224205251912633121);
def v_0_vamp = (v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15);
V_0_ref = v_0_vamp;