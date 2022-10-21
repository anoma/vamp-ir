/* An implementation of standard arithmetic logic unit operations in vampir. Run
   as follows:
   vamp-ir setup -o params.pp
   vamp-ir compile -u params.pp -s tests/alu.pir -o circuit.plonk
   vamp-ir prove -u params.pp -c circuit.plonk -o proof.plonk
   vamp-ir verify -u params.pp -c circuit.plonk -p proof.plonk
*/

// Ensure that the given argument is 1 or 0, and returns it

def bool x { x*(x-1) = 0; x };

// Extract the n+1 bits from a number argument
// given a range function for n bits

def next_range range a {
    def a0 = bool (fresh (a%2));
    def a1 = fresh (a\2);
    a = a0 + 2*a1;
    (a0, range a1)
};

// Inductively define range for each bit width

def range0 0 { () };

def range1 = next_range range0;

def range2 = next_range range1;

def range3 = next_range range2;

def range4 = next_range range3;

def range5 = next_range range4;

def range6 = next_range range5;

def range7 = next_range range6;

def range8 = next_range range7;

def range9 = next_range range8;

// Pair up corresponding elements of each tuple

def zip8 (a0,a1,a2,a3,a4,a5,a6,a7,ar) (b0,b1,b2,b3,b4,b5,b6,b7,br) {
    ((a0,b0),(a1,b1),(a2,b2),(a3,b3),(a4,b4),(a5,b5),(a6,b6),(a7,b7),())
};

// Apply function to each element of tuple

def map f (g0,g1,g2,g3,g4,g5,g6,g7,gr) {
    (f g0, f g1, f g2, f g3, f g4, f g5, f g6, f g7, ())
};

// Multiply each tuple element by corresponding unit

def combine8 (a0,a1,a2,a3,a4,a5,a6,a7,ar) {
    a0 + 2*a1 + 4*a2 + 8*a3 + 16*a4 + 32*a5 + 64*a6 + 128*a7
};

// Apply given function to corresponding bit-pairs in bit representation

def bitwise8 g a b {
    def zipped = zip8 (range8 a) (range8 b);
    def new_bits = map g zipped;
    combine8 new_bits
};

// Definition of xor for domain {0, 1}^2

def bit_xor (a,b) { a*(1-b)+(1-a)*b };

// Definition of bitwise xor for 8 bit values

def xor8 = bitwise8 bit_xor;

// Definition of or for domain {0, 1}^2

def bit_or (a,b) { a + b - a*b };

// Definition of bitwise or for 8 bit values

def or8 = bitwise8 bit_or;

// Definition of and for domain {0, 1}^2

def bit_and (a,b) { a*b };

// Definition of bitwise and for 8 bit values

def and8 = bitwise8 bit_and;

// Definition of bitwise not for 8 bit values

def not8 y { combine8 (map (fun x { 1-x }) (range8 y)) };

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
    def (a0,a1,a2,a3,a4,a5,a6,a7,ar) = range8 x;
    combine8 (a1, a2, a3, a4, a5, a6, a7, a7, ())
};

// Logical shift right for 8 bit values

def lshr8 x {
    def (a0,a1,a2,a3,a4,a5,a6,a7,ar) = range8 x;
    combine8 (a1, a2, a3, a4, a5, a6, a7, 0, ())
};

// Shift left for 8 bit values

def shl8 x {
    def (a0,a1,a2,a3,a4,a5,a6,a7,ar) = range8 x;
    combine8 (0, a0, a1, a2, a3, a4, a5, a6, ())
};

// Rotate right for 8 bit values

def ror8 x {
    def (a0,a1,a2,a3,a4,a5,a6,a7,ar) = range8 x;
    combine8 (a1, a2, a3, a4, a5, a6, a7, a0, ())
};

// Rotate left for 8 bit values

def rol8 x {
    def (a0,a1,a2,a3,a4,a5,a6,a7,ar) = range8 x;
    combine8 (a7, a0, a1, a2, a3, a4, a5, a6, ())
};

// Add two 8-bit values modulo 8

def add8 a b {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,ar) = range9 (a+b);
    combine8 (a0, a1, a2, a3, a4, a5, a6, a7, ())
};

// Subtract two 8-bit values modulo 8

def sub8 a b {
    def (a0,a1,a2,a3,a4,a5,a6,a7,a8,ar) = range9 (a+256-b);
    combine8 (a0, a1, a2, a3, a4, a5, a6, a7, ())
};

// Unsigned less than or equal to for 8 bits. 1 if true, 0 otherwise

def ule8 a b {
    def (c0,c1,c2,c3,c4,c5,c6,c7,c8, ar) = range9 (256+b-a);
    c8
};

// Unsigned less than for 8 bits

def ult8 a b { ule8 a (b-1) };

// Signed less than or equal to for 8 bits

def slt8 a b {
    def (c0,c1,c2,c3,c4,c5,c6,c7,c8,ar) = range9 (a+256-b);
    c7
};

// Signed less than for 8 bits

def sle8 a b { slt8 a (b+1) };

// Extract the first element of any supplied pair

def fst (a,b) { a };

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
    (combine8 (c0, c1, c2, c3, 0, 0, 0, 0, ()), rem)
};

def div a b { fst (divrem a b) };

def rem a b { snd (divrem a b) };

// Check the operations work correctly

242 = xor8 13 255;

254 = not8 1;

1 = ult8 2 3;

1 = ult8 2 255;

1 = ult8 0 255;

1 = slt8 255 0;

1 = slt8 255 127;

1 = sle8 5 5;

(4,1) = divrem 9 2;

def D = 10;

range4 15;
