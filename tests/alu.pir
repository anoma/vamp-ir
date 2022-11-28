/* An implementation of standard arithmetic logic unit operations in vampir. Run
   as follows:
   vamp-ir setup -o params.pp
   vamp-ir compile -u params.pp -s tests/alu.pir -o circuit.plonk
   vamp-ir prove -u params.pp -c circuit.plonk -o proof.plonk
   vamp-ir verify -u params.pp -c circuit.plonk -p proof.plonk
*/

// Access elements of a list

def hd (h:t) = h;

def tl (h:t) = t;

def nth lst n = hd (iter n tl lst);

// Append two lists together

def append_base [] lst = lst;

def append_ind append (h:t) lst = h:(append t lst);

def append n = iter n append_ind append_base;

// Take the given prefix of a given list

def take_base lst = [];

def take_ind take (h:t) = h:(take t);

def take n = iter n take_ind take_base;

// Ensure that the given argument is 1 or 0, and returns it

def bool x = { x*(x-1) = 0; x };

// Extract the n+1 bits from a number argument
// given a range function for n bits

def range_ind range a = {
    def a0 = bool (fresh (a%2));
    def a1 = fresh (a\2);
    a = a0 + 2*a1;
    (a0 : range a1)
};

// Inductively define range for each bit width

def range_base 0 = [];

def range n = iter n range_ind range_base;

// Pair up corresponding elements of each tuple

def zip_base [] [] = [];

def zip_ind zip (ah:ar) (bh:br) = (ah,bh):(zip ar br);

def zip n = iter n zip_ind zip_base;

// Apply function to each element of tuple

def map_base f [] = [];

def map_ind map f (h:t) = (f h):(map f t);

def map n = iter n map_ind map_base;

// Multiply each tuple element by corresponding unit

def combine_base [] = 0;

def combine_ind combine (h:t) = h + 2*(combine t);

def combine n = iter n combine_ind combine_base;

// Apply given function to corresponding bit-pairs in bit representation

def bitwise n g a b = {
    def zipped = zip n (range n a) (range n b);
    def new_bits = map n g zipped;
    combine n new_bits
};

// Definition of xor for domain {0, 1}^2

def bit_xor (a,b) = a*(1-b)+(1-a)*b;

// Definition of bitwise xor for n bit values

def xor n = bitwise n bit_xor;

// Definition of or for domain {0, 1}^2

def bit_or (a,b) = a + b - a*b;

// Definition of bitwise or for n bit values

def or n = bitwise n bit_or;

// Definition of and for domain {0, 1}^2

def bit_and (a,b) = a*b;

// Definition of bitwise and for n bit values

def and n = bitwise n bit_and;

// Definition of bitwise not for n bit values

def not n y = combine n (map n (fun x { 1-x }) (range n y));

// Repeated function applications, useful for big-step rotations/shifts

def c0 f x = x;

def c1 f x = f (c0 f x);

def c2 f x = f (c1 f x);

def c3 f x = f (c2 f x);

def c4 f x = f (c3 f x);

def c5 f x = f (c4 f x);

def c6 f x = f (c5 f x);

def c7 f x = f (c6 f x);

// Arithmetic shift right for 8 bit values

def ashr n x = {
    def a = range n x;
    def msb = nth a (n-1);
    def new_bits = append (n-1) (tl a) (msb:[]);
    combine n new_bits
};

// Logical shift right for 8 bit values

def lshr n x = {
    def a = range n x;
    def new_bits = append (n-1) (tl a) (0:[]);
    combine n new_bits
};

// Shift left for 8 bit values

def shl n x = {
    def a = range n x;
    combine n (0:(take (n-1) a))
};

// Rotate right for 8 bit values

def ror n x = {
    def a = range n x;
    def msb = hd a;
    def new_bits = append (n-1) (tl a) (msb:[]);
    combine n new_bits
};

// Rotate left for 8 bit values

def rol n x = {
    def a = range n x;
    def lsb = nth a (n-1);
    def msbs = take (n-1) a;
    combine n (lsb:msbs)
};

// Add two 8-bit values modulo 2^8

def add n a b = {
    def a = range (n+1) (a+b);
    combine n (take n a)
};

// Subtract two 8-bit values modulo 8

def sub n a b = {
    def a = range (n+1) (a+(2^n)-b);
    combine n (take n a)
};

// Unsigned less than or equal to for 8 bits. 1 if true, 0 otherwise

def ule n a b = {
    def c = range (n+1) ((2^n)+b-a);
    nth c n
};

// Unsigned less than for 8 bits

def ult n a b = ule n a (b-1);

// Signed less than or equal to for 8 bits

def slt n a b = {
    def c = range (n+1) (a+(2^n)-b);
    nth c (n-1)
};

// Signed less than for 8 bits

def sle n a b = slt n a (b+1);

// Extract the first element of any supplied pair

def fst (a,b) = a;

// Extract the second element of any supplied pair

def snd (a,b) = b;

// 4 bit unsigned Euclidean division

def divrem a3 b0 = {
    def b1 = b0*2;
    def b2 = b1*2;
    def b3 = b2*2;
    def c3 = ult 8 b3 a3;
    def a2 = a3 - c3*b3;
    def c2 = ult 8 b2 a2;
    def a1 = a2 - c2*b2;
    def c1 = ult 8 b1 a1;
    def a0 = a1 - c1*b1;
    def c0 = ult 8 b0 a0;
    def rem = a0 - c0*b0;
    (combine 8 (c0:c1:c2:c3:0:0:0:0:[]), rem)
};

def div a b = fst (divrem a b);

def rem a b = snd (divrem a b);

// Check the operations work correctly

242 = xor 8 13 255;

254 = not 8 1;

1 = ult 8 2 3;

1 = ult 8 2 255;

1 = ult 8 0 255;

1 = slt 8 255 0;

1 = slt 8 255 127;

1 = sle 8 5 5;

(4,1) = divrem 9 2;

range 4 15;

def D = 10;
