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

def cons x y = x:y;

def append xs = fold xs cons;

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

// Make function that always returns constant

def const x y = x;

// Pair up corresponding elements of each tuple

def zip_aux x z (y:ys) = (x, y):(z ys);

def zip xs = fold xs zip_aux (const []);

// Apply function to each element of tuple

def map_ind f x acc = (f x):acc;

def map f xs = fold xs (map_ind f) [];

// Multiply each tuple element by corresponding unit

def combine_aux x y = x + 2*y;

def combine xs = fold xs combine_aux 0;

// Apply given function to corresponding bit-pairs in bit representation

def bitwise n g a b = {
    def zipped = zip (range n a) (range n b);
    def new_bits = map g zipped;
    combine new_bits
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

def not n y = combine (map (fun x { 1-x }) (range n y));

// Arithmetic shift right for 8 bit values

def ashr n x = {
    def a = range n x;
    def msb = nth a (n-1);
    def new_bits = append (tl a) (msb:[]);
    combine new_bits
};

// Logical shift right for 8 bit values

def lshr n x = {
    def a = range n x;
    def new_bits = append (tl a) (0:[]);
    combine new_bits
};

// Shift left for 8 bit values

def shl n x = {
    def a = range n x;
    combine (0:(take (n-1) a))
};

// Rotate right for 8 bit values

def ror n x = {
    def a = range n x;
    def msb = hd a;
    def new_bits = append (tl a) (msb:[]);
    combine new_bits
};

// Rotate left for 8 bit values

def rol n x = {
    def a = range n x;
    def lsb = nth a (n-1);
    def msbs = take (n-1) a;
    combine (lsb:msbs)
};

// Add two 8-bit values modulo 2^8

def add n a b = {
    def a = range (n+1) (a+b);
    combine (take n a)
};

// Subtract two 8-bit values modulo 8

def sub n a b = {
    def a = range (n+1) (a+(2^n)-b);
    combine (take n a)
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

// Single round of n bit unsigned Euclidean division

def div_next prev a b m n acc = {
    def n = n - 1;
    // Shift the divisor
    def c = b * (2^n);
    // Check if shifting produces larger number
    def d = ule m c a;
    // Subtract the shifted value if it is smaller or equal
    def e = a - d * c;
    // Move to the next round on the remainder
    prev e b m n (d:acc)
};

// n bit unsigned Euclidean division base case

def div_base a b m n acc = (acc, a);

// n bit unsigned Euclidean division

def divrem n a b = {
    // Inductively build up division function
    def div = iter n div_next div_base;
    // Do the division, doubling space for inequalities
    def (quot, rem) = div a b (2*n) n [];
    // Combine the bits into a number again
    (combine quot, rem)
};

def div n a b = fst (divrem n a b);

def rem n a b = snd (divrem n a b);

// Check the operations work correctly

242 = xor 8 13 255;

254 = not 8 1;

1 = ult 8 2 3;

1 = ult 8 2 255;

1 = ult 8 0 255;

1 = slt 8 255 0;

1 = slt 8 255 127;

1 = sle 8 5 5;

(25,21) = divrem 32 3846 153;

(282,0) = divrem 32 282 1;

range 4 15;

def D = 10;
