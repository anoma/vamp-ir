/* Implementation for less and mod operations as applications. Run as follows:
   vamp-ir setup -o params.pp
   vamp-ir compile -u params.pp -s tests/less_mod16.pir -o circuit.plonk
   vamp-ir prove -u params.pp -c circuit.plonk -o proof.plonk
   vamp-ir verify -u params.pp -c circuit.plonk -p proof.plonk
*/

// Ensure that the given argument is 1 or 0, and returns it
def bool x = { x*(x-1) = 0; x };

// Extract the 16 bits from a number argument
def range16 a = {
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
    a = a0 + 2*a1 + 4*a2 + 8*a3 + 16*a4 + 32*a5 + 64*a6 + 128*a7 + 256*a8 + 512*a9 + 1024*a10 + 2048*a11 + 4096*a12 + 8192*a13 + 16384*a14 + 32768*a15;
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, ())
};

// Extract the 15 bits from a number argument
def range15 a = {
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
    a = a0 + 2*a1 + 4*a2 + 8*a3 + 16*a4 + 32*a5 + 64*a6 + 128*a7 + 256*a8 + 512*a9 + 1024*a10 + 2048*a11 + 4096*a12 + 8192*a13 + 16384*a14;
    (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, ())
};

// It also range checks for -2^15 <= x < 2^15.
def intRange16 a = {
  range16 (a + 32768)
};

// It also range checks for -2^14 <= x < 2^14.
def intRange15 a = {
  range15 (a + 16384)
};

//intRange16 30000 = (0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, ());
//intRange16 ((-30000)) = (0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, ());

// Check if a number is non-negative
def negative16 a = {
  def (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, ()) = intRange16 a;

  a15
};

//negative16 0 = 1;
//negative16 165 = 1;
//negative16 ((-165)) = 0;

// Check if a number is negative
def nonNegative16 a = 1 - negative16 a;

//nonNegative16 0 = 0;
//nonNegative16 165 = 0;
//nonNegative16 ((-165)) = 1;

// Check if a number is positive
def positive16 a = nonNegative16 (a - 1);

//positive16 0 = 1;
//positive16 165 = 0;
//positive16 ((-165)) = 1;

// Test if a is less than b
def less16 a b = {
  // Range checks
  // Are these necessary?
  intRange15 a;
  intRange15 b;

  negative16 (a - b)
};

// Tests for less16
//less16 0 0 = 1;
//less16 1 0 = 1;
//less16 0 1 = 0;
//less16 1 1 = 1;
//less16 ((-1)) 0 = 0;
//less16 0 ((-1)) = 1;
//less16 ((-1)) ((-1)) = 1;
//less16 20 0 = 1;
//less16 0 20 = 0;
//less16 20 10 = 1;
//less16 10 20 = 0;
//less16 ((-20)) 0 = 0;
//less16 0 ((-20)) = 1;
//less16 ((-20)) ((-10)) = 0;
//less16 ((-10)) ((-20)) = 1;
//less16 ((-20)) 10 = 0;
//less16 10 ((-20)) = 1;
//less16 20 ((-10)) = 1;
//less16 ((-10)) 20 = 0;

// Pointwise Variant
def pointLess16 f g p q x = {
  def t = less16 (f x) (g x);
  (1 - t) * (p x) + t * (q x)
};

//pointLess16 (fun x  { x + 10 }) (fun x  { x * x }) (fun x  { x + 1 }) (fun x  { x - 1 }) 2 = 1;
//pointLess16 (fun x  { x + 10 }) (fun x  { x * x }) (fun x  { x + 1 }) (fun x  { x - 1 }) 10 = 11;

// Calculate the modulus of a and b
// Note: This gives correct answers for a >= 0 and b > 0.
//       For arbitrary a, "%" needs to be modified to give positive values when a < 0.
//	 Rust calculates (-5) % 3 as -1 when the actual answer should be 1.
//       Additionally "\" needs to be modified to perform floor division. 
//	 Rust calculates (-5)/3 as -1 when floor division would give -2.
def mod a b = {
  nonNegative16 b = 0;
  def q = fresh (a\b);
  def r = fresh (a%b);
  nonNegative16 r = 0;
  
  a = b * q + r;
  //less16 a (b * (q + 1)) = 0;
  less16 r b = 0;
  
  r
};

// Tests for mod
//mod 0 1 = 0;
//mod 0 2 = 0;
//mod 0 3 = 0;
//mod 0 4 = 0;
//mod 0 5 = 0;
//mod 1 1 = 0;
//mod 1 2 = 1;
//mod 1 3 = 1;
//mod 1 4 = 1;
//mod 1 5 = 1;
//mod 2 1 = 0;
//mod 2 2 = 0;
//mod 2 3 = 2;
//mod 2 4 = 2;
//mod 2 5 = 2;
//mod 3 1 = 0;
//mod 3 2 = 1;
//mod 3 3 = 0;
//mod 3 4 = 3;
mod 3 5 = 3;
//mod 4 1 = 0;
//mod 4 2 = 0;
//mod 4 3 = 1;
//mod 4 4 = 0;
//mod 4 5 = 4;
//mod 5 1 = 0;
//mod 5 2 = 1;
//mod 5 3 = 2;
//mod 5 4 = 1;
//mod 5 5 = 0;
mod 5 6 = 5;

// These don't work; see note over mod
//mod ((-5)) 2 = 1;
//mod ((-5)) 3 = 1;
//mod ((-5)) 4 = 3;
//mod ((-5)) 5 = 0;



