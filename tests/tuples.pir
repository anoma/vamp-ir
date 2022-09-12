/* Any assignment such that the sets {q.0, q.1, q.2, q.3} =
   {0, 1} and r.0.0=r.0.1, r.1.0=r.1.1, r.2.0=r.2.1, r.3.0=r.3.1
   is valid. Run as follows:
   vamp-ir setup params.pp
   vamp-ir compile tests/tuples.pir params.pp circuit.plonk
   vamp-ir prove circuit.plonk params.pp proof.plonk
   vamp-ir verify circuit.plonk params.pp proof.plonk
*/
// define higher order function
def map f (a,b,c,d) {
    f a;
    f b;
    f c;
    f d
};
// user higher order function
map bool q;
// function value
def myeq2 = fun (a,b) { a = b };
// polymorphism
map myeq2 r;
