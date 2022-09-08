def aa = fun x y {
    let a () { 30 } in
        x*y = a ();
        x*y = 30
}

aa 5 6;

// constrains values to be 0 or a
def bool a x { x*(x-a) = 0 }

def map (f,m) (a,b,c,d) {
    f a;
    f b;
    f c;
    f d
}

map (bool 1,0) c;
map (bool 1,0) c;

fun f { f a };

//let h = 1/g;;

h*j = 20;
