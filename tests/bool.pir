// defining a constant
def myval = 0;
// constraint expression
myval = 0;
// defining a function
def bool x { x*(x-1) = 0 };
// calling a function
bool 1;
// solicit a user input
bool a;
// defining a two parameter function
def myeq a b { a = b };
// solicit equal user inputs
myeq b c;
// currying example
(myeq d) e;
// currying with definition
def mycurry = myeq f;
// use defined curry
mycurry g;
// tuples
(h, j) = (k, l);
// tuple user input
def (m, n) = p;
// define higher order function
def map f (a,b,c,d) {
    f a;
    f b;
    f c;
    f d
};
// user higher order function
map bool q;
// computation + BIDMAS
def pyt a b { a*a + b*b };
// constrain computation
pyt x y = 25;
// let bindings
let a = 5 in a*5 = 25;
// function value
def myeq2 = fun (a,b) { a = b };
// polymorphism
map myeq2 r;
