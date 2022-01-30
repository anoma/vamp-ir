def add x y -> z {
  z = poly_gate[0 1 1 -1 0] x y
}

def add x y -> (pub z) {
  z = pubout_poly_gate[0 1 1 0 0] x y x
}

def add x y -> (const c) {
  poly_gate[0 1 1 0 c] x y x
}

def mul x y -> z {
  z = poly_gate[1 0 0 -1 0] x y
}

def mul x y -> (pub z) {
  z = pubout_poly_gate[1 0 0 0 0] x y x
}

def mul x y -> (const c) {
  pubout_poly_gate[1 0 0 0 c] x y x
}
