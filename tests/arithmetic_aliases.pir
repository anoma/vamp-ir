def add (priv x) (priv y) -> (priv z) {
  z = poly_gate[0 1 1 -1 0 0] x y x
}

def add (priv x) (priv y) -> (pub z) {
  z = pubout_poly_gate[0 1 1 0 0 0] x y x x
}

def add (priv x) (priv y) -> (const c) {
  poly_gate[0 1 1 0 0 c] x y x x
}

def mul (priv x) (priv y) -> (priv z) {
  z = poly_gate[1 0 0 -1 0 0] x y x
}

def mul (priv x) (priv y) -> (pub z) {
  z = pubout_poly_gate[1 0 0 0 0 0] x y x x
}

def mul (priv x) (priv y) -> (const c) {
  pubout_poly_gate[1 0 0 0 0 c] x y x x
}
