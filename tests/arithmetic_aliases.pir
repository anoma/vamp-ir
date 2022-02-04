def add (priv x) (priv y) -> (priv z) {
  z = poly_gate[0 1 1 -1 0] x y
}

def add (priv x) (priv y) -> (pub z) {
  z = pubout_poly_gate[0 1 1 0 0] x y x
}

def add (priv x) (priv y) -> (const c) {
  poly_gate[0 1 1 0 c] x y x
}

def mul (priv x) (priv y) -> (priv z) {
  z = poly_gate[1 0 0 -1 0] x y
}

def mul (priv x) (priv y) -> (pub z) {
  z = pubout_poly_gate[1 0 0 0 0] x y x
}

def mul (priv x) (priv y) -> (const c) {
  poly_gate[1 0 0 0 c] x y x
}

def eq (priv x) -> (priv y) {
  y = poly_gate[0 1 0 -1 0] x x
}

def eq (priv x) -> (pub y) {
  y = pubout_poly_gate[0 1 0 0 0] x x x
}

def eq (priv x) -> (const y) {
  poly_gate[0 1 0 0 y] x x x
}

