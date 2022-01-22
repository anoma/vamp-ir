use plonk_ir::ast;

fn main() {
    let circuit = ast::parse_circuit_from_string("pubout_poly_gate[0 0 1 1] y y y y x");
    println!("{:?}", circuit);
}

