use plonk_ir::ast;

fn main() {
    let circuit = ast::parse_circuit_from_string("def a b c { gate a }");
    println!("{:?}", circuit);
}

