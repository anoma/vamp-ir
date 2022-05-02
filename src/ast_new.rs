extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;




#[derive(Parser)]
#[grammar = "vampir.pest"]
struct VampirParser;


fn main() {

    let test_poly = "x^3 + a*x + b - y*y";

    let pairs = VampirParser::parse(Rule::poly, test_poly);

    println!("{:?}", pairs);

    // fn parse_circuit(file: &str) -> Result<Circuit, Error<Rule>> {
    //     use pest::iterators::Pair;

    //     fn parse_objects(pair: Pair<Rule>) -> 
    // }
}