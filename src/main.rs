extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::Parser;
pub mod ast;

#[derive(Parser)]
#[grammar = "plonk_ir.pest"]
struct PlonkIRParser;

pub(crate) fn print_pair(level: usize, pair: Pair<Rule>) {
    println!("{}{:?}: {}", "  ".repeat(level), pair.as_rule(), pair.as_str());

    pair.into_inner().for_each(|pair| print_pair(level + 1, pair));
}

fn parse_example() -> Result<Pairs<'static, Rule>, pest::error::Error<Rule>> {
    PlonkIRParser::parse(Rule::circuit,
                         "pub c d f
range a 2^6
range b 2^5
c = a + b
d = a * b
f = (fixed_base_scalar_mul e)")
}

fn parse_example_two() -> Result<Pairs<'static, Rule>, pest::error::Error<Rule>> {
    PlonkIRParser::parse(Rule::circuit,
                         "c = d * (fi a b c) + b ^ 5")
}

fn main() {
    let pairs = parse_example_two().unwrap_or_else(|e| panic!("{}", e));
    let first = pairs.clone().nth(0).unwrap_or_else(|| panic!("error"));

    pairs.for_each(|pair| print_pair(0, pair));
    println!("Testing repl like settings\n");
    println!("{:?}", first.as_rule());
}

#[cfg(test)]
mod tests {
    use crate::ast;

    #[test]
    fn test_running() {
        let pairs = super::parse_example().unwrap_or_else(|e| panic!("{}", e));
        let first = pairs.clone().nth(0).unwrap_or_else(|| panic!("error"));
        assert!(ast::Statement::Zero == ast::Statement::Zero);
    }

    #[test]
    fn test_running_second() {
        println!("(how fun)");
        println!(module_path!());
        let pairs = super::parse_example_two().unwrap_or_else(|e| panic!("{}", e));
        let first = pairs.clone().nth(0).unwrap_or_else(|| panic!("error"));
        println!("{:?}", first.as_span());
        let wire_a = String::from("a");
        assert!(   ast::Statement::Public { wire : wire_a.clone()}
                == ast::Statement::Public { wire : wire_a});
    }
}
