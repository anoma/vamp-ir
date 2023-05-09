use crate::pest::Parser;

use crate::ast::{Definition, Module, Rule, TExpr, VampirParser, Variable};
use crate::transform::{compile_repl, FieldOps};
//use crate::repl::transform::{compile, evaluate_module, collect_module_variables, FieldOps};

use crate::plonk::synth::PrimeFieldOps as PlonkPrimeFieldOps;
use ark_bls12_381::Fr;

use crate::halo2::synth::PrimeFieldOps as Halo2PrimeFieldOps;
use halo2_proofs::pasta::Fp;

use std::fs;
use std::io::Write;

use clap::Args;
use std::path::PathBuf;

#[derive(Args)]
pub struct REPL {
    /// Path to source file that is being loaded into repl
    #[arg(short, long)]
    source: Option<PathBuf>,
    /// Field to compute with
    #[arg(short, long)]
    field: String,
}

pub fn repl_cmd(source: &Option<PathBuf>, field_ops: &dyn FieldOps) {
    let mut module: Module = Module::default();

    if let Some(path) = source {
        let unparsed_file: String = fs::read_to_string(path).expect("cannot read file");
        module = Module::parse(&unparsed_file).unwrap();
        println!("Entering REPL with module loaded from file.");
        // println!("{}", module);
    } else {
        println!("Entering REPL with no module loaded.");
    }

    let mut defs: Vec<Definition>;
    let mut exprs: Vec<TExpr>;
    let mut pubs: Vec<Variable>;

    loop {
        print!("In : ");
        std::io::stdout().flush().expect("Error flushing stdout");

        let mut input: String = String::new();
        std::io::stdin()
            .read_line(&mut input)
            .expect("Error reading from stdin");

        if input.trim() == "quit" || input.trim() == "exit" {
            break;
        }

        defs = vec![];
        exprs = vec![];
        pubs = vec![];

        match VampirParser::parse(Rule::moduleItems, &input) {
            Ok(mut pairs) => {
                while let Some(pair) = pairs.next() {
                    match pair.as_rule() {
                        Rule::expr => {
                            let expr: TExpr = TExpr::parse(pair).expect("expected expression");
                            exprs.push(expr);
                        }
                        Rule::definition => {
                            let definition: Definition =
                                Definition::parse(pair).expect("expected definition");
                            defs.push(definition);
                        }
                        Rule::declaration => {
                            let mut pairs: pest::iterators::Pairs<Rule> = pair.into_inner();
                            while let Some(pair) = pairs.next() {
                                let var: Variable =
                                    Variable::parse(pair).expect("expected variable");
                                pubs.push(var);
                            }
                        }
                        Rule::EOI => continue,
                        _ => unreachable!(
                            "module item should either be expression, definition, or EOI"
                        ),
                    }
                }

                if let Some(last_expr) = compile_repl(defs, exprs, pubs, &mut module, field_ops) {
                    println!("Out: {}", last_expr);
                } else {
                    println!("No expression to evaluate.");
                }
            }
            Err(e) => eprintln!("Parse Error: {:?}", e),
        }
    }
}

pub fn repl(args: &REPL) {
    match args.field.as_str() {
        "Halo2" => repl_cmd(&args.source, &Halo2PrimeFieldOps::<Fp>::default()),
        "Plonk" => repl_cmd(&args.source, &PlonkPrimeFieldOps::<Fr>::default()),
        field_str => {
            if let Ok(_) = field_str.parse::<usize>() {
                //repl_cmd(&args.source, &fin_field(n))
                eprintln!("Arbitrary finite fields not implemented");
            } else {
                eprintln!("Invalid field value");
                return;
            }
        }
    };
}
