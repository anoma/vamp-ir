use crate::pest::Parser;

use crate::ast::{VampirParser, Rule, Module, TExpr, Expr, Definition, Pat, InfixOp, Variable, VariableId};
use crate::transform::{compile, evaluate_module_repl, collect_module_variables, compile_repl, FieldOps, VarGen};
//use crate::repl::transform::{compile, evaluate_module, collect_module_variables, FieldOps};

use ark_bls12_381::{Fr};
use crate::plonk::synth::{PrimeFieldOps as PlonkPrimeFieldOps};

use halo2_proofs::pasta::{Fp};
use crate::halo2::synth::{PrimeFieldOps as Halo2PrimeFieldOps};

use std::fs; 
use std::fs::File;
use std::io::{BufWriter, BufReader, BufRead, Write};
use std::fs::OpenOptions;
use std::fmt::{Display};
use std::collections::{HashMap, HashSet};

use clap::{Args, Subcommand};
use std::path::PathBuf;

use num_bigint::BigInt;

#[derive(Subcommand)]
pub enum REPLCommands {
    /// Run repl evaluated using Halo2 field.
    Halo2(Halo2),
    /// Run repl evaluated using Plonk field.
    Plonk(Plonk),
}


#[derive(Args)]
pub struct Halo2 {
    /// Path to source file that is being loaded into repl
    #[arg(short, long)]
    source: Option<PathBuf>,
}


#[derive(Args)]
pub struct Plonk {
    /// Path to source file that is being loaded into repl
    #[arg(short, long)]
    source: Option<PathBuf>,
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

    let mut defs: Vec<Definition> = vec![];
    let mut exprs: Vec<TExpr> = vec![];
    let mut pubs: Vec<Variable> = vec![];

    loop {
        print!("In : ");
        std::io::stdout().flush().expect("Error flushing stdout");

        let mut input: String = String::new();
        std::io::stdin().read_line(&mut input).expect("Error reading from stdin");

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
                            let definition: Definition = Definition::parse(pair).expect("expected definition");
                            defs.push(definition);
                        }
                        Rule::declaration => {
                            let mut pairs: pest::iterators::Pairs<Rule> = pair.into_inner();
                            while let Some(pair) = pairs.next() {
                                let var: Variable = Variable::parse(pair).expect("expected variable");
                                pubs.push(var);
                            }
                        }
                        Rule::EOI => continue,
                        _ => unreachable!("module item should either be expression, definition, or EOI"),
                    }
                }

                module.defs.extend(defs);
                module.exprs.extend(exprs);
                module.pubs.extend(pubs);

                if let Some(last_expr) = compile_repl(module.clone(), field_ops) {
                    println!("Out: {}", last_expr);
                } else {
                    println!("No expression to evaluate.");
                }
                },
            Err(e) => eprintln!("Parse Error: {:?}", e)
        }
    }
}


/* Implements the subcommand that writes witnesses to a JSON file. */
pub fn halo2_repl_cmd(Halo2 { source }: &Halo2) {
    println!("** Entering Halo2 Repl");
    repl_cmd(&source, &Halo2PrimeFieldOps::<Fp>::default());
}

/* Implements the subcommand that writes circuit into comma separated, three-address file. */
pub fn plonk_repl_cmd(Plonk { source }: &Plonk) {
    println!("** Entering Plonk Repl");
    repl_cmd(&source, &PlonkPrimeFieldOps::<Fr>::default());
}

pub fn repl(repl_commands: &REPLCommands) {
    match repl_commands {
        REPLCommands::Halo2(args) => halo2_repl_cmd(args),
        REPLCommands::Plonk(args) => plonk_repl_cmd(args),
    }
}

