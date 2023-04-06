use crate::ast::{InfixOp, Module, Pat, Expr};
use crate::error::Error;
use crate::transform::{collect_module_variables, compile, FieldOps};

use serde_json::Map;
use std::collections::HashMap;

use std::fs;
use std::io::{BufWriter, Write};
use std::fs::OpenOptions;

use clap::{Args, Subcommand};
use std::path::PathBuf;

use num_bigint::BigInt;

use crate::qprintln;
use crate::util::Config;

#[derive(Subcommand)]
pub enum GenerateCommands {
    /// Export witnesses into a template JSON file
    WitnessFile(JSONWitnessFile),
    /// Export circuit into a comma-separated three-address format
    ThreeAddressFile(ThreeAddressFile),
    /// Export circuit into a z3 compatable format.
    Z3File(ThreeAddressFile),
}

#[derive(Args)]
pub struct JSONWitnessFile {
    /// Path to source file that witnesses come from
    #[arg(short, long)]
    source: PathBuf,
    /// Path to which the witness file is written
    #[arg(short, long)]
    output: PathBuf,
}


#[derive(Args)]
pub struct ThreeAddressFile {
    /// Path to source file that witnesses come from
    #[arg(short, long)]
    source: PathBuf,
    /// Path to which the witness file is written
    #[arg(short, long)]
    output: PathBuf,
}


#[derive(Args)]
pub struct Z3File {
    /// Path to source file that witnesses come from
    #[arg(short, long)]
    source: PathBuf,
    /// Path to which the witness file is written
    #[arg(short, long)]
    output: PathBuf,
}


// Trivial FieldOps for witness file generation
impl FieldOps for () {
    fn canonical(&self, a: BigInt) -> BigInt {
        a
    }
    fn negate(&self, a: BigInt) -> BigInt {
        a
    }
    fn infix(&self, _: InfixOp, a: BigInt, _: BigInt) -> BigInt {
        a
    }
}

/* Implements the subcommand that writes witnesses to a JSON file. */
pub fn witness_file_cmd(
    JSONWitnessFile { source, output }: &JSONWitnessFile,
    config: &Config,
) -> Result<(), Error> {
    qprintln!(config, "** Reading file...");
    let unparsed_file = fs::read_to_string(source).expect("cannot read file");
    let module = Module::parse(&unparsed_file).unwrap();
    let module_3ac = compile(module.clone(), &(), config);

    qprintln!(config, "** Collecting variables...");
    // Collect unbound variables from module
    let mut input_variables = HashMap::new();
    collect_module_variables(&module_3ac, &mut input_variables);

    // Defined variables should not be written to file
    for def in &module.defs {
        if let Pat::Variable(var) = &def.0 .0.v {
            input_variables.remove(&var.id);
        }
    }

    // Put values into a serde JSON friendly format.
    let mut input_variables_m = Map::new();
    for val in input_variables.values() {
        match &val.name {
            None => {}
            Some(x) => {
                input_variables_m.insert(x.to_string(), "?".into());
            }
        }
    }

    qprintln!(config, "** Writing witnesses to file...");
    std::fs::write(
        output,
        serde_json::to_string_pretty(&input_variables_m).unwrap(),
    )
    .unwrap();

    qprintln!(config, "** Witnesses file generation success!");

    Ok(())
}

fn atom_to_string(expr: &Expr) -> String {
    match expr {
        Expr::Constant(c) => c.to_string(),
        Expr::Variable(v) => format!("x{}", v.id),
        _ => "".to_string(),
    }
}

pub fn dump_equations_three_addr(module_3ac: &Module, path: &PathBuf) -> std::io::Result<()> {
    let file = OpenOptions::new()
        .write(true)
        .create(true)
        .open(path)?;
    let mut writer = BufWriter::new(file);
    
    for expr in &module_3ac.exprs {
        if let Expr::Infix(InfixOp::Equal, left, right) = &expr.v {
            match (&left.v, &right.v) {
                // a = c, for constant c and variables a
                (Expr::Variable(a), Expr::Constant(c)) => {
                    write!(
                        writer,
                        "= x{} {},\n",
                        a.id, c
                    )?;
                }
            
                // a = c, for variables a and c
                (Expr::Variable(a), Expr::Variable(c)) => {
                    write!(
                        writer,
                        "= x{} x{},\n",
                        a.id, c.id
                    )?;
                }
            
                (Expr::Variable(c), Expr::Infix(op, t1, t2)) => {
                    write!(
                      writer,
                      "{} {} {} x{},\n",
                      op, atom_to_string(&t1.v), atom_to_string(&t2.v), c.id
                    )?;
                }
                  
                _ => {}
            }
        }
    }

    writer.flush()?;
    Ok(())
}

/* Implements the subcommand that writes circuit into comma separated, three-address file. */
pub fn three_addr_file_cmd(ThreeAddressFile { source, output }: &ThreeAddressFile, config: &Config) -> Result<(), Error> {
    qprintln!(config, "** Reading file...");
    let unparsed_file = fs::read_to_string(source).expect("cannot read file");
    let module = Module::parse(&unparsed_file).unwrap();
    let module_3ac = compile(module.clone(), &(), config);
    
    qprintln!(config, "** Writing equations to file...");
    dump_equations_three_addr(&module_3ac, output).map_err(|err| qprintln!(config, "{:?}", err)).ok();
    qprintln!(config, "** Three address file generation success!");

    Ok(())
}


pub fn dump_equations_z3(module_3ac: &Module, path: &PathBuf) -> std::io::Result<()> {
    let file = OpenOptions::new()
        .write(true)
        .create(true)
        .open(path)?;
    let mut writer = BufWriter::new(file);
    
    for expr in &module_3ac.exprs {
        if let Expr::Infix(InfixOp::Equal, left, right) = &expr.v {
            match (&left.v, &right.v) {
                // a = c, for constant c and variables a
                (Expr::Variable(a), Expr::Constant(c)) => {
                    write!(
                        writer,
                        "(assert (= x{} {}))\n",
                        a.id, c
                    )?;
                }
            
                // a = c, for variables a and c
                (Expr::Variable(a), Expr::Variable(c)) => {
                    write!(
                        writer,
                        "(assert (= x{} x{}))\n",
                        a.id, c.id
                    )?;
                }
            
                (Expr::Variable(c), Expr::Infix(op, t1, t2)) => {
                    write!(
                      writer,
                      "(assert (= ({} {} {}) x{}))\n",
                      op, atom_to_string(&t1.v), atom_to_string(&t2.v), c.id
                    )?;
                }
                  
                _ => {}
            }
        }
    }

    writer.flush()?;
    Ok(())
}

/* Implements the subcommand that writes circuit into comma separated, three-address file. */
pub fn z3_file_cmd(ThreeAddressFile { source, output }: &ThreeAddressFile, config: &Config) -> Result<(), Error> {
    qprintln!(config, "** Reading file...");
    let unparsed_file = fs::read_to_string(source).expect("cannot read file");
    let module = Module::parse(&unparsed_file).unwrap();
    let module_3ac = compile(module.clone(), &(), config);
    
    qprintln!(config, "** Writing equations to file...");
    dump_equations_z3(&module_3ac, output).map_err(|err| qprintln!(config, "{:?}", err)).ok();

    qprintln!(config, "** Three address file generation success!");

    Ok(())
}

pub fn generate(generate_commands: &GenerateCommands, config: &Config) -> Result<(), Error> {
    match generate_commands {
        GenerateCommands::WitnessFile(args) => witness_file_cmd(args, config),
        GenerateCommands::ThreeAddressFile(args) => three_addr_file_cmd(args, config),
        GenerateCommands::Z3File(args) => z3_file_cmd(args, config),
    }
}
