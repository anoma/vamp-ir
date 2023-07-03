use crate::ast::{Expr, InfixOp, Module, Pat, Variable};
use crate::error::Error;
use crate::transform::{collect_module_variables, compile, FieldOps};
use crate::string::simplify_3ac;

use serde_json::Map;
use std::collections::{HashMap, HashSet};

use std::fs;
use std::fs::OpenOptions;
use std::io::{BufWriter, Write};

use clap::{Args, Subcommand};
use std::path::PathBuf;

use num_bigint::BigInt;

use crate::qprintln;
use crate::util::Config;

#[derive(Subcommand)]
pub enum GenerateCommands {
    /// Export witnesses into a template JSON file.
    WitnessFile(JSONWitnessFile),
    /// Export circuit into a comma-separated three-address format.
    ThreeAddressFile(ThreeAddressFile),
    /// Export circuit into a Z3 compatable format.
    Z3File(Z3File),
    /// Export circuit into a Mathematica compatable format.
    MathematicaFile(MathematicaFile),
    /// Export circuit into a Prolog compatable format.
    PrologFile(PrologFile),
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
    /// Path to source file that witnesses come from.
    #[arg(short, long)]
    source: PathBuf,
    /// Path to which the witness file is written.
    #[arg(short, long)]
    output: PathBuf,
}

#[derive(Args)]
pub struct Z3File {
    /// Path to source file that witnesses come from.
    #[arg(short, long)]
    source: PathBuf,
    /// Path to which the witness file is written.
    #[arg(short, long)]
    output: PathBuf,
    /// Type of elements within generated file. If unspecified, will be filed with "?".
    #[arg(short, long)]
    typ: Option<String>,
}

#[derive(Args)]
pub struct MathematicaFile {
    /// Path to source file that witnesses come from.
    #[arg(short, long)]
    source: PathBuf,
    /// Path to which the witness file is written.
    #[arg(short, long)]
    output: PathBuf,
    /// Type of elements within generated file. If unspecified, will be filed with "?".
    #[arg(short, long)]
    typ: Option<String>,
}

#[derive(Args)]
pub struct PrologFile {
    /// Path to source file that witnesses come from.
    #[arg(short, long)]
    source: PathBuf,
    /// Path to which the witness file is written.
    #[arg(short, long)]
    output: PathBuf,
}

// Trivial FieldOps for witness file generation
impl FieldOps for () {
    fn canonical(&self, a: BigInt) -> BigInt {
        a
    }
    fn negate(&self, a: BigInt) -> BigInt {
        -a
    }
    fn infix(&self, op: InfixOp, a: BigInt, b: BigInt) -> BigInt {
        match op {
            InfixOp::Add => a + b,
            InfixOp::Subtract => a - b,
            InfixOp::Multiply => a * b,
            InfixOp::Divide => panic!("cannot evaluate Divide expression"),
            InfixOp::DivideZ => panic!("cannot evaluate DivideZ expression"),
            InfixOp::IntDivide => a / b,
            InfixOp::Modulo => a % b,
            InfixOp::Exponentiate => a.pow(b.try_into().expect("exponent too large for pow()")),
            InfixOp::Equal => panic!("cannot evaluate equals expression"),
        }
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

fn var_to_string(var: &Variable) -> String {
    match &var.name {
        None => format!("Var{}", var.id),
        Some(n) => format!("Var{n}"),
    }
}

fn atom_to_string(expr: &Expr) -> String {
    match expr {
        Expr::Constant(c) => c.to_string(),
        Expr::Variable(v) => var_to_string(v),
        _ => "".to_string(),
    }
}

pub fn dump_equations_three_addr(module_3ac: &Module, path: &PathBuf) -> std::io::Result<()> {
    let file = OpenOptions::new().write(true).create(true).open(path)?;
    let mut writer = BufWriter::new(file);

    for expr in &module_3ac.exprs {
        if let Expr::Infix(InfixOp::Equal, left, right) = &expr.v {
            match (&left.v, &right.v) {
                // a = c, for constant c and variables a
                (Expr::Variable(_), Expr::Constant(_)) => {
                    writeln!(
                        writer,
                        "= {} {},",
                        atom_to_string(&left.v),
                        atom_to_string(&right.v)
                    )?;
                }

                // a = c, for variables a and c
                (Expr::Variable(_), Expr::Variable(_)) => {
                    writeln!(
                        writer,
                        "= {} {},",
                        atom_to_string(&left.v),
                        atom_to_string(&right.v)
                    )?;
                }

                // a = b op c, for variable a
                (Expr::Variable(_), Expr::Infix(op, t1, t2)) => {
                    writeln!(
                        writer,
                        "{} {} {} {},",
                        op,
                        atom_to_string(&t1.v),
                        atom_to_string(&t2.v),
                        atom_to_string(&left.v)
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
pub fn three_addr_file_cmd(
    ThreeAddressFile { source, output }: &ThreeAddressFile,
    config: &Config,
) -> Result<(), Error> {
    qprintln!(config, "** Reading file...");
    let unparsed_file = fs::read_to_string(source).expect("cannot read file");
    let module = Module::parse(&unparsed_file).unwrap();
    let module_3ac = compile(module, &(), config);

    qprintln!(config, "** Writing equations to file...");
    dump_equations_three_addr(&module_3ac, output)
        .map_err(|err| qprintln!(config, "{:?}", err))
        .ok();
    qprintln!(config, "** Three address file generation success!");

    Ok(())
}

pub fn dump_equations_z3(
    module_3ac: &Module,
    path: &PathBuf,
    typ: &Option<String>,
    config: &Config,
) -> std::io::Result<()> {
    let file = OpenOptions::new().write(true).create(true).open(path)?;
    let mut writer = BufWriter::new(file);

    let tystr = match typ {
        Some(s) => s,
        None => "?",
    };

    qprintln!(config, "** Collecting variables...");
    // Collect unbound variables from module
    let mut input_variables = HashMap::new();
    collect_module_variables(module_3ac, &mut input_variables);

    for var in input_variables.values() {
        writeln!(writer, "(declare-const {} {})", var_to_string(var), tystr)?;
    }

    for expr in &module_3ac.exprs {
        if let Expr::Infix(InfixOp::Equal, left, right) = &expr.v {
            match (&left.v, &right.v) {
                // a = c, for constant c and variables a
                (Expr::Variable(_), Expr::Constant(_)) => {
                    writeln!(
                        writer,
                        "(assert (= {} {}))",
                        atom_to_string(&left.v),
                        atom_to_string(&right.v)
                    )?;
                }

                // a = c, for variables a and c
                (Expr::Variable(_), Expr::Variable(_)) => {
                    writeln!(
                        writer,
                        "(assert (= {} {}))",
                        atom_to_string(&left.v),
                        atom_to_string(&right.v)
                    )?;
                }

                // a = b op c, for variable a
                (Expr::Variable(_), Expr::Infix(op, t1, t2)) => {
                    writeln!(
                        writer,
                        "(assert (= ({} {} {}) {}))",
                        op,
                        atom_to_string(&t1.v),
                        atom_to_string(&t2.v),
                        atom_to_string(&left.v)
                    )?;
                }

                _ => {}
            }
        }
    }

    write!(writer, "(check-sat)\n(get-model)\n")?;

    writer.flush()?;
    Ok(())
}

/* Implements the subcommand that writes circuit into comma separated, three-address file. */
pub fn z3_file_cmd(
    Z3File {
        source,
        output,
        typ,
    }: &Z3File,
    config: &Config,
) -> Result<(), Error> {
    qprintln!(config, "** Reading file...");
    let unparsed_file = fs::read_to_string(source).expect("cannot read file");
    let module = Module::parse(&unparsed_file).unwrap();
    let module_3ac = compile(module, &(), config);

    qprintln!(config, "** Writing equations to file...");
    dump_equations_z3(&module_3ac, output, typ, config)
        .map_err(|err| qprintln!(config, "{:?}", err))
        .ok();

    qprintln!(config, "** Z3 file generation success!");

    Ok(())
}

pub fn dump_equations_mathematica(
    module_3ac: &Module,
    path: &PathBuf,
    typ: &Option<String>,
    config: &Config,
) -> std::io::Result<()> {
    let file = OpenOptions::new().write(true).create(true).open(path)?;
    let mut writer = BufWriter::new(file);

    let tystr = match typ {
        Some(s) => s,
        None => "?",
    };

    qprintln!(config, "** Collecting variables...");
    // Collect unbound variables from module
    let mut input_variables = HashMap::new();
    collect_module_variables(module_3ac, &mut input_variables);

    writeln!(writer, "FindInstance[")?;

    let mut input_variables2 = HashMap::new();
    collect_module_variables(module_3ac, &mut input_variables2);
    // Defined variables should not be requested from user
    for def in &module_3ac.defs {
        if let Pat::Variable(var) = &def.0 .0.v {
            input_variables2.remove(&var.id);
        }
    }
    let mut input_ids = HashSet::new();
    for (id, _) in input_variables2 {
        input_ids.insert(id);
    }

    let simp_expr = simplify_3ac(module_3ac.exprs.clone(), &input_ids, &());

    for (index, expr) in simp_expr.iter().enumerate() {
        write!(writer, "  ")?;

        if let Expr::Infix(InfixOp::Equal, left, right) = &expr.v {
            //println!("{:?}", &expr.v);
            match (&left.v, &right.v) {
                // a = c, for constant c and variables a
                (Expr::Variable(_), Expr::Constant(_)) => {
                    write!(
                        writer,
                        "{} == {}",
                        atom_to_string(&left.v),
                        atom_to_string(&right.v)
                    )?;
                }

                // a = c, for variables a and c
                (Expr::Variable(_), Expr::Variable(_)) => {
                    write!(
                        writer,
                        "{} == {}",
                        atom_to_string(&left.v),
                        atom_to_string(&right.v)
                    )?;
                }

                // a = c, for variables a and c
                (Expr::Variable(_), Expr::Negate(var)) => {
                    if let Expr::Variable(_) = var.v {
                    write!(
                        writer,
                        "{} == - {}",
                        atom_to_string(&left.v),
                        atom_to_string(&var.v)
                    )?;}
                }

                // a = b op c, for variable a
                (Expr::Variable(_), Expr::Infix(op, t1, t2)) => {
                    write!(
                        writer,
                        "{} == {} {} {}",
                        atom_to_string(&left.v),
                        atom_to_string(&t1.v),
                        op,
                        atom_to_string(&t2.v),
                    )?;
                }

                _ => {}
            }
        }

        if index != module_3ac.exprs.len() - 1 {
            writeln!(writer, " &&")?;
        }
    }

    write!(writer, ", \n  {{")?;

    for (index, var) in input_variables.values().enumerate() {
        write!(writer, "{}", var_to_string(var))?;

        if index != input_variables.values().len() - 1 {
            write!(writer, ", ")?;
        }
    }

    writeln!(writer, "}}, {tystr}]")?;

    writer.flush()?;
    Ok(())
}

/* Implements the subcommand that writes circuit into a z3 file. */
pub fn mathematica_file_cmd(
    MathematicaFile {
        source,
        output,
        typ,
    }: &MathematicaFile,
    config: &Config,
) -> Result<(), Error> {
    qprintln!(config, "** Reading file...");
    let unparsed_file = fs::read_to_string(source).expect("cannot read file");
    let module = Module::parse(&unparsed_file).unwrap();
    let module_3ac = compile(module, &(), config);

    qprintln!(config, "** Writing equations to file...");
    dump_equations_mathematica(&module_3ac, output, typ, config)
        .map_err(|err| qprintln!(config, "{:?}", err))
        .ok();

    qprintln!(config, "** Mathematica file generation success!");

    Ok(())
}

pub fn dump_equations_prolog(
    module_3ac: &Module,
    path: &PathBuf,
    config: &Config,
) -> std::io::Result<()> {
    let file = OpenOptions::new().write(true).create(true).open(path)?;
    let mut writer = BufWriter::new(file);

    qprintln!(config, "** Collecting variables...");
    // Collect unbound variables from module
    let mut input_variables = HashMap::new();
    collect_module_variables(module_3ac, &mut input_variables);

    write!(writer, ":- use_module(library(clpfd)).\n\ncircuit(")?;

    for (index, var) in input_variables.values().enumerate() {
        write!(writer, "{}", var_to_string(var))?;

        if index != input_variables.values().len() - 1 {
            write!(writer, ", ")?;
        }
    }

    writeln!(writer, ") :- ")?;

    for (index, expr) in module_3ac.exprs.iter().enumerate() {
        write!(writer, "  ")?;

        if let Expr::Infix(InfixOp::Equal, left, right) = &expr.v {
            match (&left.v, &right.v) {
                // a = c, for constant c and variables a
                (Expr::Variable(_), Expr::Constant(_)) => {
                    write!(
                        writer,
                        "{} #= {}",
                        atom_to_string(&left.v),
                        atom_to_string(&right.v)
                    )?;
                }

                // a = c, for variables a and c
                (Expr::Variable(_), Expr::Variable(_)) => {
                    write!(
                        writer,
                        "{} #= {}",
                        atom_to_string(&left.v),
                        atom_to_string(&right.v)
                    )?;
                }

                // a = b op c, for variable a
                (Expr::Variable(_), Expr::Infix(op, t1, t2)) => {
                    write!(
                        writer,
                        "{} {} {} #= {}",
                        atom_to_string(&t1.v),
                        op,
                        atom_to_string(&t2.v),
                        atom_to_string(&left.v)
                    )?;
                }

                _ => {}
            }
        }

        if index != module_3ac.exprs.len() - 1 {
            writeln!(writer, ",")?;
        }
    }

    writeln!(writer, ".")?;

    writer.flush()?;
    Ok(())
}

/* Implements the subcommand that writes circuit into a prolog file. */
pub fn prolog_file_cmd(
    PrologFile { source, output }: &PrologFile,
    config: &Config,
) -> Result<(), Error> {
    qprintln!(config, "** Reading file...");
    let unparsed_file = fs::read_to_string(source).expect("cannot read file");
    let module = Module::parse(&unparsed_file).unwrap();
    let module_3ac = compile(module, &(), config);

    qprintln!(config, "** Writing equations to file...");
    dump_equations_prolog(&module_3ac, output, config)
        .map_err(|err| qprintln!(config, "{:?}", err))
        .ok();

    qprintln!(config, "** Prolog file generation success!");

    Ok(())
}

pub fn generate(generate_commands: &GenerateCommands, config: &Config) -> Result<(), Error> {
    match generate_commands {
        GenerateCommands::WitnessFile(args) => witness_file_cmd(args, config),
        GenerateCommands::ThreeAddressFile(args) => three_addr_file_cmd(args, config),
        GenerateCommands::Z3File(args) => z3_file_cmd(args, config),
        GenerateCommands::MathematicaFile(args) => mathematica_file_cmd(args, config),
        GenerateCommands::PrologFile(args) => prolog_file_cmd(args, config),
    }
}
