use std::fs::File;
use std::io;
use std::io::{BufRead, Write};
use crate::ast;
use crate::transform::VarGen;
use crate::ast::{Definition, Expr, InfixOp, LetBinding, Module, Pat, TExpr, TPat, Variable};

pub trait DisplayLLVM {
    fn display_llvm(&self, file: &mut File, con_c: &mut i32);
}

impl DisplayLLVM for Module {
    fn display_llvm(&self, file: &mut File, con_c: &mut i32) {
        *con_c = 0;
        write!(file, "define i1 @main(").unwrap();
        let mut f_i = 0;
        for var in &self.pubs {
            if f_i == 0 {
                if let Some(name) = &var.name {
                    write!(file, "i64 %{name}").unwrap();
                } else {
                    write!(file, "i64 %v{}", var.id).unwrap();
                }
                f_i += 1;
            } else {
                if let Some(name) = &var.name {
                    write!(file, ", i64 %{name}").unwrap();
                } else {
                    write!(file, "i64 %v{}, ", var.id).unwrap();
                }
            }
        }
        writeln!(file, ") {{\nentry:").unwrap();
        for def in &self.defs {
            def.display_llvm(file, con_c);
        }
        for expr in &self.exprs {
            expr.display_llvm(file, con_c);
        }
        writeln!(file, "%res1 = and i1 %constr1, %constr2").unwrap();
        for i in 2..*con_c {
            writeln!(file, "%res{} = and i1 %res{}, %constr{}", i, i-1, i+1).unwrap();
        }

        write!(file, "ret i1 %res{}\n}}\n", *con_c - 1).unwrap();

    }
}

impl DisplayLLVM for Definition {
    fn display_llvm(&self, file: &mut File, con_c: &mut i32) {
        self.0.display_llvm(file, con_c);
    }
}

impl DisplayLLVM for LetBinding {
    fn display_llvm(&self, file: &mut File, con_c: &mut i32) {
        match &self.1.v {
            Expr::Variable(var) => {
                if let Some(name) = &var.name {
                    self.0.display_llvm(file, con_c);
                    writeln!(file, "p = alloca i64").unwrap();
                    write!(file, "store i64 %{name}, i64* ").unwrap();
                    self.0.display_llvm(file, con_c);
                    writeln!(file, "p").unwrap();
                    self.0.display_llvm(file, con_c);
                    write!(file, "= load i64, i64* ").unwrap();
                    self.0.display_llvm(file, con_c);
                    writeln!(file, "p").unwrap();
                } else {
                    self.0.display_llvm(file, con_c);
                    writeln!(file, "p = alloca i64").unwrap();
                    write!(file, "store i64 %v{}, i64* ", var.id).unwrap();
                    self.0.display_llvm(file, con_c);
                    writeln!(file, "p").unwrap();
                    self.0.display_llvm(file, con_c);
                    write!(file, "= load i64, i64* ").unwrap();
                    self.0.display_llvm(file, con_c);
                    writeln!(file, "p").unwrap();
                }
            }
            Expr::Constant(val) => {
                self.0.display_llvm(file, con_c);
                writeln!(file, "p = alloca i64").unwrap();
                write!(file, "store i64 {val}, i64* ").unwrap();
                self.0.display_llvm(file, con_c);
                writeln!(file, "p").unwrap();
                self.0.display_llvm(file, con_c);
                write!(file, "= load i64, i64* ").unwrap();
                self.0.display_llvm(file, con_c);
                writeln!(file, "p").unwrap();
            }
            _ => {
                self.0.display_llvm(file, con_c);
                write!(file, " = ").unwrap();
                self.1.display_llvm(file, con_c);
                writeln!(file).unwrap();
            }
        };
    }
}

impl DisplayLLVM for TPat {
    fn display_llvm(&self, file: &mut File, con_c: &mut i32) {
        match &self.v {
            Pat::Variable(var) => {
                var.display_llvm(file, con_c);
            }
            Pat::Constant(val) => write!(file, "{}", val).unwrap(),
            _ => writeln!(file, "{:?} not supported by LLVM", self.v).unwrap(),
        }
    }
}

impl DisplayLLVM for TExpr {
    fn display_llvm(&self, file: &mut File, con_c: &mut i32) {
        match &self.v {
            Expr::Infix(op, expr1, expr2) => {
                match op {
                    InfixOp::Divide | InfixOp::Multiply | InfixOp::Add | InfixOp::Subtract | InfixOp::IntDivide | InfixOp::Modulo | InfixOp::Exponentiate | InfixOp::DivideZ=> {
                        op.display_llvm(file, con_c);
                        expr1.display_llvm(file, con_c);
                        write!(file, ", ").unwrap();
                        expr2.display_llvm(file, con_c);
                    }
                    InfixOp::Equal => {
                        *con_c += 1;
                        match &expr2.v {
                            Expr::Infix(op2, expr12, expr22) => {
                                write!(file, "%subexp{} = ", *con_c).unwrap();
                                op2.display_llvm(file, con_c);
                                expr12.display_llvm(file, con_c);
                                write!(file, ", ").unwrap();
                                expr22.display_llvm(file, con_c);
                                write!(file, "\n").unwrap();
                                write!(file, "%constr{} = icmp eq i64 ", *con_c).unwrap();
                                expr1.display_llvm(file, con_c);
                                write!(file, ", ").unwrap();
                                write!(file, "%subexp{}", *con_c).unwrap();
                                writeln!(file).unwrap();
                            }
                            _ => {
                                op.display_llvm(file, con_c);
                                expr1.display_llvm(file, con_c);
                                write!(file, ", ").unwrap();
                                expr2.display_llvm(file, con_c);
                                writeln!(file).unwrap();
                            }
                        }
                    }
                }
            }
            Expr::Constant(val) => {
                write!(file, "{}", val).unwrap();
            }
            Expr::Variable(var) => {
                var.display_llvm(file, con_c);
            }
            _ => writeln!(file, "{:?} not supported by LLVM", self.v).unwrap(),
        }
    }
}

impl DisplayLLVM for Variable {
    fn display_llvm(&self, file: &mut File, con_c: &mut i32) {
        if let Some(name) = &self.name {
            write!(file, "%{}", name).unwrap();
        } else {
            write!(file, "%v{}", self.id).unwrap();
        }
    }
}

impl DisplayLLVM for InfixOp {
    fn display_llvm(&self, file: &mut File, con_c: &mut i32) {
        match self {
            Self::Divide => write!(file, "udiv i64 "),
            Self::Modulo => write!(file, "urem i64 "),
            Self::IntDivide => write!(file, "udiv i64 "),
            Self::Multiply => write!(file, "mul i64 "),
            Self::Add => write!(file, "add i64 "),
            Self::Subtract => write!(file, "sub i64 "),
            Self::Equal => write!(file, "%constr{} = icmp eq i64 ", *con_c),
            _ =>  panic!("Invalid InfixOp value for LLVM encountered. {:?}", self)
        }.unwrap();
    }
}

pub fn back_to_3ac(filename: &str) -> Module {
    let file = File::open(filename).unwrap();
    let reader = io::BufReader::new(file);
    let mut lines = reader.lines().skip(4).collect::<io::Result<Vec<String>>>().unwrap();
    let mut module_3ac = Module::default();

    fn get_arguments(input: &str) -> Vec<String> {
        let mut result = Vec::new();

        // Find the opening parenthesis index
        let open_parenthesis_index = input.find('(');

        if let Some(open_index) = open_parenthesis_index {
            // Find the closing parenthesis index
            let close_parenthesis_index = input.find(')');

            if let Some(close_index) = close_parenthesis_index {
                // Extract the substring between the parenthesis
                let arguments_str = &input[open_index + 1..close_index];

                // Split the arguments by commas
                let arguments: Vec<&str> = arguments_str.split(',').collect();

                // Iterate over the arguments and trim whitespace
                for arg in arguments {
                    let trimmed_arg = arg.trim();

                    // Extract the characters after the "%"
                    if let Some(arg_chars) = trimmed_arg.split_once('%') {
                        result.push(arg_chars.1.to_owned());
                    }
                }
            }
        }
        result
    }

    let pub_input_names = get_arguments(lines[0].as_ref());
    let mut gen = VarGen::new();
    for name in pub_input_names {
        let id = gen.generate_id();
        module_3ac.pubs.push(ast::Variable::new_name(id, Option::from(name)));
    }
    println!("{:?}", lines[2]);
    module_3ac
}