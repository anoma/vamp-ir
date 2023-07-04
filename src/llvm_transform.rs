use std::fs::File;
use std::io::Write;

use crate::ast::{Definition, Expr, InfixOp, LetBinding, Module, Pat, TExpr, TPat, Variable};

pub trait DisplayLLVM {
    fn display_llvm(&self, file: &mut File, con_c: &mut i32);
}

impl DisplayLLVM for Module {
    fn display_llvm(&self, file: &mut File, con_c: &mut i32) {
        *con_c = 0;
        write!(file, "define i1 @main() {{\nentry:\n").unwrap();
        for var in &self.pubs {
            if let Some(name) = &var.name {
                writeln!(file, "%{name}{}p = alloca i64", var.id).unwrap();
                writeln!(file, "store i64 1, i64* %{name}{}p", var.id).unwrap();
                writeln!(file, "%{name}{} = load i64, i64* %{name}{}p", var.id, var.id).unwrap();
            } else {
                writeln!(file, "%{}p = alloca i64", var.id).unwrap();
                writeln!(file, "store i64 1, i64* %{}p", var.id).unwrap();
                writeln!(file, "%{} = load i64, i64* %{}p", var.id, var.id).unwrap();
            }
        }
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
        write!(file, "%").unwrap();
        if let Some(name) = &self.name {
            write!(file, "{}", name).unwrap();
        } else {
            write!(file, "v").unwrap();
        }
        write!(file, "{}", self.id).unwrap();
    }
}

impl DisplayLLVM for InfixOp {
    fn display_llvm(&self, file: &mut File, con_c: &mut i32) {
        match self {
            Self::Divide => write!(file, "udiv i64 "),
            Self::DivideZ => write!(file, "|"),
            Self::IntDivide => write!(file, "udiv i64 "),
            Self::Multiply => write!(file, "mul i64 "),
            Self::Add => write!(file, "add i64 "),
            Self::Subtract => write!(file, "sub i64 "),
            Self::Equal => write!(file, "%constr{} = icmp eq i64 ", *con_c),
            Self::Exponentiate => write!(file, "^"),
            Self::Modulo => write!(file, "urem i64 "),
        }.unwrap();
    }
}