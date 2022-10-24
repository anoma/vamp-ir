use pest::iterators::Pair;
use std::fmt;
use std::fmt::Write;
use crate::typecheck::Type;
use crate::pest::Parser;
use bincode::{Encode, Decode};
use std::collections::{HashMap, HashSet};
use crate::transform::VarGen;
#[derive(Parser)]
#[grammar = "vampir.pest"]
pub struct VampirParser;

#[derive(Debug, Clone, Encode, Decode)]
pub struct Module {
    pub defs: Vec<Definition>,
    pub exprs: Vec<TExpr>,
}

impl Module {
    pub fn parse(unparsed_file: &str) -> Result<Self, pest::error::Error<Rule>> {
        let mut pairs = VampirParser::parse(Rule::moduleItems, &unparsed_file)?;
        let mut defs = vec![];
        let mut exprs = vec![];
        while let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::expr => {
                    let expr = TExpr::parse(pair).expect("expected expression");
                    exprs.push(expr);
                },
                Rule::definition => {
                    let definition = Definition::parse(pair).expect("expected definition");
                    defs.push(definition);
                },
                Rule::EOI => return Ok(Self {
                    defs,
                    exprs,
                }),
                _ => unreachable!("module item should either be expression, definition, or EOI")
            }
        }
        unreachable!("EOI should have been encountered")
    }
}

impl Default for Module {
    fn default() -> Self {
        Self { defs: vec![], exprs: vec![] }
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for def in &self.defs {
            writeln!(f, "{};", def)?;
        }
        for expr in &self.exprs {
            writeln!(f, "{};", expr)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct Definition(pub LetBinding);

impl Definition {
    pub fn parse(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::definition { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next().expect("definition should have a single let binding");
        let binding = LetBinding::parse(pair).expect("definition should contain single binding");
        Some(Self(binding))
    }
}

impl fmt::Display for Definition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Expr::Function(Function(params, body)) = &self.0.1.v {
            write!(f, "def {}", self.0.0)?;
            for param in params {
                write!(f, " {}", param)?;
            }
            let mut body_str = String::new();
            write!(body_str, "{}", body)?;
            if body_str.contains("\n") {
                body_str = body_str.replace("\n", "\n    ");
                write!(f, " =\n    {}", body_str)?
            } else {
                write!(f, " = {}", body_str)?
            }
        } else {
            let mut val_str = String::new();
            write!(val_str, "{}", self.0.1)?;
            if val_str.contains("\n") {
                val_str = val_str.replace("\n", "\n    ");
                write!(f, "def {} =\n    {}", self.0.0, val_str)?
            } else {
                write!(f, "def {} = {}", self.0.0, val_str)?
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct LetBinding(pub Pattern, pub Box<TExpr>);

impl LetBinding {
    pub fn parse(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::letBinding { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("let binding should not be empty");
        let expr = TExpr::parse(pair).expect("expression should end with expression");
        let pair = pairs.next().expect("let binding should have at least two parts");
        match pair.as_rule() {
            Rule::valueName => {
                let name = Variable::parse(pair).expect("expression should be value name");
                let mut pats = vec![];
                while let Some(pair) = pairs.next() {
                    let rhs = Pattern::parse(pair)
                        .expect("expected RHS to be a product");
                    pats.push(rhs);
                }
                let expr = Box::new(Expr::Function(Function(pats, Box::new(expr))).into());
                Some(Self(Pattern::Variable(name), expr))
            },
            Rule::pattern => {
                let pat = Pattern::parse(pair).expect("pattern should start with pattern");
                Some(Self(pat, Box::new(expr)))
            },
            _ => unreachable!("let binding is of unknown form")
        }
    }
}

impl fmt::Display for LetBinding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.1.v {
            Expr::Function(fun) => {
                write!(f, "{}", self.0)?;
                for pat in &fun.0 {
                    write!(f, " {}", pat)?;
                }
                write!(f, " =")?;
                let mut body = String::new();
                write!(body, "{}", fun.1)?;
                if body.contains("\n") {
                    write!(f, "\n    {}", body.replace("\n", "\n    "))?;
                } else {
                    write!(f, " {}", body)?;
                }
            },
            _ => write!(f, "{} = {}", self.0, self.1)?,
        };
        Ok(())
    }
}

#[derive(Debug, Clone, Encode, Decode)]
pub enum Pattern {
    Unit,
    Nil,
    As(Box<Pattern>, Variable),
    Product(Box<Pattern>, Box<Pattern>),
    Cons(Box<Pattern>, Box<Pattern>),
    Variable(Variable),
    Constant(i32),
}

impl Pattern {
    pub fn parse(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::pattern { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next().expect("pattern should not be empty");
        let mut pat =
            Self::parse_pat1(pair).expect("pattern should start with pattern");
        while let Some(pair) = pairs.next() {
            let name = Variable::parse(pair).expect("expected pattern name");
            pat = Self::As(Box::new(pat), name);
        }
        Some(pat)
    }

    pub fn parse_pat1(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::pattern1 { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("pattern should not be empty");
        let mut pats =
            Self::parse_pat2(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next_back() {
            let rhs = Self::parse_pat2(pair)
                .expect("expected RHS to be a product");
            pats = Self::Product(Box::new(rhs), Box::new(pats));
        }
        Some(pats)
    }

    pub fn parse_pat2(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::pattern2 { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("pattern should not be empty");
        let mut pats =
            Self::parse_pat3(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next_back() {
            let rhs = Self::parse_pat3(pair)
                .expect("expected RHS to be a product");
            pats = Self::Cons(Box::new(rhs), Box::new(pats));
        }
        Some(pats)
    }

    pub fn parse_pat3(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::pattern3 { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("expression should not be empty");
        match pair.as_rule() {
            Rule::constant if pair.as_str().starts_with("(") => {
                Some(Self::Unit)
            },
            Rule::constant if pair.as_str().starts_with("[") => {
                Some(Self::Nil)
            },
            Rule::constant => {
                let value = pair.as_str().parse().ok().expect("constant should be an integer");
                Some(Self::Constant(value))
            },
            Rule::valueName => {
                let name = Variable::parse(pair).expect("pattern should be value name");
                Some(Self::Variable(name))
            },
            Rule::pattern => Self::parse(pair),
            _ => unreachable!("pattern is of unknown form")
        }
    }

    pub fn to_expr(&self) -> TExpr {
        match self {
            Self::Unit => Expr::Unit.into(),
            Self::Nil => Expr::Nil.into(),
            Self::Constant(val) => Expr::Constant(*val).into(),
            Self::Variable(var) => Expr::Variable(var.clone()).into(),
            Self::As(pat, _name) => pat.to_expr(),
            Self::Product(pat1, pat2) => {
                Expr::Product(
                    Box::new(pat1.to_expr()),
                    Box::new(pat2.to_expr()),
                ).into()
            },
            Self::Cons(pat1, pat2) => {
                Expr::Cons(
                    Box::new(pat1.to_expr()),
                    Box::new(pat2.to_expr()),
                ).into()
            },
        }
    }

    pub fn to_typed_expr(&self, typ: Type) -> TExpr {
        match (self, typ) {
            (Self::Unit, Type::Unit | Type::Variable(_)) =>
                TExpr { v: Expr::Unit, t: Some(Type::Unit) },
            (Self::Constant(val), Type::Int | Type::Variable(_)) =>
                TExpr { v: Expr::Constant(*val), t: Some(Type::Int) },
            (Self::Variable(var), typ) =>
                TExpr { v:Expr::Variable(var.clone()), t: Some(typ) },
            (Self::As(pat, _name), typ) => pat.to_typed_expr(typ),
            (Self::Product(pat1, pat2), Type::Product(typ1, typ2)) =>
            {
                let expr1 = Box::new(pat1.to_typed_expr(*typ1.clone()));
                let expr2 = Box::new(pat2.to_typed_expr(*typ2.clone()));
                TExpr {
                    v: Expr::Product(expr1, expr2),
                    t: Some(Type::Product(typ1, typ2)),
                }
            },
            (_, typ) => panic!("expression {} cannot have the type {}", self, typ),
        }
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "()")?,
            Self::Nil => write!(f, "[]")?,
            Self::As(pat, name) => write!(f, "{} as {}", pat, name)?,
            Self::Product(pat1, pat2) =>
                write!(f, "({}, {})", pat1, pat2)?,
            Self::Cons(pat1, pat2) =>
                write!(f, "({}: {})", pat1, pat2)?,
            Self::Variable(var) => write!(f, "{}", var)?,
            Self::Constant(val) => write!(f, "{}", val)?,
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct TExpr {
    pub v: Expr,
    pub t: Option<Type>,
}

#[derive(Debug, Clone, Encode, Decode)]
pub enum Expr {
    Unit,
    Nil,
    Sequence(Vec<TExpr>),
    Product(Box<TExpr>, Box<TExpr>),
    Cons(Box<TExpr>, Box<TExpr>),
    Infix(InfixOp, Box<TExpr>, Box<TExpr>),
    Negate(Box<TExpr>),
    Application(Box<TExpr>, Box<TExpr>),
    Constant(i32),
    Variable(Variable),
    Function(Function),
    Intrinsic(Intrinsic),
    LetBinding(LetBinding, Box<TExpr>),
    Match(Match),
}

impl TExpr {
    pub fn parse(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr { return None }
        let string = pair.as_str();
        let mut pairs = pair.into_inner();
        if string.starts_with("fun") {
            let pair = pairs.next().expect("expression should not be empty");
            Function::parse(pair).map(|x| Expr::Function(x).into())
        } else if string.starts_with("match") {
            let pair = pairs.next().expect("expression should not be empty");
            Match::parse(pair).map(|x| Expr::Match(x).into())
        } else if string.starts_with("def") {
            let pair = pairs.next().expect("body expression should be prefixed by binding");
            let binding = LetBinding::parse(pair).expect("expression should start with binding");
            let mut body = vec![];
            while let Some(pair) = pairs.next() {
                body.push(Self::parse(pair).expect("expression should end with expression"));
            }
            if body.is_empty() { panic!("expression should not be empty") }
            Some(Expr::LetBinding(binding, Box::new(Expr::Sequence(body).into())).into())
        } else {
            let pair = pairs.next().expect("expression should not be empty");
            Self::parse_expr1(pair)
        }
    }
    
    pub fn parse_expr1(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr1 { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next().expect("expression should not be empty");
        let mut exprs =
            vec![Self::parse_expr2(pair).expect("expression should start with product")];
        while let Some(pair) = pairs.next() {
            let rhs = Self::parse_expr2(pair)
                .expect("expected RHS to be a product");
            exprs.push(rhs);
            
        }
        Some(if exprs.len() == 1 { exprs[0].clone() } else { Expr::Sequence(exprs).into() })
    }

    pub fn parse_expr2(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr2 { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("expression should not be empty");
        let mut exprs =
            Self::parse_expr3(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next_back() {
            let rhs = Self::parse_expr3(pair)
                .expect("expected RHS to be a product");
            exprs = Expr::Product(Box::new(rhs), Box::new(exprs)).into();
        }
        Some(exprs)
    }

    pub fn parse_expr3(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr3 { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next().expect("expression should not be empty");
        let mut expr =
            Self::parse_expr4(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next() {
            let op = InfixOp::parse(pair).expect("expected arithmetic operator");
            let rhs_pair = pairs.next().expect("expected RHS product");
            let rhs = Self::parse_expr4(rhs_pair)
                .expect("expected RHS to be a product");
            expr = Expr::Infix(op, Box::new(expr), Box::new(rhs)).into();
        }
        Some(expr)
    }

    pub fn parse_expr4(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr4 { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("expression should not be empty");
        let mut exprs =
            Self::parse_expr5(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next_back() {
            let rhs = Self::parse_expr5(pair)
                .expect("expected RHS to be a product");
            exprs = Expr::Cons(Box::new(rhs), Box::new(exprs)).into();
        }
        Some(exprs)
    }

    pub fn parse_expr5(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr5 { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next().expect("expression should not be empty");
        let mut expr =
            Self::parse_expr6(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next() {
            let op = InfixOp::parse(pair).expect("expected arithmetic operator");
            let rhs_pair = pairs.next().expect("expected RHS product");
            let rhs = Self::parse_expr6(rhs_pair)
                .expect("expected RHS to be a product");
            expr = Expr::Infix(op, Box::new(expr), Box::new(rhs)).into();
        }
        Some(expr)
    }

    pub fn parse_expr6(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr6 { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next().expect("expression should not be empty");
        let mut expr =
            Self::parse_expr7(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next() {
            let op = InfixOp::parse(pair).expect("expected arithmetic operator");
            let rhs_pair = pairs.next().expect("expected RHS product");
            let rhs = Self::parse_expr7(rhs_pair)
                .expect("expected RHS to be a product");
            expr = Expr::Infix(op, Box::new(expr), Box::new(rhs)).into();
        }
        Some(expr)
    }

    pub fn parse_expr7(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr7 { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("expression should not be empty");
        let mut expr =
            Self::parse_expr8(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next_back() {
            let op = InfixOp::parse(pair).expect("expected arithmetic operator");
            let lhs_pair = pairs.next_back().expect("expected RHS product");
            let lhs = Self::parse_expr8(lhs_pair)
                .expect("expected RHS to be a product");
            expr = Expr::Infix(op, Box::new(lhs), Box::new(expr)).into();
        }
        Some(expr)
    }

    pub fn parse_expr8(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr8 { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("expression should not be empty");
        let mut expr =
            Self::parse_expr9(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next_back() {
            if pair.as_rule() == Rule::negate {
                expr = Expr::Negate(Box::new(expr)).into();
            } else {
                unreachable!("only negative signs should occur here");
            }
        }
        Some(expr)
    }

    pub fn parse_expr9(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr9 { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next().expect("expression should not be empty");
        let mut expr =
            Self::parse_expr10(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next() {
            let rhs = Self::parse_expr10(pair)
                .expect("expected RHS to be a product");
            expr = Expr::Application(Box::new(expr), Box::new(rhs)).into();
        }
        Some(expr)
    }

    pub fn parse_expr10(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr10 { return None }
        let string = pair.as_str();
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("expression should not be empty");
        if pair.as_rule() == Rule::constant && string.starts_with("(") {
            Some(Expr::Unit.into())
        } else if pair.as_rule() == Rule::constant && string.starts_with("[") {
            Some(Expr::Nil.into())
        } else if pair.as_rule() == Rule::constant {
            let value = pair.as_str().parse().ok().expect("constant should be an integer");
            Some(Expr::Constant(value).into())
        } else if pair.as_rule() == Rule::valueName {
            let name = Variable::parse(pair).expect("expression should be value name");
            Some(Expr::Variable(name).into())
        } else if string.starts_with("(") || string.starts_with("fun") ||
        string.starts_with("def") || string.starts_with("match") {
            Self::parse(pair)
        } else {
            unreachable!("expression is of unknown form")
        }
    }
}

impl fmt::Display for TExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.v {
            Expr::Unit => write!(f, "()")?,
            Expr::Nil => write!(f, "[]")?,
            Expr::Sequence(exprs) => {
                if exprs.len() > 1 {
                    write!(f, "{{")?;
                }
                let mut iter = exprs.iter();
                let expr = iter.next()
                    .expect("sequence should contain at least one expression");
                write!(f, "{}", expr)?;
                while let Some(expr) = iter.next() {
                    write!(f, ";\n{}", expr)?;
                }
                if exprs.len() > 1 {
                    write!(f, "}}")?;
                }
            },
            Expr::Product(expr1, expr2) =>
                write!(f, "({}, {})", expr1, expr2)?,
            Expr::Cons(expr1, expr2) =>
                write!(f, "({}: {})", expr1, expr2)?,
            Expr::Infix(op, expr1, expr2) =>
                write!(f, "({}{}{})", expr1, op, expr2)?,
            Expr::Negate(expr) => write!(f, "-{}", expr)?,
            Expr::Application(expr1, expr2) => write!(f, "{} {}", expr1, expr2)?,
            Expr::Constant(val) => write!(f, "{}", val)?,
            Expr::Variable(var) => write!(f, "{}", var)?,
            Expr::Function(fun) => write!(f, "{}", fun)?,
            Expr::Intrinsic(intr) => write!(f, "{}", intr)?,
            Expr::LetBinding(binding, expr) => {
                if let Expr::Sequence(seq) = &expr.v {
                    write!(f, "def {}", binding)?;
                    for expr in seq {
                        write!(f, ";\n{}", expr)?;
                    }
                } else {
                    write!(f, "def {};\n{}", binding, expr)?;
                }
            },
            Expr::Match(matche) => write!(f, "{}", matche)?,
        }
        Ok(())
    }
}

impl From<Expr> for TExpr {
    fn from(v: Expr) -> TExpr {
        TExpr { v, t: None }
    }
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct Match(pub Box<TExpr>, pub Vec<Pattern>, pub Vec<TExpr>);

impl Match {
    pub fn parse(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::case { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next().expect("match should not be empty");
        let val = TExpr::parse(pair).expect("match should start with expression");
        let mut pats = vec![];
        let mut exprs = vec![];
        while let Some(pair) = pairs.next() {
            let pat =
                Pattern::parse(pair).expect("all prefixes to function should be patterns");
            let expr_pair =
                pairs.next().expect("pattern should followed by expression");
            let expr =
                TExpr::parse(expr_pair).expect("pattern should be followed by expression");
            pats.push(pat);
            exprs.push(expr);
        }
        Some(Self(Box::new(val), pats, exprs))
    }
}

impl fmt::Display for Match {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "match {} {{", self.0)?;
        for (pat, expr2) in self.1.iter().zip(self.2.iter()) {
            let mut body = String::new();
            writeln!(body, "{}", expr2)?;
            if body.contains('\n') {
                body = body.replace("\n", "\n    ");
                writeln!(f, "  {} => {{\n    {}}},", pat, body)?;
            } else {
                writeln!(f, "  {} => {},", pat, expr2)?;
            }
        }
        write!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Encode, Decode)]
pub enum InfixOp {
    Divide,
    Multiply,
    Add,
    Subtract,
    Equal,
    Exponentiate,
    IntDivide,
    Modulo,
}

impl InfixOp {
    pub fn parse(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::infixOp { return None }
        match pair.as_span().as_str() {
            "=" => Some(Self::Equal),
            "/" => Some(Self::Divide),
            "*" => Some(Self::Multiply),
            "+" => Some(Self::Add),
            "-" => Some(Self::Subtract),
            "^" => Some(Self::Exponentiate),
            "\\" => Some(Self::IntDivide),
            "%" => Some(Self::Modulo),
            _ => unreachable!("Encountered unknown infix operator")
        }
    }
}

impl fmt::Display for InfixOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Divide => write!(f, "/"),
            Self::Multiply => write!(f, "*"),
            Self::Add => write!(f, "+"),
            Self::Subtract => write!(f, "-"),
            Self::Equal => write!(f, "="),
            Self::Exponentiate => write!(f, "^"),
            Self::IntDivide => write!(f, "\\"),
            Self::Modulo => write!(f, "%"),
        }
    }
}

pub type VariableId = u32;

#[derive(Clone, Debug, Encode, Decode)]
pub struct Variable {
    pub name: Option<String>,
    pub id: VariableId,
}

impl Variable {
    pub fn new(id: VariableId) -> Self {
        Self { id, name: None }
    }
    
    pub fn parse(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::valueName { return None }
        Some(Self{name: Some(pair.as_str().to_string()), id: 0 })
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{}", name)?;
        }
        write!(f, "[{}]", self.id)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct Function(pub Vec<Pattern>, pub Box<TExpr>);

impl Function {
    pub fn parse(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::function { return None }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("function should not be empty");
        let body = TExpr::parse(pair).expect("function should end with expression");
        let mut params = vec![];
        while let Some(pair) = pairs.next() {
            let param =
                Pattern::parse(pair).expect("all prefixes to function should be patterns");
            params.push(param);
        }
        Some(Self(params, Box::new(body)))
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fun {}", self.0[0])?;
        for pat in &self.0[1..] {
            write!(f, " {}", pat)?;
        }

        let mut body = String::new();
        write!(body, "{}", self.1)?;
        if body.contains("\n") {
            body = body.replace("\n", "\n    ");
            write!(f, " ->\n    {}", body)?
        } else {
            write!(f, " -> {}", body)?
        }
        Ok(())
    }
}

/* The underlying function that expands an intrinsic call. */
type IntrinsicImp = fn(
    &Vec<TExpr>,
    &HashMap<VariableId, TExpr>,
    &mut HashSet<VariableId>,
    &mut VarGen
) -> TExpr;

#[derive(Clone)]
pub struct Intrinsic {
    arity: usize,
    pub imp_typ: Type,
    imp: IntrinsicImp,
    pub args: Vec<TExpr>,
}

impl bincode::Encode for Intrinsic {
    fn encode<E: bincode::enc::Encoder>(
        &self,
        _encoder: &mut E,
    ) -> core::result::Result<(), bincode::error::EncodeError> {
        panic!("intrinsic functions cannot be encoded")
    }
}

impl bincode::Decode for Intrinsic {
    fn decode<D: bincode::de::Decoder>(
        _decoder: &mut D,
    ) -> core::result::Result<Self, bincode::error::DecodeError> {
        panic!("intrinsic functions cannot be decoded")
    }
}

impl fmt::Debug for Intrinsic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Intrinsic")
            .field("arity", &self.arity)
            .field("args", &self.args)
            .field("apply", &(self.imp as fn(_, _, _, _) -> _))
            .finish()
    }
}

impl fmt::Display for Intrinsic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:p}", self.imp as fn(_, _, _, _) -> _)?;
        for arg in &self.args {
            write!(f, " {}", arg)?;
        }
        Ok(())
    }
}

impl Intrinsic {
    pub fn new(arity: usize, imp_typ: Type, imp: IntrinsicImp) -> Self {
        Self { arity, imp, imp_typ, args: vec![] }
    }
    
    pub fn apply(
        mut self,
        arg: TExpr,
        bindings: &HashMap<VariableId, TExpr>,
        prover_defs: &mut HashSet<VariableId>,
        gen: &mut VarGen
    ) -> TExpr {
        self.args.push(arg);
        if self.args.len() < self.arity {
            Expr::Intrinsic(self).into()
        } else if self.args.len() == self.arity {
            (self.imp)(&self.args, bindings, prover_defs, gen)
        } else {
            panic!("too many arguments applied to intrinsic function")
        }
    }
}
