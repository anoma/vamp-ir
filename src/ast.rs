use crate::pest::Parser;
use crate::transform::VarGen;
use crate::typecheck::Type;
use crate::util::parse_prefixed_num;
use bincode::{Decode, Encode, impl_borrow_decode};
use num_bigint::BigInt;
use pest::iterators::Pair;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::Write;
#[derive(Parser)]
#[grammar = "vampir.pest"]
pub struct VampirParser;

#[derive(Debug, Clone, Encode, Decode)]
pub struct Module {
    pub pubs: Vec<Variable>,
    pub defs: Vec<Definition>,
    pub exprs: Vec<TExpr>,
}

impl Module {
    pub fn parse(unparsed_file: &str) -> Result<Self, pest::error::Error<Rule>> {
        let mut pairs = VampirParser::parse(Rule::moduleItems, &unparsed_file)?;
        let mut defs = vec![];
        let mut exprs = vec![];
        let mut pubs = vec![];
        while let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::expr => {
                    let expr = TExpr::parse(pair).expect("expected expression");
                    exprs.push(expr);
                }
                Rule::definition => {
                    let definition = Definition::parse(pair).expect("expected definition");
                    defs.push(definition);
                }
                Rule::declaration => {
                    let mut pairs = pair.into_inner();
                    while let Some(pair) = pairs.next() {
                        let var = Variable::parse(pair).expect("expected variable");
                        pubs.push(var);
                    }
                }
                Rule::EOI => return Ok(Self { pubs, defs, exprs }),
                _ => unreachable!("module item should either be expression, definition, or EOI"),
            }
        }
        unreachable!("EOI should have been encountered")
    }
}

impl Default for Module {
    fn default() -> Self {
        Self {
            defs: vec![],
            exprs: vec![],
            pubs: vec![],
        }
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut prefix = "pub";
        for var in &self.pubs {
            write!(f, "{} {}", prefix, var)?;
            prefix = ",";
        }
        writeln!(f, ";")?;
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
        if pair.as_rule() != Rule::definition {
            return None;
        }
        let mut pairs = pair.into_inner();
        let pair = pairs
            .next()
            .expect("definition should have a single let binding");
        let binding = LetBinding::parse(pair).expect("definition should contain single binding");
        Some(Self(binding))
    }
}

impl fmt::Display for Definition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "def {}", self.0)
    }
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct LetBinding(pub TPat, pub Box<TExpr>);

impl LetBinding {
    pub fn parse(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::letBinding {
            return None;
        }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("let binding should not be empty");
        let expr = TExpr::parse(pair).expect("expression should end with expression");
        let pair = pairs
            .next()
            .expect("let binding should have at least two parts");
        match pair.as_rule() {
            Rule::valueName => {
                let name = Variable::parse(pair).expect("expression should be value name");
                let mut pats = vec![];
                while let Some(pair) = pairs.next() {
                    let rhs = TPat::parse(pair).expect("expected RHS to be a product");
                    pats.push(rhs);
                }
                let expr = Box::new(
                    Expr::Function(Function {
                        params: pats,
                        body: Box::new(expr),
                        env: HashMap::new(),
                    })
                    .type_expr(None),
                );
                Some(Self(Pat::Variable(name).type_pat(None), expr))
            }
            Rule::pattern => {
                let pat = TPat::parse(pair).expect("pattern should start with pattern");
                Some(Self(pat, Box::new(expr)))
            }
            _ => unreachable!("let binding is of unknown form"),
        }
    }
}

impl fmt::Display for LetBinding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.1.v {
            Expr::Function(Function { params, body, .. }) => {
                write!(f, "{}", self.0)?;
                for pat in params {
                    write!(f, " {}", pat)?;
                }
                write!(f, " =")?;
                let mut body_str = String::new();
                write!(body_str, "{}", body)?;
                if body_str.contains("\n") {
                    write!(f, "\n    {}", body_str.replace("\n", "\n    "))?;
                } else {
                    write!(f, " {}", body)?;
                }
            }
            _ => {
                let mut val_str = String::new();
                write!(val_str, "{}", self.1)?;
                if val_str.contains("\n") {
                    let val_str = val_str.replace("\n", "\n    ");
                    write!(f, "{} =\n    {}", self.0, val_str)?;
                } else {
                    write!(f, "{} = {}", self.0, val_str)?;
                }
            }
        };
        Ok(())
    }
}

// This structure is required to Bincode BigInts
struct BigIntBincode(BigInt);

impl bincode::Encode for BigIntBincode {
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> core::result::Result<(), bincode::error::EncodeError> {
        self.0.to_signed_bytes_le().encode(encoder)
    }
}

impl bincode::Decode for BigIntBincode {
    fn decode<D: bincode::de::Decoder>(
        decoder: &mut D,
    ) -> core::result::Result<Self, bincode::error::DecodeError> {
        let digits = Vec::<u8>::decode(decoder)?;
        Ok(Self(BigInt::from_signed_bytes_le(&digits)))
    }
}

#[derive(Debug, Clone)]
pub enum Pat {
    Unit,
    Nil,
    As(Box<TPat>, Variable),
    Product(Box<TPat>, Box<TPat>),
    Cons(Box<TPat>, Box<TPat>),
    Variable(Variable),
    Constant(BigInt),
}

// Encode is manually implemented for Pattern because some of its fields do not
// implement Encode. This implementation uses wrappers to effect the encoding of
// problematic fields.
impl ::bincode::Encode for Pat {
    fn encode<E: ::bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> core::result::Result<(), ::bincode::error::EncodeError> {
        match self {
            Self::Unit => {
                <u32 as ::bincode::Encode>::encode(&(0u32), encoder)?;
                Ok(())
            }
            Self::As(field_0, field_1) => {
                <u32 as ::bincode::Encode>::encode(&(1u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                ::bincode::Encode::encode(field_1, encoder)?;
                Ok(())
            }
            Self::Product(field_0, field_1) => {
                <u32 as ::bincode::Encode>::encode(&(2u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                ::bincode::Encode::encode(field_1, encoder)?;
                Ok(())
            }
            Self::Variable(field_0) => {
                <u32 as ::bincode::Encode>::encode(&(3u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                Ok(())
            }
            Self::Constant(field_0) => {
                <u32 as ::bincode::Encode>::encode(&(4u32), encoder)?;
                ::bincode::Encode::encode(&BigIntBincode(field_0.clone()), encoder)?;
                Ok(())
            }
            Self::Nil => {
                <u32 as ::bincode::Encode>::encode(&(5u32), encoder)?;
                Ok(())
            }
            Self::Cons(field_0, field_1) => {
                <u32 as ::bincode::Encode>::encode(&(6u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                ::bincode::Encode::encode(field_1, encoder)?;
                Ok(())
            }
        }
    }
}

// Decode is manually implemented for Pattern because some of its fields do not
// implement Decode. This implementation uses wrappers to effect the decoding of
// problematic fields.
impl ::bincode::Decode for Pat {
    fn decode<D: ::bincode::de::Decoder>(
        decoder: &mut D,
    ) -> core::result::Result<Self, ::bincode::error::DecodeError> {
        let variant_index = <u32 as ::bincode::Decode>::decode(decoder)?;
        match variant_index {
            0u32 => Ok(Self::Unit {}),
            1u32 => Ok(Self::As {
                0: ::bincode::Decode::decode(decoder)?,
                1: ::bincode::Decode::decode(decoder)?,
            }),
            2u32 => Ok(Self::Product {
                0: ::bincode::Decode::decode(decoder)?,
                1: ::bincode::Decode::decode(decoder)?,
            }),
            3u32 => Ok(Self::Variable {
                0: ::bincode::Decode::decode(decoder)?,
            }),
            4u32 => Ok(Self::Constant {
                0: <BigIntBincode as ::bincode::Decode>::decode(decoder)?.0,
            }),
            5u32 => Ok(Self::Nil {}),
            6u32 => Ok(Self::Cons {
                0: ::bincode::Decode::decode(decoder)?,
                1: ::bincode::Decode::decode(decoder)?,
            }),
            variant => Err(::bincode::error::DecodeError::UnexpectedVariant {
                found: variant,
                type_name: "Pattern",
                allowed: &::bincode::error::AllowedEnumVariants::Range { min: 0, max: 4 },
            }),
        }
    }
}

impl_borrow_decode!(Pat);

#[derive(Debug, Clone, Encode, Decode)]
pub struct TPat {
    pub v: Pat,
    pub t: Option<Type>,
}

impl Pat {
    pub fn type_pat(self, t: Option<Type>) -> TPat {
        let t = t.or_else(|| match &self {
            Self::Unit => Some(Type::Unit),
            Self::Cons(pat1, pat2) => pat2
                .t
                .clone()
                .or_else(|| pat1.t.clone().map(|x| Type::List(Box::new(x)))),
            Self::Product(pat1, pat2) => pat1
                .t
                .clone()
                .zip(pat2.t.clone())
                .map(|(a, b)| Type::Product(Box::new(a), Box::new(b))),
            Self::As(pat1, _) => pat1.t.clone(),
            Self::Constant(_) => Some(Type::Int),
            Self::Variable(_) | Self::Nil => None,
        });
        TPat { v: self, t }
    }
}

impl TPat {
    pub fn parse(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::pattern {
            return None;
        }
        let mut pairs = pair.into_inner();
        let pair = pairs.next().expect("pattern should not be empty");
        let mut pat = Self::parse_pat1(pair).expect("pattern should start with pattern");
        while let Some(pair) = pairs.next() {
            let name = Variable::parse(pair).expect("expected pattern name");
            pat = Pat::As(Box::new(pat), name).type_pat(None);
        }
        Some(pat)
    }

    pub fn parse_pat1(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::pattern1 {
            return None;
        }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("pattern should not be empty");
        let mut pats = Self::parse_pat2(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next_back() {
            let rhs = Self::parse_pat2(pair).expect("expected RHS to be a product");
            pats = Pat::Product(Box::new(rhs), Box::new(pats)).type_pat(None);
        }
        Some(pats)
    }

    pub fn parse_pat2(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::pattern2 {
            return None;
        }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("pattern should not be empty");
        let mut pats = Self::parse_pat3(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next_back() {
            let rhs = Self::parse_pat3(pair).expect("expected RHS to be a product");
            pats = Pat::Cons(Box::new(rhs), Box::new(pats)).type_pat(None);
        }
        Some(pats)
    }

    pub fn parse_pat3(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::pattern3 {
            return None;
        }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("expression should not be empty");
        match pair.as_rule() {
            Rule::constant if pair.as_str().starts_with("(") => Some(Pat::Unit.type_pat(None)),
            Rule::constant if pair.as_str().starts_with("[") => Some(Pat::Nil.type_pat(None)),
            Rule::constant => {
                let value = pair
                    .as_str()
                    .parse()
                    .ok()
                    .expect("constant should be an integer");
                Some(Pat::Constant(value).type_pat(None))
            }
            Rule::valueName => {
                let name = Variable::parse(pair).expect("pattern should be value name");
                Some(Pat::Variable(name).type_pat(None))
            }
            Rule::pattern => Self::parse(pair),
            _ => unreachable!("pattern is of unknown form"),
        }
    }

    pub fn to_expr(&self) -> TExpr {
        let v = match &self.v {
            Pat::Unit => Expr::Unit,
            Pat::Nil => Expr::Nil,
            Pat::Constant(val) => Expr::Constant(val.clone()),
            Pat::Variable(var) => Expr::Variable(var.clone()),
            Pat::As(pat, _name) => pat.to_expr().v,
            Pat::Product(pat1, pat2) => {
                Expr::Product(Box::new(pat1.to_expr()), Box::new(pat2.to_expr()))
            }
            Pat::Cons(pat1, pat2) => Expr::Cons(Box::new(pat1.to_expr()), Box::new(pat2.to_expr())),
        };
        TExpr {
            v,
            t: self.t.clone(),
        }
    }
}

impl fmt::Display for TPat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.v {
            Pat::Unit => write!(f, "()")?,
            Pat::Nil => write!(f, "[]")?,
            Pat::As(pat, name) => write!(f, "{} as {}", pat, name)?,
            Pat::Product(pat1, pat2) => write!(f, "({}, {})", pat1, pat2)?,
            Pat::Cons(pat1, pat2) => write!(f, "({}: {})", pat1, pat2)?,
            Pat::Variable(var) => write!(f, "{}", var)?,
            Pat::Constant(val) => write!(f, "{}", val)?,
        }
        Ok(())
    }
}

// Encode is manually implemented for Pattern because some of its fields do not
// implement Encode. This implementation uses wrappers to effect the encoding of
// problematic fields.
impl ::bincode::Encode for Pat {
    fn encode<E: ::bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> core::result::Result<(), ::bincode::error::EncodeError> {
        match self {
            Self::Unit => {
                <u32 as ::bincode::Encode>::encode(&(0u32), encoder)?;
                Ok(())
            }
            Self::As(field_0, field_1) => {
                <u32 as ::bincode::Encode>::encode(&(1u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                ::bincode::Encode::encode(field_1, encoder)?;
                Ok(())
            }
            Self::Product(field_0, field_1) => {
                <u32 as ::bincode::Encode>::encode(&(2u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                ::bincode::Encode::encode(field_1, encoder)?;
                Ok(())
            }
            Self::Variable(field_0) => {
                <u32 as ::bincode::Encode>::encode(&(3u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                Ok(())
            }
            Self::Constant(field_0) => {
                <u32 as ::bincode::Encode>::encode(&(4u32), encoder)?;
                ::bincode::Encode::encode(&BigIntBincode(field_0.clone()), encoder)?;
                Ok(())
            }
            Self::Nil => {
                <u32 as ::bincode::Encode>::encode(&(5u32), encoder)?;
                Ok(())
            }
            Self::Cons(field_0, field_1) => {
                <u32 as ::bincode::Encode>::encode(&(6u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                ::bincode::Encode::encode(field_1, encoder)?;
                Ok(())
            }
        }
    }
}

// Decode is manually implemented for Pattern because some of its fields do not
// implement Decode. This implementation uses wrappers to effect the decoding of
// problematic fields.
impl ::bincode::Decode for Pat {
    fn decode<D: ::bincode::de::Decoder>(
        decoder: &mut D,
    ) -> core::result::Result<Self, ::bincode::error::DecodeError> {
        let variant_index = <u32 as ::bincode::Decode>::decode(decoder)?;
        match variant_index {
            0u32 => Ok(Self::Unit {}),
            1u32 => Ok(Self::As {
                0: ::bincode::Decode::decode(decoder)?,
                1: ::bincode::Decode::decode(decoder)?,
            }),
            2u32 => Ok(Self::Product {
                0: ::bincode::Decode::decode(decoder)?,
                1: ::bincode::Decode::decode(decoder)?,
            }),
            3u32 => Ok(Self::Variable {
                0: ::bincode::Decode::decode(decoder)?,
            }),
            4u32 => Ok(Self::Constant {
                0: <BigIntBincode as ::bincode::Decode>::decode(decoder)?.0,
            }),
            5u32 => Ok(Self::Nil {}),
            6u32 => Ok(Self::Cons {
                0: ::bincode::Decode::decode(decoder)?,
                1: ::bincode::Decode::decode(decoder)?,
            }),
            variant => Err(::bincode::error::DecodeError::UnexpectedVariant {
                found: variant,
                type_name: "Pattern",
                allowed: &::bincode::error::AllowedEnumVariants::Range { min: 0, max: 4 },
            }),
        }
    }
}

impl_borrow_decode!(Pat);

#[derive(Debug, Clone, Encode, Decode)]
pub struct TExpr {
    pub v: Expr,
    pub t: Option<Type>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    Nil,
    Sequence(Vec<TExpr>),
    Product(Box<TExpr>, Box<TExpr>),
    Cons(Box<TExpr>, Box<TExpr>),
    Infix(InfixOp, Box<TExpr>, Box<TExpr>),
    Negate(Box<TExpr>),
    Application(Box<TExpr>, Box<TExpr>),
    Constant(BigInt),
    Variable(Variable),
    Function(Function),
    Intrinsic(Intrinsic),
    LetBinding(LetBinding, Box<TExpr>),
    Match(Match),
}

impl Expr {
    pub fn type_expr(self, t: Option<Type>) -> TExpr {
        let t = t.or_else(|| match &self {
            Self::Unit => Some(Type::Unit),
            Self::Sequence(exprs) => exprs.last().unwrap().t.clone(),
            Self::Cons(_expr1, expr2) => expr2.t.clone(),
            Self::Product(expr1, expr2) => expr1
                .t
                .clone()
                .zip(expr2.t.clone())
                .map(|(a, b)| Type::Product(Box::new(a), Box::new(b))),
            Self::Infix(InfixOp::Equal, _, _) => Some(Type::Unit),
            Self::Infix(_, _, _) => Some(Type::Int),
            Self::Negate(_) => Some(Type::Int),
            Self::Constant(_) => Some(Type::Int),
            Self::LetBinding(_, expr) => expr.t.clone(),
            Self::Application(_, _)
            | Self::Match(_)
            | Self::Variable(_)
            | Self::Function(_)
            | Self::Intrinsic(_)
            | Self::Nil => None,
        });
        TExpr { v: self, t }
    }
}

// Encode is manually implemented for Expr because some of its fields do not
// implement Encode. This implementation uses wrappers to effect the encoding of
// problematic fields.
impl ::bincode::Encode for Expr {
    fn encode<E: ::bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> core::result::Result<(), ::bincode::error::EncodeError> {
        match self {
            Self::Unit => {
                <u32 as ::bincode::Encode>::encode(&(0u32), encoder)?;
                Ok(())
            }
            Self::Sequence(field_0) => {
                <u32 as ::bincode::Encode>::encode(&(1u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                Ok(())
            }
            Self::Product(field_0, field_1) => {
                <u32 as ::bincode::Encode>::encode(&(2u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                ::bincode::Encode::encode(field_1, encoder)?;
                Ok(())
            }
            Self::Infix(field_0, field_1, field_2) => {
                <u32 as ::bincode::Encode>::encode(&(3u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                ::bincode::Encode::encode(field_1, encoder)?;
                ::bincode::Encode::encode(field_2, encoder)?;
                Ok(())
            }
            Self::Negate(field_0) => {
                <u32 as ::bincode::Encode>::encode(&(4u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                Ok(())
            }
            Self::Application(field_0, field_1) => {
                <u32 as ::bincode::Encode>::encode(&(5u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                ::bincode::Encode::encode(field_1, encoder)?;
                Ok(())
            }
            Self::Constant(field_0) => {
                <u32 as ::bincode::Encode>::encode(&(6u32), encoder)?;
                ::bincode::Encode::encode(&BigIntBincode(field_0.clone()), encoder)?;
                Ok(())
            }
            Self::Variable(field_0) => {
                <u32 as ::bincode::Encode>::encode(&(7u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                Ok(())
            }
            Self::Function(field_0) => {
                <u32 as ::bincode::Encode>::encode(&(8u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                Ok(())
            }
            Self::Intrinsic(field_0) => {
                <u32 as ::bincode::Encode>::encode(&(9u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                Ok(())
            }
            Self::LetBinding(field_0, field_1) => {
                <u32 as ::bincode::Encode>::encode(&(10u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                ::bincode::Encode::encode(field_1, encoder)?;
                Ok(())
            }
            Self::Match(field_0) => {
                <u32 as ::bincode::Encode>::encode(&(11u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                Ok(())
            }
            Self::Nil => {
                <u32 as ::bincode::Encode>::encode(&(12u32), encoder)?;
                Ok(())
            }
            Self::Cons(field_0, field_1) => {
                <u32 as ::bincode::Encode>::encode(&(13u32), encoder)?;
                ::bincode::Encode::encode(field_0, encoder)?;
                ::bincode::Encode::encode(field_1, encoder)?;
                Ok(())
            }
        }
    }
}

// Decode is manually implemented for Expr because some of its fields do not
// implement Decode. This implementation uses wrappers to effect the decoding of
// problematic fields.
impl ::bincode::Decode for Expr {
    fn decode<D: ::bincode::de::Decoder>(
        decoder: &mut D,
    ) -> core::result::Result<Self, ::bincode::error::DecodeError> {
        let variant_index = <u32 as ::bincode::Decode>::decode(decoder)?;
        match variant_index {
            0u32 => Ok(Self::Unit {}),
            1u32 => Ok(Self::Sequence {
                0: ::bincode::Decode::decode(decoder)?,
            }),
            2u32 => Ok(Self::Product {
                0: ::bincode::Decode::decode(decoder)?,
                1: ::bincode::Decode::decode(decoder)?,
            }),
            3u32 => Ok(Self::Infix {
                0: ::bincode::Decode::decode(decoder)?,
                1: ::bincode::Decode::decode(decoder)?,
                2: ::bincode::Decode::decode(decoder)?,
            }),
            4u32 => Ok(Self::Negate {
                0: ::bincode::Decode::decode(decoder)?,
            }),
            5u32 => Ok(Self::Application {
                0: ::bincode::Decode::decode(decoder)?,
                1: ::bincode::Decode::decode(decoder)?,
            }),
            6u32 => Ok(Self::Constant {
                0: <BigIntBincode as ::bincode::Decode>::decode(decoder)?.0,
            }),
            7u32 => Ok(Self::Variable {
                0: ::bincode::Decode::decode(decoder)?,
            }),
            8u32 => Ok(Self::Function {
                0: ::bincode::Decode::decode(decoder)?,
            }),
            9u32 => Ok(Self::Intrinsic {
                0: ::bincode::Decode::decode(decoder)?,
            }),
            10u32 => Ok(Self::LetBinding {
                0: ::bincode::Decode::decode(decoder)?,
                1: ::bincode::Decode::decode(decoder)?,
            }),
            11u32 => Ok(Self::Match {
                0: ::bincode::Decode::decode(decoder)?,
            }),
            12u32 => Ok(Self::Nil {}),
            13u32 => Ok(Self::Cons {
                0: ::bincode::Decode::decode(decoder)?,
                1: ::bincode::Decode::decode(decoder)?,
            }),
            variant => Err(::bincode::error::DecodeError::UnexpectedVariant {
                found: variant,
                type_name: "Expr",
                allowed: &::bincode::error::AllowedEnumVariants::Range { min: 0, max: 11 },
            }),
        }
    }
}

impl_borrow_decode!(Expr);

impl TExpr {
    pub fn parse(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr {
            return None;
        }
        let string = pair.as_str();
        let mut pairs = pair.into_inner();
        if string.starts_with("fun") {
            let pair = pairs.next().expect("expression should not be empty");
            Function::parse(pair).map(|x| Expr::Function(x).type_expr(None))
        } else if string.starts_with("def") {
            let pair = pairs
                .next()
                .expect("body expression should be prefixed by binding");
            let binding = LetBinding::parse(pair).expect("expression should start with binding");
            let mut body = vec![];
            while let Some(pair) = pairs.next() {
                body.push(Self::parse(pair).expect("expression should end with expression"));
            }
            if body.is_empty() {
                panic!("expression should not be empty")
            }
            Some(
                Expr::LetBinding(binding, Box::new(Expr::Sequence(body).type_expr(None)))
                    .type_expr(None),
            )
        } else {
            let pair = pairs.next().expect("expression should not be empty");
            Self::parse_expr1(pair)
        }
    }

    pub fn parse_expr1(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr1 {
            return None;
        }
        let mut pairs = pair.into_inner();
        let pair = pairs.next().expect("expression should not be empty");
        let mut exprs =
            vec![Self::parse_expr2(pair).expect("expression should start with product")];
        while let Some(pair) = pairs.next() {
            let rhs = Self::parse_expr2(pair).expect("expected RHS to be a product");
            exprs.push(rhs);
        }
        Some(if exprs.len() == 1 {
            exprs[0].clone()
        } else {
            Expr::Sequence(exprs).type_expr(None)
        })
    }

    pub fn parse_expr2(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr2 {
            return None;
        }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("expression should not be empty");
        let mut exprs = Self::parse_expr3(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next_back() {
            let rhs = Self::parse_expr3(pair).expect("expected RHS to be a product");
            exprs = Expr::Product(Box::new(rhs), Box::new(exprs)).type_expr(None);
        }
        Some(exprs)
    }

    pub fn parse_expr3(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr3 {
            return None;
        }
        let mut pairs = pair.into_inner();
        let pair = pairs.next().expect("expression should not be empty");
        let mut expr = Self::parse_expr4(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next() {
            let op = InfixOp::parse(pair).expect("expected arithmetic operator");
            let rhs_pair = pairs.next().expect("expected RHS product");
            let rhs = Self::parse_expr4(rhs_pair).expect("expected RHS to be a product");
            expr = Expr::Infix(op, Box::new(expr), Box::new(rhs)).type_expr(None);
        }
        Some(expr)
    }

    pub fn parse_expr4(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr4 {
            return None;
        }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("expression should not be empty");
        let mut exprs = Self::parse_expr5(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next_back() {
            let rhs = Self::parse_expr5(pair).expect("expected RHS to be a product");
            exprs = Expr::Cons(Box::new(rhs), Box::new(exprs)).type_expr(None);
        }
        Some(exprs)
    }

    pub fn parse_expr5(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr5 {
            return None;
        }
        let mut pairs = pair.into_inner();
        let pair = pairs.next().expect("expression should not be empty");
        let mut expr = Self::parse_expr6(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next() {
            let op = InfixOp::parse(pair).expect("expected arithmetic operator");
            let rhs_pair = pairs.next().expect("expected RHS product");
            let rhs = Self::parse_expr6(rhs_pair).expect("expected RHS to be a product");
            expr = Expr::Infix(op, Box::new(expr), Box::new(rhs)).type_expr(None);
        }
        Some(expr)
    }

    pub fn parse_expr6(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr6 {
            return None;
        }
        let mut pairs = pair.into_inner();
        let pair = pairs.next().expect("expression should not be empty");
        let mut expr = Self::parse_expr7(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next() {
            let op = InfixOp::parse(pair).expect("expected arithmetic operator");
            let rhs_pair = pairs.next().expect("expected RHS product");
            let rhs = Self::parse_expr7(rhs_pair).expect("expected RHS to be a product");
            expr = Expr::Infix(op, Box::new(expr), Box::new(rhs)).type_expr(None);
        }
        Some(expr)
    }

    pub fn parse_expr7(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr7 {
            return None;
        }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("expression should not be empty");
        let mut expr = Self::parse_expr8(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next_back() {
            let op = InfixOp::parse(pair).expect("expected arithmetic operator");
            let lhs_pair = pairs.next_back().expect("expected RHS product");
            let lhs = Self::parse_expr8(lhs_pair).expect("expected RHS to be a product");
            expr = Expr::Infix(op, Box::new(lhs), Box::new(expr)).type_expr(None);
        }
        Some(expr)
    }

    pub fn parse_expr8(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr8 {
            return None;
        }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("expression should not be empty");
        let mut expr = Self::parse_expr9(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next_back() {
            if pair.as_rule() == Rule::negate {
                expr = Expr::Negate(Box::new(expr)).type_expr(None);
            } else {
                unreachable!("only negative signs should occur here");
            }
        }
        Some(expr)
    }

    pub fn parse_expr9(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr9 {
            return None;
        }
        let mut pairs = pair.into_inner();
        let pair = pairs.next().expect("expression should not be empty");
        let mut expr = Self::parse_expr10(pair).expect("expression should start with product");
        while let Some(pair) = pairs.next() {
            let rhs = Self::parse_expr10(pair).expect("expected RHS to be a product");
            expr = Expr::Application(Box::new(expr), Box::new(rhs)).type_expr(None);
        }
        Some(expr)
    }

    pub fn parse_expr10(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::expr10 {
            return None;
        }
        let string = pair.as_str();
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("expression should not be empty");
        if pair.as_rule() == Rule::constant && string.starts_with("(") {
            Some(Expr::Unit.type_expr(None))
        } else if pair.as_rule() == Rule::constant && string.starts_with("[") {
            Some(Expr::Nil.type_expr(None))
        } else if pair.as_rule() == Rule::constant {
            let value = parse_prefixed_num(pair.as_str()).expect("constant should be an integer");
            Some(Expr::Constant(value).type_expr(None))
        } else if pair.as_rule() == Rule::valueName {
            let name = Variable::parse(pair).expect("expression should be value name");
            Some(Expr::Variable(name).type_expr(None))
        } else if string.starts_with("(")
            || string.starts_with("fun")
            || string.starts_with("def")
            || string.starts_with("match")
        {
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
                let expr = iter
                    .next()
                    .expect("sequence should contain at least one expression");
                write!(f, "{}", expr)?;
                while let Some(expr) = iter.next() {
                    write!(f, ";\n{}", expr)?;
                }
                if exprs.len() > 1 {
                    write!(f, "}}")?;
                }
            }
            Expr::Product(expr1, expr2) => write!(f, "({}, {})", expr1, expr2)?,
            Expr::Cons(expr1, expr2) => write!(f, "({}: {})", expr1, expr2)?,
            Expr::Infix(op, expr1, expr2) => write!(f, "({}{}{})", expr1, op, expr2)?,
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
            }
            Expr::Match(matche) => write!(f, "{}", matche)?,
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct Match(pub Box<TExpr>, pub Vec<TPat>, pub Vec<TExpr>);

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

#[derive(Debug, Clone, Copy, Encode, Decode, Eq, PartialEq)]
pub enum InfixOp {
    Divide,
    DivideZ,
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
        if pair.as_rule() != Rule::infixOp {
            return None;
        }
        match pair.as_span().as_str() {
            "=" => Some(Self::Equal),
            "/" => Some(Self::Divide),
            "|" => Some(Self::DivideZ),
            "*" => Some(Self::Multiply),
            "+" => Some(Self::Add),
            "-" => Some(Self::Subtract),
            "^" => Some(Self::Exponentiate),
            "\\" => Some(Self::IntDivide),
            "%" => Some(Self::Modulo),
            _ => unreachable!("Encountered unknown infix operator"),
        }
    }
}

impl fmt::Display for InfixOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Divide => write!(f, "/"),
            Self::DivideZ => write!(f, "|"),
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
        if pair.as_rule() != Rule::valueName {
            return None;
        }
        Some(Self {
            name: Some(pair.as_str().to_string()),
            id: 0,
        })
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
pub struct Function {
    pub params: Vec<TPat>,
    pub body: Box<TExpr>,
    pub env: HashMap<VariableId, TExpr>,
}

impl Function {
    pub fn parse(pair: Pair<Rule>) -> Option<Self> {
        if pair.as_rule() != Rule::function {
            return None;
        }
        let mut pairs = pair.into_inner();
        let pair = pairs.next_back().expect("function should not be empty");
        let body = TExpr::parse(pair).expect("function should end with expression");
        let mut params = vec![];
        while let Some(pair) = pairs.next() {
            let param = TPat::parse(pair).expect("all prefixes to function should be patterns");
            params.push(param);
        }
        Some(Self {
            params,
            body: Box::new(body),
            env: HashMap::default(),
        })
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fun {}", self.params[0])?;
        for pat in &self.params[1..] {
            write!(f, " {}", pat)?;
        }

        let mut body = String::new();
        write!(body, "{}", self.body)?;
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
type IntrinsicImp =
    fn(&Vec<TPat>, &HashMap<VariableId, TExpr>, &mut HashSet<VariableId>, &mut VarGen) -> TExpr;

#[derive(Clone)]
pub struct Intrinsic {
    pub pos: usize,
    imp: IntrinsicImp,
    pub params: Vec<TPat>,
    pub env: HashMap<VariableId, TExpr>,
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
            .field("pos", &self.pos)
            .field("params", &self.params)
            .field("imp", &(self.imp as fn(_, _, _, _) -> _))
            .field("env", &self.env)
            .finish()
    }
}

impl fmt::Display for Intrinsic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:p}", self.imp as fn(_, _, _, _) -> _)?;
        for arg in &self.params {
            write!(f, " {}", arg)?;
        }
        Ok(())
    }
}

impl Intrinsic {
    pub fn new(params: Vec<TPat>, imp: IntrinsicImp) -> Self {
        Self {
            imp,
            params,
            pos: 0,
            env: HashMap::new(),
        }
    }

    pub fn execute(
        &self,
        bindings: &HashMap<VariableId, TExpr>,
        prover_defs: &mut HashSet<VariableId>,
        gen: &mut VarGen,
    ) -> TExpr {
        (self.imp)(&self.params, bindings, prover_defs, gen)
    }
}
