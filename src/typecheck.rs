use crate::ast::{
    Definition, Expr, Function, InfixOp, Intrinsic, LetBinding, Module, Pat, TExpr, TPat, Variable,
    VariableId,
};
use crate::error::Error;
use crate::qprintln;
use crate::transform::{collect_pattern_variables, VarGen};
use crate::util::Config;
use bincode::{Decode, Encode};
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Display};

/* Collect the free variables occuring in the given type. */
fn collect_free_type_vars(typ: &Type, map: &mut HashMap<VariableId, Variable>) {
    match typ {
        Type::Int | Type::Unit => {}
        Type::Variable(var) => {
            map.insert(var.id, var.clone());
        }
        Type::List(typ1) => {
            collect_free_type_vars(typ1, map);
        }
        Type::Function(typ1, typ2) | Type::Product(typ1, typ2) => {
            collect_free_type_vars(typ1, map);
            collect_free_type_vars(typ2, map);
        }
        Type::Forall(var, typ2) => {
            if map.contains_key(&var.id) {
                collect_free_type_vars(typ2, map);
            } else {
                collect_free_type_vars(typ2, map);
                map.remove(&var.id);
            }
        }
    }
}

/* Generate a unique type variables for each pattern. */
fn allocate_pat_types(pat: &mut TPat, gen: &mut VarGen) {
    let new_var = gen.generate_id();
    pat.t = Some(Type::Variable(Variable::new(new_var)));

    match &mut pat.v {
        Pat::Product(pat1, pat2) | Pat::Cons(pat1, pat2) => {
            allocate_pat_types(pat1, gen);
            allocate_pat_types(pat2, gen);
        }
        Pat::As(pat1, _) => {
            allocate_pat_types(pat1, gen);
        }
        Pat::Constant(_) | Pat::Variable(_) | Pat::Unit | Pat::Nil => {}
    }
}

/* Generate a unique type variables for each expression. */
fn allocate_expr_types(expr: &mut TExpr, gen: &mut VarGen) {
    let new_var = gen.generate_id();
    expr.t = Some(Type::Variable(Variable::new(new_var)));

    match &mut expr.v {
        Expr::Sequence(exprs) => {
            for expr in exprs {
                allocate_expr_types(expr, gen);
            }
        }
        Expr::Infix(_, expr1, expr2)
        | Expr::Application(expr1, expr2)
        | Expr::Product(expr1, expr2)
        | Expr::Cons(expr1, expr2) => {
            allocate_expr_types(expr1, gen);
            allocate_expr_types(expr2, gen);
        }
        Expr::Negate(expr1) => {
            allocate_expr_types(expr1, gen);
        }
        Expr::Function(fun) => {
            for param in &mut fun.params {
                allocate_pat_types(param, gen);
            }
            allocate_expr_types(&mut *fun.body, gen);
        }
        Expr::LetBinding(binding, body) => {
            allocate_pat_types(&mut binding.0, gen);
            allocate_expr_types(&mut *binding.1, gen);
            allocate_expr_types(body, gen);
        }
        Expr::Match(matche) => {
            allocate_expr_types(&mut matche.0, gen);
            for pat1 in &mut matche.1 {
                allocate_pat_types(pat1, gen);
            }
            for expr2 in &mut matche.2 {
                allocate_expr_types(expr2, gen);
            }
        }
        Expr::Constant(_) | Expr::Variable(_) | Expr::Unit | Expr::Intrinsic(_) | Expr::Nil => {}
    }
}

/* Generate unique type variables for everything within this definition. */
fn allocate_def_types(def: &mut Definition, gen: &mut VarGen) {
    allocate_pat_types(&mut def.0 .0, gen);
    allocate_expr_types(&mut *def.0 .1, gen);
}

/* Generate unique type variables for everything within the given module. */
pub fn allocate_module_types(module: &mut Module, gen: &mut VarGen) {
    for def in &mut module.defs {
        allocate_def_types(def, gen);
    }
    for expr in &mut module.exprs {
        allocate_expr_types(expr, gen);
    }
}

/* A representation of expression types. */
#[derive(Debug, Clone, Encode, Decode)]
pub enum Type {
    Unit,
    Int,
    Variable(Variable),
    List(Box<Type>),
    Function(Box<Type>, Box<Type>),
    Product(Box<Type>, Box<Type>),
    Forall(Variable, Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "()"),
            Type::Int => write!(f, "int"),
            Type::List(a) => write!(f, "[{}]", a),
            Type::Variable(var) => write!(f, "{}", var),
            Type::Function(a, b) => write!(f, "({} -> {})", a, b),
            Type::Product(a, b) => write!(f, "({}, {})", a, b),
            Type::Forall(var, b) => write!(f, "forall {}. {}", var, b),
        }
    }
}

/* Get or generate the type variable associated with a given expression. */
pub fn expr_type_var(expr: &TExpr) -> &Type {
    expr.t.as_ref().unwrap()
}

/* Check if the given variable occurs in the type expression. */
fn occurs_in(
    var1: &Variable,
    type2: &Type,
    types: &mut HashMap<VariableId, Type>,
) -> Result<bool, Error> {
    match type2 {
        Type::Variable(var2) if var1.id == var2.id => Ok(true),
        Type::Variable(var2) if types.contains_key(&var2.id) => {
            occurs_in(var1, &types[&var2.id].clone(), types)
        }
        Type::Variable(_) | Type::Int | Type::Unit => Ok(false),
        Type::List(a) => occurs_in(var1, a, types),
        Type::Function(a, b) | Type::Product(a, b) => {
            Ok(occurs_in(var1, a, types)? || occurs_in(var1, b, types)?)
        }
        Type::Forall(_, _) => Err(Error::OccursCheckError),
    }
}

/* Unify the type variable with the given type. */
fn unify_variable(
    var1: &Variable,
    type2: &Type,
    types: &mut HashMap<VariableId, Type>,
    inserts: &mut Option<HashSet<VariableId>>,
) -> Result<(), Error> {
    match (var1, type2) {
        (var1, Type::Variable(var2)) if var1.id == var2.id => Ok(()),
        (var1, type2) if types.contains_key(&var1.id) => {
            unify_types(&types[&var1.id].clone(), type2, types, inserts)?;
            Ok(())
        }
        (var1, Type::Variable(var2)) if types.contains_key(&var2.id) => {
            unify_types(
                &Type::Variable(var1.clone()),
                &types[&var2.id].clone(),
                types,
                inserts,
            )?;
            Ok(())
        }
        (var1, type2) if !occurs_in(var1, type2, types)? => {
            types.insert(var1.id, type2.clone());
            if let Some(x) = inserts {
                x.insert(var1.id);
            }
            Ok(())
        }
        _ => Err(Error::VariableTypeError {
            v: var1.clone(),
            t: type2.clone(),
        }),
    }
}

/* Unify the two given types together. */
pub fn unify_types(
    type1: &Type,
    type2: &Type,
    types: &mut HashMap<VariableId, Type>,
    inserts: &mut Option<HashSet<VariableId>>,
) -> Result<(), Error> {
    match (type1, type2) {
        (Type::Int, Type::Int) | (Type::Unit, Type::Unit) => Ok(()),
        (Type::List(a), Type::List(b)) => unify_types(a, b, types, inserts),
        (Type::Function(a1, b1), Type::Function(a2, b2))
        | (Type::Product(a1, b1), Type::Product(a2, b2)) => {
            unify_types(&*a1, &*a2, types, inserts)?;
            unify_types(&*b1, &*b2, types, inserts)?;
            Ok(())
        }
        (Type::Variable(v1), type2) | (type2, Type::Variable(v1)) => {
            unify_variable(v1, type2, types, inserts)
        }
        _ => Err(Error::TypeError {
            t1: type1.clone(),
            t2: type2.clone(),
        }),
    }
}

/* Fully expand the variables in the given type. */
pub fn expand_type(typ: &Type, types: &HashMap<VariableId, Type>) -> Type {
    match typ {
        Type::Unit => Type::Unit,
        Type::Int => Type::Int,
        Type::Variable(var) if types.contains_key(&var.id) => expand_type(&types[&var.id], types),
        Type::Variable(_) => typ.clone(),
        Type::List(a) => Type::List(Box::new(expand_type(a, types))),
        Type::Function(a, b) => Type::Function(
            Box::new(expand_type(a, types)),
            Box::new(expand_type(b, types)),
        ),
        Type::Product(a, b) => Type::Product(
            Box::new(expand_type(a, types)),
            Box::new(expand_type(b, types)),
        ),
        Type::Forall(var, b) => Type::Forall(var.clone(), Box::new(expand_type(b, types))),
    }
}

/* Expand the given type until either it is no longer a variable or until there
 * are no more available expansions. */
fn partial_expand_type(typ: &Type, types: &HashMap<VariableId, Type>) -> Type {
    let mut typ = typ;
    while let Type::Variable(var) = typ {
        if let Some(inner) = types.get(&var.id) {
            typ = inner;
        } else {
            break;
        }
    }
    typ.clone()
}

/* Consistently replace all quantified type variables occuring in type
 * expression with fresh ones. Useful for let polymorphism. */
fn instantiate_type_vars(
    typ: &mut Type,
    map: &mut HashMap<VariableId, VariableId>,
    gen: &mut VarGen,
) {
    match typ {
        Type::Int | Type::Unit => {}
        Type::Variable(var) => {
            if let Some(target) = map.get(&var.id) {
                var.id = *target;
            }
        }
        Type::List(a) => {
            instantiate_type_vars(a, map, gen);
        }
        Type::Function(a, b) | Type::Product(a, b) => {
            instantiate_type_vars(a, map, gen);
            instantiate_type_vars(b, map, gen);
        }
        Type::Forall(var, b) => {
            map.insert(var.id, gen.generate_id());
            instantiate_type_vars(b, map, gen);
            *typ = *b.clone();
        }
    }
}

/* Strip all type variables occuring in the given pattern. */
pub fn strip_pat_types(pat: &mut TPat) {
    pat.t = None;
    match &mut pat.v {
        Pat::As(pat1, _) => {
            strip_pat_types(pat1);
        }
        Pat::Product(pat1, pat2) | Pat::Cons(pat1, pat2) => {
            strip_pat_types(pat1);
            strip_pat_types(pat2);
        }
        Pat::Constant(_) | Pat::Unit | Pat::Variable(_) | Pat::Nil => {}
    }
}

/* Strip all type variables occuring in the given expression. */
pub fn strip_expr_types(expr: &mut TExpr) {
    expr.t = None;
    match &mut expr.v {
        Expr::Sequence(exprs) => {
            for expr in exprs {
                strip_expr_types(expr);
            }
        }
        Expr::Infix(_, expr1, expr2)
        | Expr::Application(expr1, expr2)
        | Expr::Product(expr1, expr2)
        | Expr::Cons(expr1, expr2) => {
            strip_expr_types(expr1);
            strip_expr_types(expr2);
        }
        Expr::Match(matche) => {
            strip_expr_types(&mut matche.0);
            for (_, expr2) in matche.1.iter_mut().zip(matche.2.iter_mut()) {
                strip_expr_types(expr2);
            }
        }
        Expr::Negate(expr) => {
            strip_expr_types(expr);
        }
        Expr::Constant(_) | Expr::Unit | Expr::Variable(_) | Expr::Nil => {}
        Expr::Intrinsic(Intrinsic { env, .. }) => {
            for val in env.values_mut() {
                strip_expr_types(val);
            }
        }
        Expr::Function(Function { body, env, .. }) => {
            strip_expr_types(body);
            for val in env.values_mut() {
                strip_expr_types(val);
            }
        }
        Expr::LetBinding(binding, expr) => {
            strip_expr_types(&mut binding.1);
            strip_expr_types(expr);
        }
    }
}

/* Strip all type variables occuring in the given definition. */
pub fn strip_def_types(def: &mut Definition) {
    strip_pat_types(&mut def.0 .0);
    strip_expr_types(&mut def.0 .1);
}

/* Strip all type variables occuring in the given module. */
pub fn strip_module_types(module: &mut Module) {
    for def in &mut module.defs {
        strip_def_types(def);
    }
    for expr in &mut module.exprs {
        strip_expr_types(expr);
    }
}

/* Infer the principle type of the given binding's expression and make a type
 * scheme that quantifies all free variables found in the expression that
 * cannot also be found in the environment. Finally, extend the environment with
 * the derived type schemes. */
fn infer_binding_types(
    def: &LetBinding,
    env_ftvs: &mut HashMap<VariableId, Variable>,
    vars: &mut HashMap<VariableId, Type>,
    types: &mut HashMap<VariableId, Type>,
    gen: &mut VarGen,
) {
    let expr1_var = expr_type_var(&*def.1);
    infer_expr_types(&*def.1, env_ftvs, vars, types, gen);
    infer_pat_types(&def.0, vars, types, gen);
    unify_types(
        pat_type_var(&def.0),
        expr_type_var(&def.1),
        types,
        &mut None,
    )
    .unwrap();
    // Compute the set of free variables occuring in RHS' TYPE that
    // do not occur in the type environment
    let mut quant_vars = HashMap::new();
    collect_free_type_vars(&expand_type(expr1_var, types), &mut quant_vars);
    for env_ftv in env_ftvs.keys() {
        quant_vars.remove(env_ftv);
    }
    // Quantify the type of each variable bound by the pattern with the
    // free variables unique to the RHS
    let mut pat_vars = HashMap::new();
    collect_pattern_variables(&def.0, &mut pat_vars);
    for pat_var in pat_vars.keys() {
        let quant_expr = vars.get_mut(&pat_var).unwrap();
        // Quantify every free variable unique to the RHS' type
        for qvar in quant_vars.values() {
            *quant_expr = Type::Forall(qvar.clone(), Box::new(quant_expr.clone()));
        }
        // Add this type schema to the type environment in which the let
        // body is type-checked
        let quant_expr = quant_expr.clone();
        collect_free_type_vars(&expand_type(&quant_expr, types), env_ftvs);
    }
}

/* Get or generate the type variable associated with a given pattern. */
pub fn pat_type_var(pat: &TPat) -> &Type {
    pat.t.as_ref().unwrap()
}

/* Assign each variable in the pattern an explicit type, even if that means
 * expanding upon parts of the given type. */
pub fn infer_pat_types(
    pat: &TPat,
    vars: &mut HashMap<VariableId, Type>,
    types: &mut HashMap<VariableId, Type>,
    gen: &mut VarGen,
) {
    match &pat.v {
        Pat::Nil => {
            let pat_var = pat_type_var(pat);
            let poly_var = Type::Variable(Variable::new(gen.generate_id()));
            // []: [a]
            unify_types(pat_var, &Type::List(Box::new(poly_var)), types, &mut None).unwrap();
        }
        Pat::Unit => {
            let pat_var = pat_type_var(pat);
            // (): ()
            unify_types(pat_var, &Type::Unit, types, &mut None).unwrap();
        }
        Pat::Constant(_) => {
            let pat_var = pat_type_var(pat);
            // num: int
            unify_types(pat_var, &Type::Int, types, &mut None).unwrap();
        }
        Pat::Variable(var) => {
            let pat_var = pat_type_var(pat);
            // Map the pattern name to its type
            vars.insert(var.id, pat_var.clone());
        }
        Pat::As(pat1, name) => {
            let pat1_var = pat_type_var(pat1);
            let pat_var = pat_type_var(pat);
            // a1: t1 |- a1 as _: t1
            unify_types(&pat_var, &pat1_var, types, &mut None).unwrap();
            infer_pat_types(&pat1, vars, types, gen);
            // Map the pattern name to its type
            vars.insert(name.id, pat_var.clone());
        }
        Pat::Product(pat1, pat2) => {
            let pat1_var = pat_type_var(pat1);
            let pat2_var = pat_type_var(pat2);
            let pat_var = pat_type_var(pat);
            // a1: t1, a2: t2 |- (a1, a2): (t1, tN)
            unify_types(
                &pat_var,
                &Type::Product(Box::new(pat1_var.clone()), Box::new(pat2_var.clone())),
                types,
                &mut None,
            )
            .unwrap();
            infer_pat_types(pat1, vars, types, gen);
            infer_pat_types(pat2, vars, types, gen);
        }
        Pat::Cons(pat1, pat2) => {
            let pat1_var = pat_type_var(pat1);
            let pat2_var = pat_type_var(pat2);
            let pat_var = pat_type_var(pat);
            // a1: t1, a2: [t1] |- (a1:a2): [t1]
            unify_types(
                &pat2_var,
                &Type::List(Box::new(pat1_var.clone())),
                types,
                &mut None,
            )
            .unwrap();
            unify_types(&pat_var, &pat2_var, types, &mut None).unwrap();
            infer_pat_types(pat1, vars, types, gen);
            infer_pat_types(pat2, vars, types, gen);
        }
    }
}

/* Recursively infer the types of expressions in the given expression tree.
 * Works by repeatedly generating and solving equations in the given typing
 * context. */
fn infer_expr_types(
    expr: &TExpr,
    env: &HashMap<VariableId, Variable>,
    vars: &HashMap<VariableId, Type>,
    types: &mut HashMap<VariableId, Type>,
    gen: &mut VarGen,
) {
    match &expr.v {
        Expr::Nil => {
            let expr_var = expr_type_var(expr);
            let poly_var = Type::Variable(Variable::new(gen.generate_id()));
            // []: [a]
            unify_types(expr_var, &Type::List(Box::new(poly_var)), types, &mut None).unwrap();
        }
        Expr::Unit => {
            let expr_var = expr_type_var(expr);
            // (): ()
            unify_types(expr_var, &Type::Unit, types, &mut None).unwrap();
        }
        Expr::Constant(_) => {
            let expr_var = expr_type_var(expr);
            // num: int
            unify_types(expr_var, &Type::Int, types, &mut None).unwrap();
        }
        Expr::Infix(InfixOp::Equal, expr1, expr2) => {
            let expr_var = expr_type_var(expr);
            let expr1_var = expr_type_var(expr1);
            let expr2_var = expr_type_var(expr2);
            // a = b: ()
            unify_types(&expr_var, &Type::Unit, types, &mut None).unwrap();
            // a: c |- b: c
            unify_types(&expr1_var, &expr2_var, types, &mut None).unwrap();
            infer_expr_types(expr1, env, vars, types, gen);
            infer_expr_types(expr2, env, vars, types, gen);
        }
        Expr::Infix(
            InfixOp::Add
            | InfixOp::Subtract
            | InfixOp::Multiply
            | InfixOp::Divide
            | InfixOp::DivideZ
            | InfixOp::Exponentiate
            | InfixOp::IntDivide
            | InfixOp::Modulo,
            expr1,
            expr2,
        ) => {
            let expr_var = expr_type_var(expr);
            let expr1_var = expr_type_var(expr1);
            let expr2_var = expr_type_var(expr2);
            // a op b: int
            unify_types(&expr_var, &Type::Int, types, &mut None).unwrap();
            // a: int
            unify_types(&expr1_var, &Type::Int, types, &mut None).unwrap();
            // b: int
            unify_types(&expr2_var, &Type::Int, types, &mut None).unwrap();
            infer_expr_types(expr1, env, vars, types, gen);
            infer_expr_types(expr2, env, vars, types, gen);
        }
        Expr::Negate(expr1) => {
            let expr_var = expr_type_var(expr);
            let expr1_var = expr_type_var(expr1);
            // (-a): int
            unify_types(&expr_var, &Type::Int, types, &mut None).unwrap();
            // a: int
            unify_types(&expr1_var, &Type::Int, types, &mut None).unwrap();
            infer_expr_types(expr1, env, vars, types, gen);
        }
        Expr::Sequence(seq) => {
            let last_expr = seq.last().expect("encountered empty sequence");
            let expr_var = expr_type_var(expr);
            let last_expr_var = expr_type_var(last_expr);
            // aN: c |- (a1; ...; aN): c
            unify_types(&expr_var, &last_expr_var, types, &mut None).unwrap();
            for expr in seq {
                infer_expr_types(expr, env, vars, types, gen);
            }
        }
        Expr::Product(expr1, expr2) => {
            let expr1_var = expr_type_var(expr1);
            let expr2_var = expr_type_var(expr2);
            let expr_var = expr_type_var(expr);
            // a1: t1, a2: t2 |- (a1, a2): (t1, t2)
            unify_types(
                &expr_var,
                &Type::Product(Box::new(expr1_var.clone()), Box::new(expr2_var.clone())),
                types,
                &mut None,
            )
            .unwrap();
            infer_expr_types(expr1, env, vars, types, gen);
            infer_expr_types(expr2, env, vars, types, gen);
        }
        Expr::Cons(expr1, expr2) => {
            let expr1_var = expr_type_var(expr1);
            let expr2_var = expr_type_var(expr2);
            let expr_var = expr_type_var(expr);
            // a1: t1, a2: [t1] |- (a1:a2): [t1]
            unify_types(
                &expr2_var,
                &Type::List(Box::new(expr1_var.clone())),
                types,
                &mut None,
            )
            .unwrap();
            unify_types(&expr_var, &expr2_var, types, &mut None).unwrap();
            infer_expr_types(expr1, env, vars, types, gen);
            infer_expr_types(expr2, env, vars, types, gen);
        }
        Expr::Application(expr1, expr2) => {
            let expr_var = expr_type_var(expr);
            let expr1_var = expr_type_var(expr1);
            let expr2_var = expr_type_var(expr2);
            // b: t, a b: u |- a: t -> u
            unify_types(
                &expr1_var,
                &Type::Function(Box::new(expr2_var.clone()), Box::new(expr_var.clone())),
                types,
                &mut None,
            )
            .unwrap();
            infer_expr_types(expr1, env, vars, types, gen);
            infer_expr_types(expr2, env, vars, types, gen);
        }
        Expr::Function(Function {
            params,
            body: expr1,
            ..
        }) => {
            let expr_var = expr_type_var(expr);
            let expr1_var = expr_type_var(expr1);
            let mut func_var = expr1_var.clone();
            let mut env = env.clone();
            let mut vars = vars.clone();
            for param in params.iter().rev() {
                infer_pat_types(param, &mut vars, types, gen);
                let param_type = pat_type_var(param);
                collect_free_type_vars(&expand_type(&param_type, types), &mut env);
                func_var = Type::Function(Box::new(param_type.clone()), Box::new(func_var));
            }
            // a1: t1, ..., aN: tN |- b: u
            // fun a1 ... aN -> b : t1 -> ... -> tN -> u
            unify_types(&expr_var, &func_var, types, &mut None).unwrap();
            infer_expr_types(expr1, &env, &vars, types, gen);
        }
        Expr::Match(matche) => {
            let expr_var = expr_type_var(expr);
            let expr1_var = expr_type_var(&matche.0);
            for (pat, expr2) in matche.1.iter().zip(matche.2.iter()) {
                let mut vars = vars.clone();
                let mut env = env.clone();
                infer_pat_types(pat, &mut vars, types, gen);
                let pat_type = pat_type_var(pat);
                unify_types(&pat_type, &expr1_var, types, &mut None).unwrap();
                let expr2_var = expr_type_var(expr2);
                unify_types(&expr_var, &expr2_var, types, &mut None).unwrap();
                collect_free_type_vars(&expand_type(&pat_type, types), &mut env);
                infer_expr_types(expr2, &env, &vars, types, gen);
            }
        }
        Expr::Intrinsic(Intrinsic { params, .. }) => {
            let expr_var = expr_type_var(expr);
            let mut vars = vars.clone();
            let mut func_var = Type::Variable(Variable::new(gen.generate_id()));
            for param in params.iter().rev() {
                infer_pat_types(param, &mut vars, types, gen);
                let param_type = pat_type_var(param);
                func_var = Type::Function(Box::new(param_type.clone()), Box::new(func_var));
            }
            unify_types(&func_var, &expr_var, types, &mut None).unwrap();
        }
        Expr::LetBinding(def, expr2) => {
            let expr_var = expr_type_var(expr);
            let expr2_var = expr_type_var(expr2);
            let mut env = env.clone();
            let mut vars = vars.clone();
            infer_binding_types(def, &mut env, &mut vars, types, gen);
            unify_types(&expr_var, &expr2_var, types, &mut None).unwrap();
            infer_expr_types(expr2, &env, &vars, types, gen);
        }
        Expr::Variable(var) => {
            let expr_var = expr_type_var(expr);
            let mut fresh = expand_type(&vars[&var.id], types);
            let mut new_map = HashMap::new();
            instantiate_type_vars(&mut fresh, &mut new_map, gen);
            unify_types(&expr_var, &fresh, types, &mut None).unwrap();
        }
    }
}

/* Infer the type of the definition bindings and its contained sub-expressions.
 */
fn infer_def_types(
    def: &Definition,
    env: &mut HashMap<VariableId, Variable>,
    vars: &mut HashMap<VariableId, Type>,
    types: &mut HashMap<VariableId, Type>,
    gen: &mut VarGen,
) {
    infer_binding_types(&def.0, env, vars, types, gen);
}

/* Type check the module using Hindley Milner. */
pub fn infer_module_types(
    annotated: &mut Module,
    globals: &HashMap<String, VariableId>,
    vars: &mut HashMap<VariableId, Type>,
    types: &mut HashMap<VariableId, Type>,
    gen: &mut VarGen,
) {
    allocate_module_types(annotated, gen);
    let mut env = HashMap::new();
    // Initialize the type environment with the types of global variables
    for (name, id) in globals {
        if !vars.contains_key(id) {
            let mut var = Variable::new(gen.generate_id());
            var.name = Some(name.clone());
            vars.insert(*id, Type::Variable(var));
        }
    }
    for typ in vars.values() {
        collect_free_type_vars(typ, &mut env);
    }
    for def in &mut annotated.defs {
        infer_def_types(def, &mut env, vars, types, gen);
    }
    for expr in &mut annotated.exprs {
        infer_expr_types(expr, &env, vars, types, gen);
    }
}

/* Expand tuple pattern variables into tuple patterns. */
pub fn expand_pattern_variables(
    pat: &mut TPat,
    expr: &TExpr,
    map: &mut HashMap<VariableId, Option<TPat>>,
    gen: &mut VarGen,
) -> Result<(), Error> {
    match (&mut pat.v, &expr.v) {
        (Pat::Variable(var), _) if map.contains_key(&var.id) => {
            if let Some(equiv_pat) = &map[&var.id] {
                *pat = equiv_pat.clone();
            }
            Ok(())
        }
        (Pat::Variable(var), Expr::Product(expr1, expr2)) => {
            let mut new_var1 = Variable::new(gen.generate_id());
            new_var1.name = var.name.as_ref().map(|x| x.to_owned() + ".0");
            let mut var1 = Pat::Variable(new_var1).type_pat(None);
            expand_pattern_variables(&mut var1, &expr1, map, gen)?;

            let mut new_var2 = Variable::new(gen.generate_id());
            new_var2.name = var.name.as_ref().map(|x| x.to_owned() + ".1");
            let mut var2 = Pat::Variable(new_var2).type_pat(None);
            expand_pattern_variables(&mut var2, &expr2, map, gen)?;

            let curr_id = var.id;
            pat.v = Pat::Product(Box::new(var1), Box::new(var2));
            map.insert(curr_id, Some(pat.clone()));

            Ok(())
        }

        (Pat::Variable(var), Expr::Cons(expr1, expr2)) => {
            let mut new_var1 = Variable::new(gen.generate_id());
            new_var1.name = var.name.as_ref().map(|x| x.to_owned() + ".0");
            let mut var1 = Pat::Variable(new_var1).type_pat(expr1.t.clone());
            expand_pattern_variables(&mut var1, &expr1, map, gen)?;

            let mut new_var2 = Variable::new(gen.generate_id());
            new_var2.name = var.name.as_ref().map(|x| x.to_owned() + ".1");
            let mut var2 = Pat::Variable(new_var2).type_pat(expr2.t.clone());
            expand_pattern_variables(&mut var2, &expr2, map, gen)?;

            let curr_id = var.id;
            pat.v = Pat::Cons(Box::new(var1), Box::new(var2));
            map.insert(curr_id, Some(pat.clone()));

            Ok(())
        }
        (Pat::Variable(var), Expr::Unit) => {
            let equiv_pat = Pat::Unit.type_pat(Some(Type::Unit));
            map.insert(var.id, Some(equiv_pat.clone()));
            *pat = equiv_pat;
            Ok(())
        }
        (Pat::Variable(var), Expr::Nil) => {
            let equiv_pat = Pat::Nil.type_pat(None);
            map.insert(var.id, Some(equiv_pat.clone()));
            *pat = equiv_pat;
            Ok(())
        }
        (Pat::Variable(var), _) => {
            map.insert(var.id, None);
            Ok(())
        }
        (Pat::Product(pat1, pat2), Expr::Product(expr1, expr2)) => {
            expand_pattern_variables(pat1, &expr1, map, gen)?;
            expand_pattern_variables(pat2, &expr2, map, gen)?;
            Ok(())
        }
        (Pat::Cons(pat1, pat2), Expr::Cons(expr1, expr2)) => {
            expand_pattern_variables(pat1, &expr1, map, gen)?;
            expand_pattern_variables(pat2, &expr2, map, gen)?;
            Ok(())
        }
        (Pat::Constant(_), _) => Ok(()),
        (Pat::Unit, _) => Ok(()),
        (Pat::Nil, Expr::Nil) => Ok(()),
        (Pat::As(pat1, _name), _) => {
            expand_pattern_variables(pat1, expr, map, gen)?;
            Ok(())
        }
        _ => Err(Error::PatternMatchError {
            e: expr.clone(),
            p: pat.clone(),
        }),
    }
}

/* Expand tuple variables into tuple expressions using the available type
 * information. */
pub fn expand_expr_variables(
    expr: &mut TExpr,
    map: &mut HashMap<VariableId, TExpr>,
    types: &HashMap<VariableId, Type>,
    gen: &mut VarGen,
) -> Result<(), Error> {
    let typ = partial_expand_type(expr_type_var(expr), types);
    match (&mut expr.v, typ) {
        (Expr::Variable(var), _) if map.contains_key(&var.id) => {
            *expr = map[&var.id].clone();
            Ok(())
        }
        (Expr::Variable(var), Type::Product(typ1, typ2)) => {
            let mut new_var1 = Variable::new(gen.generate_id());
            new_var1.name = var.name.as_ref().map(|x| x.to_owned() + ".0");
            let mut var1 = Expr::Variable(new_var1).type_expr(Some(*typ1.clone()));
            expand_expr_variables(&mut var1, map, types, gen)?;

            let mut new_var2 = Variable::new(gen.generate_id());
            new_var2.name = var.name.as_ref().map(|x| x.to_owned() + ".1");
            let mut var2 = Expr::Variable(new_var2).type_expr(Some(*typ2.clone()));
            expand_expr_variables(&mut var2, map, types, gen)?;

            let curr_id = var.id;
            expr.v = Expr::Product(Box::new(var1), Box::new(var2));
            map.insert(curr_id, expr.clone());
            Ok(())
        }
        (Expr::Variable(var), Type::Unit) => {
            map.insert(var.id, Expr::Unit.type_expr(Some(Type::Unit)));
            *expr = map[&var.id].clone();
            Ok(())
        }
        (Expr::Variable(_), Type::Int) => Ok(()),
        (Expr::Variable(var), Type::Function(_, _)) => {
            Err(Error::UndefinedGlobalFunction { v: var.clone() })
        }
        (Expr::Variable(var), Type::List(_)) => Err(Error::UndefinedGlobalList { v: var.clone() }),
        (Expr::Variable(var), Type::Variable(_)) => {
            Err(Error::UnableDetermineType { v: var.clone() })
        }
        (Expr::Product(expr1, expr2), _) => {
            expand_expr_variables(expr1, map, types, gen)?;
            expand_expr_variables(expr2, map, types, gen)?;
            Ok(())
        }
        (Expr::Constant(_), Type::Int) | (Expr::Unit, Type::Unit) => Ok(()),
        _ => Err(Error::ImpossibleType {
            e: expr.clone(),
            t: expand_type(expr_type_var(expr), types),
        }),
    }
}

/* Print out the types of top-level program definitions. */
pub fn print_types(module: &Module, types: &HashMap<VariableId, Type>, config: &Config) {
    for def in &module.defs {
        if let Some(typ) = &def.0 .1.t {
            qprintln!(config, "{}: {}", def.0 .0, expand_type(typ, types));
        }
    }
}
