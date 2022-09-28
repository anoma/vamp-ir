use std::fmt::{self, Display};
use crate::ast::{Module, VariableId, Pattern, Variable, TExpr, InfixOp, Function, Definition, Expr, LetBinding, Intrinsic};
use crate::transform::{VarGen, collect_pattern_variables};
use std::collections::HashMap;
use bincode::{Decode, Encode};

/* Collect the free variables occuring in the given type. */
fn collect_free_type_vars(
    typ: &Type,
    map: &mut HashMap<VariableId, Variable>,
) {
    match typ {
        Type::Int | Type::Unit => {},
        Type::Variable(var) => {
            map.insert(var.id, var.clone());
        },
        Type::Function(typ1, typ2) | Type::Product(typ1, typ2) => {
            collect_free_type_vars(typ1, map);
            collect_free_type_vars(typ2, map);
        },
        Type::Forall(var, typ2) => {
            if map.contains_key(&var.id) {
                collect_free_type_vars(typ2, map);
            } else {
                collect_free_type_vars(typ2, map);
                map.remove(&var.id);
            }
        },
    }
}

/* Generate a unique type variables for each expression. */
fn allocate_expr_types(
    expr: &mut TExpr,
    gen: &mut VarGen,
) {
    let new_var = gen.generate_id();
    expr.t = Some(Type::Variable(Variable::new(new_var)));
    
    match &mut expr.v {
        Expr::Sequence(exprs) | Expr::Intrinsic(Intrinsic { args: exprs, .. }) => {
            for expr in exprs {
                allocate_expr_types(expr, gen);
            }
        },
        Expr::Infix(_, expr1, expr2) | Expr::Application(expr1, expr2) |
        Expr::Product(expr1, expr2) => {
            allocate_expr_types(expr1, gen);
            allocate_expr_types(expr2, gen);
        },
        Expr::Negate(expr1) => {
            allocate_expr_types(expr1, gen);
        },
        Expr::Function(fun) => {
            allocate_expr_types(&mut *fun.1, gen);
        },
        Expr::LetBinding(binding, body) => {
            allocate_expr_types(&mut *binding.1, gen);
            allocate_expr_types(body, gen);
        },
        Expr::Match(matche) => {
            allocate_expr_types(&mut matche.0, gen);
            for expr2 in &mut matche.2 {
                allocate_expr_types(expr2, gen);
            }
        },
        Expr::Constant(_) | Expr::Variable(_) | Expr::Unit => {},
    }
}

/* Generate unique type variables for everything within this definition. */
fn allocate_def_types(
    def: &mut Definition,
    gen: &mut VarGen,
) {
    allocate_expr_types(&mut *def.0.1, gen);
}

/* Generate unique type variables for everything within the given module. */
pub fn allocate_module_types(
    module: &mut Module,
    gen: &mut VarGen,
) {
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
    Function(Box<Type>, Box<Type>),
    Product(Box<Type>, Box<Type>),
    Forall(Variable, Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "()"),
            Type::Int => write!(f, "int"),
            Type::Variable(var) => write!(f, "{}", var),
            Type::Function(a, b) => write!(f, "({} -> {})", a, b),
            Type::Product(a, b) => write!(f, "({}, {})", a, b),
            Type::Forall(var, b) =>
                write!(f, "forall {}. {}", var, b),
        }
    }
}

/* Get or generate the type variable associated with a given expression. */
fn expr_type_var(expr: &TExpr) -> &Type {
    expr.t.as_ref().unwrap()
}

/* Generate the type of expressions the given pattern can match. */
fn pattern_type(pat: &Pattern) -> Type {
    match pat {
        Pattern::Unit => Type::Unit,
        Pattern::Constant(_) => Type::Int,
        Pattern::Variable(var) => Type::Variable(var.clone()),
        Pattern::As(pat, _name) => pattern_type(pat),
        Pattern::Product(pat1, pat2) => {
            Type::Product(
                Box::new(pattern_type(pat1)),
                Box::new(pattern_type(pat2)),
            )
        }
    }
}

/* Assign each variable in the pattern an explicit type, even if that means
 * expanding upon parts of the given type. */
fn type_pattern(
    pat: &Pattern,
    typ: Type,
    types: &mut HashMap<VariableId, Type>,
    gen: &mut VarGen,
) {
    match (pat, typ) {
        (pat, Type::Variable(var)) if types.contains_key(&var.id) =>
            type_pattern(pat, types[&var.id].clone(), types, gen),
        (Pattern::Constant(_), typ) =>
            unify_types(&typ, &Type::Int, types),
        (Pattern::Variable(var), typ) => {
            types.insert(var.id, typ.clone());
        },
        (Pattern::As(pat, name), typ) => {
            types.insert(name.id, typ.clone());
            type_pattern(&pat, typ, types, gen)
        },
        (Pattern::Product(pat1, pat2), Type::Product(typ1, typ2)) => {
            type_pattern(pat1, *typ1.clone(), types, gen);
            type_pattern(pat2, *typ2.clone(), types, gen);
        },
        (Pattern::Product(pat1, pat2), Type::Variable(var)) => {
            types.insert(var.id, Type::Product(
                Box::new(Type::Variable(Variable::new(gen.generate_id()))),
                Box::new(Type::Variable(Variable::new(gen.generate_id()))),
            ));
            type_pattern(
                &Pattern::Product(pat1.clone(), pat2.clone()),
                Type::Variable(var),
                types,
                gen,
            );
        },
        (pat, typ) => panic!("unable to type pattern {} as {}", pat, typ),
    }
}

/* Check if the given variable occurs in the type expression. */
fn occurs_in(
    var1: &Variable,
    type2: &Type,
    types: &mut HashMap<VariableId, Type>,
) -> bool {
    match type2 {
        Type::Variable(var2) if var1.id == var2.id => true,
        Type::Variable(var2) if types.contains_key(&var2.id) =>
            occurs_in(var1, &types[&var2.id].clone(), types),
        Type::Variable(_) | Type::Int | Type::Unit => false,
        Type::Function(a, b) | Type::Product(a, b) =>
            occurs_in(var1, a, types) || occurs_in(var1, b, types),
        Type::Forall(_, _) =>
            panic!("universally quantified types cannot be occurs checked"),
    }
}

/* Unify the type variable with the given type. */
fn unify_variable(
    var1: &Variable,
    type2: &Type,
    types: &mut HashMap<VariableId, Type>,
) {
    match (var1, type2) {
        (var1, Type::Variable(var2)) if var1.id == var2.id => {},
        (var1, type2) if types.contains_key(&var1.id) =>
            unify_types(&types[&var1.id].clone(), type2, types),
        (var1, Type::Variable(var2)) if types.contains_key(&var2.id) =>
            unify_types(&Type::Variable(var1.clone()), &types[&var2.id].clone(), types),
        (var1, type2) if !occurs_in(var1, type2, types) => {
            types.insert(var1.id, type2.clone());
        }
        _ => panic!("unable to match {:?} with {}", var1, type2),
    }
}

/* Unify the two given types together. */
fn unify_types(
    type1: &Type,
    type2: &Type,
    types: &mut HashMap<VariableId, Type>,
) {
    match (type1, type2) {
        (Type::Int, Type::Int) |
        (Type::Unit, Type::Unit) => {},
        (Type::Function(a1, b1), Type::Function(a2, b2)) |
        (Type::Product(a1, b1), Type::Product(a2, b2)) => {
            unify_types(&*a1, &*a2, types);
            unify_types(&*b1, &*b2, types);
        },
        (Type::Variable(v1), type2) | (type2, Type::Variable(v1)) =>
            unify_variable(v1, type2, types),
        _ => panic!("unable to match {} with {}", type1, type2),
    }
}

/* Fully expand the variables in the given type. */
fn expand_type(
    typ: &Type,
    types: &HashMap<VariableId, Type>,
) -> Type {
    match typ {
        Type::Unit => Type::Unit,
        Type::Int => Type::Int,
        Type::Variable(var) if types.contains_key(&var.id) =>
            expand_type(&types[&var.id], types),
        Type::Variable(_) => typ.clone(),
        Type::Function(a, b) => Type::Function(
            Box::new(expand_type(a, types)),
            Box::new(expand_type(b, types))
        ),
        Type::Product(a, b) => Type::Product(
            Box::new(expand_type(a, types)),
            Box::new(expand_type(b, types))
        ),
        Type::Forall(var, b) => Type::Forall(
            var.clone(),
            Box::new(expand_type(b, types)),
        ),
    }
}

/* Expand the given type until either it is no longer a variable or until there
 * are no more available expansions. */
fn partial_expand_type(
    typ: &Type,
    types: &HashMap<VariableId, Type>,
) -> Type {
    let mut typ = typ;
    while let Type::Variable(var) = typ {
        if let Some(inner) = types.get(&var.id) {
            typ = inner;
        } else {
            break
        }
    }
    typ.clone()
}

/* Consistently replace all quantified type variables occuring in type
 * expression with fresh ones. Useful for let polymorphism. */
fn instantiate_type_vars(
    typ: &mut Type,
    map: &mut HashMap<VariableId, VariableId>,
    gen: &mut VarGen
) {
    match typ {
        Type::Int | Type::Unit => {},
        Type::Variable(var) => {
            if let Some(target) = map.get(&var.id) {
                var.id = *target;
            }
        },
        Type::Function(a, b) | Type::Product(a, b) => {
            instantiate_type_vars(a, map, gen);
            instantiate_type_vars(b, map, gen);
        },
        Type::Forall(var, b) => {
            map.insert(var.id, gen.generate_id());
            instantiate_type_vars(b, map, gen);
            *typ = *b.clone();
        },
    }
}

/* Infer the principle type of the given binding's expression and make a type
 * scheme that quantifies all free variables found in the expression that
 * cannot also be found in the environment. Finally, extend the environment with
 * the derived type schemes. */
fn infer_binding_types(
    def: &LetBinding,
    env: &mut Vec<Type>,
    types: &mut HashMap<VariableId, Type>,
    gen: &mut VarGen,
) {
    let expr1_var = expr_type_var(&*def.1);
    infer_expr_types(&*def.1, env, types, gen);
    type_pattern(&def.0, expr1_var.clone(), types, gen);
    // Collect all free variables occuring in the environment
    let mut env_ftvs = HashMap::new();
    for env_typ in env.iter() {
        collect_free_type_vars(&expand_type(env_typ, types), &mut env_ftvs);
    }
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
        let quant_expr = types.get_mut(&pat_var).unwrap();
        // Quantify every free variable unique to the RHS' type
        for qvar in quant_vars.values() {
            *quant_expr = Type::Forall(qvar.clone(), Box::new(quant_expr.clone()));
        }
        // Add this type schema to the type environment in which the let
        // body is type-checked
        env.push(quant_expr.clone());
    }
}

/* Recursively infer the types of expressions in the given expression tree.
 * Works by repeatedly generating and solving equations in the given typing
 * context. */
fn infer_expr_types(
    expr: &TExpr,
    env: &Vec<Type>,
    types: &mut HashMap<VariableId, Type>,
    gen: &mut VarGen,
) {
    match &expr.v {
        Expr::Unit => {
            let expr_var = expr_type_var(expr);
            // (): ()
            unify_types(expr_var, &Type::Unit, types);
        },
        Expr::Constant(_) => {
            let expr_var = expr_type_var(expr);
            // num: int
            unify_types(expr_var, &Type::Int, types);
        },
        Expr::Infix(InfixOp::Equal, expr1, expr2) => {
            let expr_var = expr_type_var(expr);
            let expr1_var = expr_type_var(expr1);
            let expr2_var = expr_type_var(expr2);
            // a = b: ()
            unify_types(&expr_var, &Type::Unit, types);
            // a: c |- b: c
            unify_types(&expr1_var, &expr2_var, types);
            infer_expr_types(expr1, env, types, gen);
            infer_expr_types(expr2, env, types, gen);
        },
        Expr::Infix(
            InfixOp::Add | InfixOp::Subtract | InfixOp::Multiply |
            InfixOp::Divide | InfixOp::Exponentiate | InfixOp::IntDivide |
            InfixOp::Modulo,
            expr1,
            expr2
        ) => {
            let expr_var = expr_type_var(expr);
            let expr1_var = expr_type_var(expr1);
            let expr2_var = expr_type_var(expr2);
            // a op b: int
            unify_types(&expr_var, &Type::Int, types);
            // a: int
            unify_types(&expr1_var, &Type::Int, types);
            // b: int
            unify_types(&expr2_var, &Type::Int, types);
            infer_expr_types(expr1, env, types, gen);
            infer_expr_types(expr2, env, types, gen);
        },
        Expr::Negate(expr1) => {
            let expr_var = expr_type_var(expr);
            let expr1_var = expr_type_var(expr1);
            // (-a): int
            unify_types(&expr_var, &Type::Int, types);
            // a: int
            unify_types(&expr1_var, &Type::Int, types);
            infer_expr_types(expr1, env, types, gen);
        },
        Expr::Sequence(seq) => {
            let last_expr = seq.last().expect("encountered empty sequence");
            let expr_var = expr_type_var(expr);
            let last_expr_var = expr_type_var(last_expr);
            // aN: c |- (a1; ...; aN): c
            unify_types(&expr_var, &last_expr_var, types);
            for expr in seq {
                infer_expr_types(expr, env, types, gen);
            }
        },
        Expr::Product(expr1, expr2) => {
            let expr1_var = expr_type_var(expr1);
            let expr2_var = expr_type_var(expr2);
            let expr_var = expr_type_var(expr);
            // a1: t1, ... aN: tN |- (a1, ..., aN): (t1, ..., tN)
            unify_types(
                &expr_var,
                &Type::Product(Box::new(expr1_var.clone()), Box::new(expr2_var.clone())),
                types
            );
            infer_expr_types(expr1, env, types, gen);
            infer_expr_types(expr2, env, types, gen);
        },
        Expr::Application(expr1, expr2) => {
            let expr_var = expr_type_var(expr);
            let expr1_var = expr_type_var(expr1);
            let expr2_var = expr_type_var(expr2);
            // b: t, a b: u |- a: t -> u
            unify_types(
                &expr1_var,
                &Type::Function(
                    Box::new(expr2_var.clone()),
                    Box::new(expr_var.clone())
                ),
                types
            );
            infer_expr_types(expr1, env, types, gen);
            infer_expr_types(expr2, env, types, gen);
        },
        Expr::Function(Function(params, expr1)) => {
            let expr_var = expr_type_var(expr);
            let expr1_var = expr_type_var(expr1);
            let mut func_var = expr1_var.clone();
            let mut env = env.clone();
            for param in params.iter().rev() {
                let param_var = pattern_type(param);
                env.push(param_var.clone());
                func_var = Type::Function(Box::new(param_var), Box::new(func_var));
            }
            // a1: t1, ..., aN: tN |- b: u
            // fun a1 ... aN -> b : t1 -> ... -> tN -> u
            unify_types(&expr_var, &func_var, types);
            infer_expr_types(expr1, &env, types, gen);
        },
        Expr::Match(matche) => {
            let expr_var = expr_type_var(expr);
            let expr1_var = expr_type_var(&matche.0);
            for (pat, expr2) in matche.1.iter().zip(matche.2.iter()) {
                let pat_var = pattern_type(pat);
                unify_types(&pat_var, &expr1_var, types);
                let expr2_var = expr_type_var(expr2);
                unify_types(&expr_var, &expr2_var, types);
            }
            for expr2 in matche.2.iter() {
                infer_expr_types(expr2, &env, types, gen);
            }
        },
        Expr::Intrinsic(Intrinsic { args, imp_typ, ..}) => {
            let expr_var = expr_type_var(expr);
            let mut func_var = expr_var.clone();
            for arg in args.iter().rev() {
                let arg_var = expr_type_var(arg);
                func_var = Type::Function(Box::new(arg_var.clone()), Box::new(func_var));
            }
            // b: t, a b: u |- a: t -> u
            unify_types(&func_var, &imp_typ, types);
            for arg in args {
                infer_expr_types(arg, &env, types, gen);
            }
        },
        Expr::LetBinding(def, expr2) => {
            let expr_var = expr_type_var(expr);
            let expr2_var = expr_type_var(expr2);
            let mut env = env.clone();
            infer_binding_types(def, &mut env, types, gen);
            unify_types(&expr_var, &expr2_var, types);
            infer_expr_types(expr2, &env, types, gen);
        },
        Expr::Variable(var) => {
            let expr_var = expr_type_var(expr);
            let mut fresh = expand_type(&Type::Variable(var.clone()), types);
            let mut new_map = HashMap::new();
            instantiate_type_vars(&mut fresh, &mut new_map, gen);
            unify_types(
                &expr_var,
                &fresh,
                types
            );
        },
    }
}

/* Infer the type of the definition bindings and its contained sub-expressions.
 */
fn infer_def_types(
    def: &Definition,
    env: &mut Vec<Type>,
    types: &mut HashMap<VariableId, Type>,
    gen: &mut VarGen,
) {
    infer_binding_types(&def.0, env, types, gen);
}

/* Type check the module using Hindley Milner. */
pub fn infer_module_types(
    annotated: &mut Module,
    globals: &HashMap<String, VariableId>,
    types: &mut HashMap<VariableId, Type>,
    gen: &mut VarGen,
) {
    allocate_module_types(annotated, gen);
    let mut env = Vec::new();
    // Initialize the type environment with the types of global variables
    for (name, id) in globals {
        let mut var = Variable::new(*id);
        var.name = Some(name.clone());
        env.push(Type::Variable(var));
    }
    for def in &mut annotated.defs {
        infer_def_types(def, &mut env, types, gen);
    }
    for expr in &mut annotated.exprs {
        infer_expr_types(expr, &env, types, gen);
    }
}

/* Expand tuple pattern variables into tuple patterns. */
fn expand_pattern_variables(
    pat: &mut Pattern,
    typ: &Type,
    map: &mut HashMap<VariableId, Pattern>,
    gen: &mut VarGen,
) {
    match (&mut *pat, typ) {
        (Pattern::Variable(var), _) if map.contains_key(&var.id) => {
            *pat = map[&var.id].clone();
        },
        (Pattern::Variable(var), Type::Product(typ1, typ2)) => {
            let mut new_var1 = Variable::new(gen.generate_id());
            new_var1.name = var
                .name
                .as_ref()
                .map(|x| x.to_owned() + ".0");
            let mut var1 = Pattern::Variable(new_var1);
            expand_pattern_variables(&mut var1, typ1, map, gen);
            
            let mut new_var2 = Variable::new(gen.generate_id());
            new_var2.name = var
                .name
                .as_ref()
                .map(|x| x.to_owned() + ".1");
            let mut var2 = Pattern::Variable(new_var2);
            expand_pattern_variables(&mut var2, typ2, map, gen);
            
            let new_pat = Pattern::Product(Box::new(var1), Box::new(var2));
            map.insert(var.id, new_pat.clone());
            *pat = new_pat;
        },
        (Pattern::Variable(_), _) => {},
        (Pattern::Product(pat1, pat2), Type::Product(typ1, typ2)) => {
            expand_pattern_variables(pat1, typ1, map, gen);
            expand_pattern_variables(pat2, typ2, map, gen);
        },
        (Pattern::Constant(_), Type::Int) => {},
        (Pattern::Unit, Type::Unit) => {},
        (Pattern::As(_pat, _name), typ) => {
            expand_pattern_variables(pat, typ, map, gen);
        },
        _ => panic!("pattern {} cannot have type {}", pat, typ),
    }
}

/* Expand tuple expression variables into tuple expressions. */
fn expand_variables(
    expr: &mut TExpr,
    map: &mut HashMap<VariableId, Pattern>,
    types: &HashMap<VariableId, Type>,
    gen: &mut VarGen,
) {
    match &mut expr.v {
        Expr::LetBinding(binding, body) => {
            expand_variables(&mut *binding.1, map, types, gen);
            let typ = expand_type(binding.1.t.as_ref().unwrap(), types);
            expand_pattern_variables(&mut binding.0, &typ, map, gen);
            expand_variables(body, map, types, gen);
        },
        Expr::Sequence(exprs) | Expr::Intrinsic(Intrinsic { args: exprs, .. }) => {
            for expr in exprs {
                expand_variables(expr, map, types, gen);
            }
        },
        Expr::Match(matche) => {
            expand_variables(&mut matche.0, map, types, gen);
            let pat_typ = expand_type(matche.0.t.as_ref().unwrap(), types);
            for (pat, expr2) in matche.1.iter_mut().zip(matche.2.iter_mut()) {
                expand_pattern_variables(pat, &pat_typ, map, gen);
                expand_variables(expr2, map, types, gen);
            }
        },
        Expr::Infix(_, expr1, expr2) | Expr::Application(expr1, expr2) |
        Expr::Product(expr1, expr2) => {
            expand_variables(expr1, map, types, gen);
            expand_variables(expr2, map, types, gen);
        },
        Expr::Negate(expr1) => {
            expand_variables(expr1, map, types, gen);
        },
        Expr::Variable(var) if map.contains_key(&var.id) => {
            let typ = expand_type(expr.t.as_ref().unwrap(), types);
            *expr = map[&var.id].to_typed_expr(typ);
        },
        Expr::Variable(var) if expr.t.is_some() => {
            let expanded_type = expand_type(expr.t.as_ref().unwrap(), types);
            if let Type::Product(typ1, typ2) = expanded_type {
                let mut new_var1 = Variable::new(gen.generate_id());
                new_var1.name = var
                    .name
                    .as_ref()
                    .map(|x| x.to_owned() + ".0");
                let var_pat1 = Pattern::Variable(new_var1);
                let mut var_expr1 = var_pat1.to_typed_expr(*typ1);
                expand_variables(&mut var_expr1, map, types, gen);
                
                let mut new_var2 = Variable::new(gen.generate_id());
                new_var2.name = var
                    .name
                    .as_ref()
                    .map(|x| x.to_owned() + ".1");
                let var_pat2 = Pattern::Variable(new_var2);
                let mut var_expr2 = var_pat2.to_typed_expr(*typ2);
                expand_variables(&mut var_expr2, map, types, gen);
                
                map.insert(var.id, Pattern::Product(
                    Box::new(var_pat1),
                    Box::new(var_pat2),
                ));
                *expr = TExpr {
                    v: Expr::Product(Box::new(var_expr1), Box::new(var_expr2)),
                    t: expr.t.clone(),
                };
            }
        },
        Expr::Function(fun) => {
            expand_variables(&mut fun.1, map, types, gen);
        },
        Expr::Constant(_) | Expr::Variable(_) | Expr::Unit => {},
    }
}

/* Expand tuple variables occuring in given definition into tuples. */
fn expand_def_variables(
    def: &mut Definition,
    map: &mut HashMap<VariableId, Pattern>,
    types: &mut HashMap<VariableId, Type>,
    gen: &mut VarGen,
) {
    expand_variables(&mut *def.0.1, map, types, gen);
    let typ = expand_type(def.0.1.t.as_ref().unwrap(), types);
    expand_pattern_variables(&mut def.0.0, &typ, map, gen);
}

/* Expand tuple variables occuring in given module into tuples. */
pub fn expand_module_variables(
    module: &mut Module,
    types: &mut HashMap<VariableId, Type>,
    gen: &mut VarGen,
) {
    let mut pattern_map = HashMap::new();
    for def in &mut module.defs {
        expand_def_variables(def, &mut pattern_map, types, gen);
    }
    for expr in &mut module.exprs {
        expand_variables(expr, &mut pattern_map, types, gen);
    }
}

/* Returns the given type with all function inner types are replaced by units.
 */
fn unitize_type_functions(
    typ: Type,
    types: &mut HashMap<VariableId, Type>,
) -> Type {
    match &typ {
        Type::Variable(var) if types.contains_key(&var.id) => {
            let res = unitize_type_functions(types[&var.id].clone(), types);
            types.insert(var.id, res);
            typ
        }
        Type::Variable(_) | Type::Int | Type::Unit => typ,
        Type::Function(_, _) => Type::Unit,
        Type::Product(typ1, typ2) => {
            Type::Product(
                Box::new(unitize_type_functions(*typ1.clone(), types)),
                Box::new(unitize_type_functions(*typ2.clone(), types)),
            )
        },
        Type::Forall(var, b) => Type::Forall(
            var.clone(),
            Box::new(unitize_type_functions(*b.clone(), types)),
        ),
    }
}

/* Takes a pattern and its type. Returns a pattern where pattern variables
 * corresponding to function types are replaced by units. */
fn unitize_pattern_functions(
    pat: Pattern,
    typ: &Type,
    types: &HashMap<VariableId, Type>,
) -> Pattern {
    match (pat, typ) {
        (pat, Type::Variable(var)) if types.contains_key(&var.id) => {
            unitize_pattern_functions(pat, &types[&var.id], types)
        },
        (Pattern::Variable(_), Type::Function(_, _)) => Pattern::Unit,
        (Pattern::As(inner_pat, name), _) => {
            let inner_pat = unitize_pattern_functions(*inner_pat, typ, types);
            Pattern::As(Box::new(inner_pat), name.clone())
        },
        (Pattern::Product(pat1, pat2), Type::Product(typ1, typ2)) => {
            Pattern::Product(
                Box::new(unitize_pattern_functions(*pat1, typ1, types)),
                Box::new(unitize_pattern_functions(*pat2, typ2, types)),
            )
        },
        (pat @ Pattern::Variable(_), Type::Variable(_)) => pat,
        (pat @ (Pattern::Constant(_) | Pattern::Variable(_)), Type::Int) => pat,
        (pat @ (Pattern::Unit | Pattern::Variable(_)), Type::Unit) => pat,
        (pat, typ) =>
            panic!("pattern {} does not correspond to type {}", pat, typ),
    }
}

/* Replace all functions occuring in the given expression with 0-tuples. */
fn unitize_expr_functions(
    expr: &mut TExpr,
    types: &mut HashMap<VariableId, Type>,
) {
    match &mut expr.v {
        Expr::Function(_) | Expr::Intrinsic(_) => {
            expr.v = Expr::Unit;
        },
        Expr::Sequence(exprs) => {
            for expr in exprs {
                unitize_expr_functions(expr, types);
            }
        },
        Expr::Match(matche) => {
            unitize_expr_functions(&mut matche.0, types);
            let pat_type = matche.0.t.as_ref().unwrap();
            for (pat, expr2) in matche.1.iter_mut().zip(matche.2.iter_mut()) {
                *pat = unitize_pattern_functions(pat.clone(), pat_type, types);
                unitize_expr_functions(expr2, types);
            }
        },
        Expr::Infix(_, expr1, expr2) | Expr::Application(expr1, expr2) |
        Expr::Product(expr1, expr2)=> {
            unitize_expr_functions(expr1, types);
            unitize_expr_functions(expr2, types);
        },
        Expr::LetBinding(binding, body) => {
            let binding_type = binding.1.t.as_ref().unwrap();
            binding.0 = unitize_pattern_functions(binding.0.clone(), binding_type, types);
            unitize_expr_functions(&mut *binding.1, types);
            unitize_expr_functions(body, types);
        },
        Expr::Negate(expr1) => unitize_expr_functions(expr1, types),
        Expr::Constant(_) | Expr::Unit => {},
        Expr::Variable(_) => {
            let partial_type = expr.t.as_ref().map(|x| partial_expand_type(x, types));
            if let Some(Type::Function(_, _)) = partial_type {
                expr.v = Expr::Unit;
            }
        },
    }
    // Scrub functions from this expression's type
    expr.t = expr.t.clone().map(|x| unitize_type_functions(x, types));
}

/* Replace all functions occuring in the given definition with 0-tuples. */
fn unitize_def_functions(
    def: &mut Definition,
    types: &mut HashMap<VariableId, Type>,
) {
    let binding_type = def.0.1.t.as_ref().unwrap();
    def.0.0 = unitize_pattern_functions(def.0.0.clone(), binding_type, types);
    unitize_expr_functions(&mut *def.0.1, types);
}

/* Replace all functions occuring in the given module with 0-tuples. */
pub fn unitize_module_functions(
    module: &mut Module,
    types: &mut HashMap<VariableId, Type>,
) {
    for def in &mut module.defs {
        unitize_def_functions(def, types);
    }
    for expr in &mut module.exprs {
        unitize_expr_functions(expr, types);
    }
}

/* Print out the types of top-level program definitions. */
pub fn print_types(module: &Module, types: &HashMap<VariableId, Type>) {
    for def in &module.defs {
        if let Some(typ) = &def.0.1.t {
            println!("{}: {}", def.0.0, expand_type(typ, types));
        }
    }
}
