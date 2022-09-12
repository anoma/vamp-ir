use std::collections::HashMap;
use crate::typecheck::{infer_module_types, expand_module_variables, unitize_module_functions};
use crate::ast::{Module, Definition, TExpr, Pattern, VariableId, LetBinding, Variable, InfixOp, Expr};

/* A structure for generating unique variable IDs. */
pub struct VarGen(VariableId);

impl VarGen {
    pub fn new() -> Self {
        VarGen(0)
    }
    pub fn generate_id(&mut self) -> VariableId {
        let curr_id = self.0;
        self.0 += 1;
        curr_id
    }
}

/* Replaces variable IDs in the given expression according to the given
 * substitution map. */
fn refresh_expr_variables(
    expr: &mut TExpr,
    map: &HashMap<VariableId, VariableId>,
    gen: &mut VarGen,
) {
    match &mut expr.v {
        Expr::Sequence(exprs) => {
            for expr in exprs {
                refresh_expr_variables(expr, map, gen);
            }
        },
        Expr::Product(exprs) => {
            for expr in exprs {
                refresh_expr_variables(expr, map, gen);
            }
        },
        Expr::Infix(_, expr1, expr2) => {
            refresh_expr_variables(expr1, map, gen);
            refresh_expr_variables(expr2, map, gen);
        },
        Expr::Negate(expr) => {
            refresh_expr_variables(expr, map, gen);
        },
        Expr::Application(expr1, expr2) => {
            refresh_expr_variables(expr1, map, gen);
            refresh_expr_variables(expr2, map, gen);
        },
        Expr::Constant(_) => {},
        Expr::Variable(var) => {
            if let Some(id) = map.get(&var.id) {
                var.id = *id;
            }
        },
        Expr::Function(fun) => {
            let mut map = map.clone();
            for param in &mut fun.0 {
                refresh_pattern_variables(param, &mut map, gen);
            }
            refresh_expr_variables(&mut fun.1, &map, gen);
        },
        Expr::LetBinding(binding, expr) => {
            refresh_expr_variables(&mut binding.1, map, gen);
            let mut map = map.clone();
            refresh_pattern_variables(&mut binding.0, &mut map, gen);
            refresh_expr_variables(expr, &map, gen);
        },
    }
}

/* Match the given expression against the given pattern. */
fn match_pattern_expr(
    pat: &Pattern,
    expr: &TExpr,
    map: &mut HashMap<VariableId, TExpr>,
    gen: &mut VarGen,
) {
    match (pat, &expr.v) {
        (pat, Expr::Variable(var)) if map.contains_key(&var.id) =>
            match_pattern_expr(pat, &map[&var.id].clone(), map, gen),
        (Pattern::As(pat, var), _) => {
            match_pattern_expr(pat, expr, map, gen);
            map.insert(var.id, expr.clone().into());
        },
        (Pattern::Variable(var), expr) => {
            map.insert(var.id, expr.clone().into());
        },
        (Pattern::Product(pats), Expr::Product(exprs))
            if pats.len() == exprs.len() =>
        {
            for (pat, expr) in pats.iter().zip(exprs.iter()) {
                match_pattern_expr(pat, expr, map, gen);
            }
        },
        (Pattern::Product(pats), Expr::Variable(var)) => {
            let mut inner_exprs = vec![];
            for _ in pats {
                let new_var = Variable::new(gen.generate_id());
                inner_exprs.push(Expr::Variable(new_var).into());
            }
            map.insert(var.id, Expr::Product(inner_exprs).into());
            match_pattern_expr(pat, expr, map, gen);
        },
        (Pattern::Constant(a), Expr::Constant(b)) if a == b => {},
        _ => panic!("unable to match {} against {}", expr, pat),
    }
}

/* Refresh all the variables occuring in the given pattern. */
fn refresh_pattern_variables(
    pat: &mut Pattern,
    map: &mut HashMap<VariableId, VariableId>,
    gen: &mut VarGen,
) {
    match pat {
        Pattern::As(pat, var) => {
            refresh_pattern_variables(pat, map, gen);
            map.insert(var.id, gen.generate_id());
            var.id = map[&var.id];
        },
        Pattern::Product(pats) => {
            for pat in pats {
                refresh_pattern_variables(pat, map, gen);
            }
        },
        Pattern::Variable(var) => {
            map.insert(var.id, gen.generate_id());
            var.id = map[&var.id];
        },
        Pattern::Constant(_) => {},
    }
}

/* Gives each variable occuring in the pattern a unique ID. Assumes that no
 * variable is used more than once in the pattern. */
fn number_pattern_variables(
    pat: &mut Pattern,
    map: &mut HashMap<String, VariableId>,
    gen: &mut VarGen,
) {
    match pat {
        Pattern::As(pat, var) => {
            number_pattern_variables(pat, map, gen);
            if let Some(name) = &var.name {
                var.id = gen.generate_id();
                map.insert(name.clone(), var.id);
            }
        },
        Pattern::Product(pats) => {
            for pat in pats {
                number_pattern_variables(pat, map, gen);
            }
        },
        Pattern::Variable(var) => {
            if let Some(name) = &var.name {
                var.id = gen.generate_id();
                map.insert(name.clone(), var.id);
            }
        },
        Pattern::Constant(_) => {},
    }
}

/* Numbers each variable occuring in the expression according to the binding in
 * local. If there is no such binding, then the global variable map is searched.
 * If not found, then a new global variable binding is made. Binding expressions
 * introduce new local bindings. */
fn number_expr_variables(
    expr: &mut TExpr,
    locals: &HashMap<String, VariableId>,
    globals: &mut HashMap<String, VariableId>,
    gen: &mut VarGen,
) {
    match &mut expr.v {
        Expr::Sequence(exprs) => {
            for expr in exprs {
                number_expr_variables(expr, locals, globals, gen);
            }
        },
        Expr::Product(exprs) => {
            for expr in exprs {
                number_expr_variables(expr, locals, globals, gen);
            }
        },
        Expr::Infix(_, expr1, expr2) => {
            number_expr_variables(expr1, locals, globals, gen);
            number_expr_variables(expr2, locals, globals, gen);
        },
        Expr::Negate(expr) => {
            number_expr_variables(expr, locals, globals, gen);
        },
        Expr::Application(expr1, expr2) => {
            number_expr_variables(expr1, locals, globals, gen);
            number_expr_variables(expr2, locals, globals, gen);
        },
        Expr::Constant(_) => {},
        Expr::Variable(var) => {
            if let Some(name) = &var.name {
                if let Some(id) = locals.get(name) {
                    var.id = *id;
                } else if let Some(id) = globals.get(name) {
                    var.id = *id;
                } else {
                    var.id = gen.generate_id();
                    globals.insert(name.clone(), var.id);
                }
            }
        },
        Expr::Function(fun) => {
            let mut locals = locals.clone();
            for param in &mut fun.0 {
                number_pattern_variables(param, &mut locals, gen);
            }
            number_expr_variables(&mut fun.1, &mut locals, globals, gen);
        },
        Expr::LetBinding(binding, expr) => {
            let mut locals = locals.clone();
            number_expr_variables(&mut binding.1, &mut locals, globals, gen);
            number_pattern_variables(&mut binding.0, &mut locals, gen);
            number_expr_variables(expr, &mut locals, globals, gen);
        },
    }
}

/* Numbers the variables occuring in the definition. Essentially numbers the
 * inner expression, and then numbers the definition pattern variables in global
 * scope. */
fn number_def_variables(
    def: &mut Definition,
    locals: &mut HashMap<String, VariableId>,
    globals: &mut HashMap<String, VariableId>,
    gen: &mut VarGen,
) {
    number_expr_variables(&mut *def.0.1, locals, globals, gen);
    number_pattern_variables(&mut def.0.0, locals, gen);
}

/* Numbers the variables occuring in the module definitions and then those
 * occuring in the module expressions. */
pub fn number_module_variables(
    module: &mut Module,
    globals: &mut HashMap<String, VariableId>,
    gen: &mut VarGen,
) {
    let mut locals = HashMap::new();
    for def in &mut module.defs {
        number_def_variables(def, &mut locals, globals, gen);
    }
    for expr in &mut module.exprs {
        number_expr_variables(expr, &locals, globals, gen);
    }
}

/* Replace each function application occuring in the expression with a let
 * binding containing an inlined body. Returns a normal form of the expression.
 */
fn apply_functions(
    expr: &mut TExpr,
    bindings: &HashMap<VariableId, TExpr>,
    gen: &mut VarGen,
) -> TExpr {
    match &mut expr.v {
        Expr::Application(expr1, expr2) => {
            match &mut expr1.v {
                Expr::Application(_, _) => {
                    apply_functions(expr1, bindings, gen);
                    apply_functions(expr, bindings, gen)
                },
                Expr::Variable(var) => {
                    if let Some(val) = bindings.get(&var.id) {
                        *expr = Expr::Application(Box::new(val.clone()), expr2.clone()).into();
                        apply_functions(expr, bindings, gen)
                    } else {
                        panic!("encountered unbound variable {}", var)
                    }
                },
                Expr::Function(_) => {
                    let substitutions = HashMap::new();
                    refresh_expr_variables(expr1, &substitutions, gen);
                    if let Expr::Function(fun) = &mut expr1.v {
                        if fun.0.is_empty() {
                            unreachable!("functions should have at least one parameter");
                        }
                        let arg1 = fun.0.remove(0);
                        let new_body = if fun.0.is_empty() { &*fun.1 } else { expr1 };
                        let new_bind = LetBinding(arg1, expr2.clone());
                        *expr = Expr::LetBinding(new_bind, Box::new(new_body.clone())).into();
                        apply_functions(expr, bindings, gen)
                    } else {
                        unreachable!("refreshing variables changed expression type")
                    }
                },
                Expr::Sequence(seq) => {
                    if let Some(val) = seq.last_mut() {
                        *val = Expr::Application(Box::new(val.clone()), expr2.clone()).into();
                        *expr = *expr1.clone();
                        apply_functions(expr, bindings, gen)
                    } else {
                        unreachable!("encountered an empty sequence")
                    }
                },
                Expr::LetBinding(_, body) => {
                    *body = Box::new(Expr::Application(body.clone(), expr2.clone()).into());
                    *expr = *expr1.clone();
                    apply_functions(expr, bindings, gen)
                },
                Expr::Infix(_, _, _) => {
                    panic!("cannot apply argument {} to infix {}", expr2, expr1)
                },
                Expr::Negate(_) => {
                    panic!("cannot apply argument {} to negation {}", expr2, expr1)
                },
                Expr::Product(_) => {
                    panic!("cannot apply argument {} to tuple {}", expr2, expr1)
                },
                Expr::Constant(_) => {
                    panic!("cannot apply argument {} to constant {}", expr2, expr1)
                },
            }
        },
        Expr::LetBinding(binding, body) => {
            let val = apply_functions(&mut *binding.1, bindings, gen);
            let mut bindings = bindings.clone();
            match_pattern_expr(&binding.0, &val, &mut bindings, gen);
            apply_functions(body, &bindings, gen)
        },
        Expr::Sequence(seq) => {
            let mut val = None;
            for expr in seq {
                val = Some(apply_functions(expr, &bindings, gen));
            }
            val.expect("encountered empty sequence")
        },
        Expr::Product(prod) => {
            let mut vals = vec![];
            for expr in prod {
                vals.push(apply_functions(expr, &bindings, gen));
            }
            Expr::Product(vals).into()
        },
        Expr::Infix(op, expr1, expr2) => {
            let expr1 = apply_functions(expr1, &bindings, gen);
            let expr2 = apply_functions(expr2, &bindings, gen);
            Expr::Infix(op.clone(), Box::new(expr1), Box::new(expr2)).into()
        },
        Expr::Negate(expr1) => {
            Expr::Negate(Box::new(apply_functions(expr1, &bindings, gen))).into()
        },
        Expr::Constant(val) => Expr::Constant(*val).into(),
        Expr::Variable(var) => {
            if let Some(val) = bindings.get(&var.id) {
                val.clone()
            } else {
                expr.clone()
            }
        },
        Expr::Function(fun) => Expr::Function(fun.clone()).into(),
    }
}

/* Replace each function application occuring in the definition with a let
 * binding containing an inlined body. */
fn apply_def_functions(
    def: &mut Definition,
    bindings: &mut HashMap<VariableId, TExpr>,
    gen: &mut VarGen,
) {
    let val = apply_functions(&mut def.0.1, bindings, gen);
    match_pattern_expr(&def.0.0, &val, bindings, gen);
}

/* Replace each function application occuring in the module with a let
 * binding containing an inlined body. */
pub fn apply_module_functions(
    module: &mut Module,
    gen: &mut VarGen,
) {
    let mut bindings = HashMap::new();
    for def in &mut module.defs {
        apply_def_functions(def, &mut bindings, gen);
    }
    for expr in &mut module.exprs {
        apply_functions(expr, &mut bindings, gen);
    }
}

/* Collect all the variables occuring in the given pattern. */
pub fn collect_pattern_variables(
    pat: &Pattern,
    map: &mut HashMap<VariableId, Variable>,
) {
    match pat {
        Pattern::Variable(var) => {
            map.insert(var.id, var.clone());
        },
        Pattern::As(pat, var) => {
            map.insert(var.id, var.clone());
            collect_pattern_variables(pat, map);
        },
        Pattern::Product(prod) => {
            for pat in prod {
                collect_pattern_variables(pat, map);
            }
        },
        Pattern::Constant(_) => {}
    }
}

/* Collect all the variables occuring in the given expression. */
fn collect_expr_variables(
    expr: &TExpr,
    map: &mut HashMap<VariableId, Variable>,
) {
    match &expr.v {
        Expr::Variable(var) => {
            map.insert(var.id, var.clone());
        },
        Expr::Sequence(seq) => {
            for expr in seq {
                collect_expr_variables(expr, map);
            }
        },
        Expr::Product(prod) => {
            for expr in prod {
                collect_expr_variables(expr, map);
            }
        },
        Expr::Infix(_, expr1, expr2) => {
            collect_expr_variables(expr1, map);
            collect_expr_variables(expr2, map);
        },
        Expr::Negate(expr1) => {
            collect_expr_variables(expr1, map);
        },
        Expr::Application(expr1, expr2) => {
            collect_expr_variables(expr1, map);
            collect_expr_variables(expr2, map);
        },
        Expr::Function(fun) => {
            for param in &fun.0 {
                collect_pattern_variables(param, map);
            }
            collect_expr_variables(&*fun.1, map);
        },
        Expr::LetBinding(binding, body) => {
            collect_expr_variables(&*binding.1, map);
            collect_pattern_variables(&binding.0, map);
            collect_expr_variables(body, map);
        },
        Expr::Constant(_) => {},
    }
}

/* Collect all the variables occuring in the given definition. */
fn collect_def_variables(
    def: &Definition,
    map: &mut HashMap<VariableId, Variable>,
) {
    collect_expr_variables(&*def.0.1, map);
    collect_pattern_variables(&def.0.0, map);
}

/* Collect all the variables occuring in the given module. */
pub fn collect_module_variables(
    module: &Module,
    map: &mut HashMap<VariableId, Variable>,
) {
    for def in &module.defs {
        collect_def_variables(def, map);
    }
    for expr in &module.exprs {
        collect_expr_variables(expr, map);
    }
}

/* Produce the given binary operation making sure to do any straightforward
 * simplifications. */
fn infix_op(op: InfixOp, e1: TExpr, e2: TExpr) -> TExpr {
    match (op, &e1.v, &e2.v) {
        (InfixOp::Multiply, Expr::Constant(1), _) => e2,
        (InfixOp::Multiply, _, Expr::Constant(1)) => e1,
        (InfixOp::Multiply, Expr::Constant(0), _) => e1,
        (InfixOp::Multiply, _, Expr::Constant(0)) => e2,
        (InfixOp::Divide, _, Expr::Constant(1)) => e1,
        (InfixOp::Add, Expr::Constant(0), _) => e2,
        (InfixOp::Add, _, Expr::Constant(0)) => e1,
        (InfixOp::Subtract, _, Expr::Constant(0)) => e1,
        (op, _, _) => Expr::Infix(op, Box::new(e1), Box::new(e2)).into()
    }
}

/* Flatten the given binding down into the set of constraints it defines. */
fn flatten_binding(
    pat: &Pattern,
    expr: &TExpr,
    flattened: &mut Module,
) {
    match (pat, &expr.v) {
        (pat @ Pattern::Variable(_),
         Expr::Variable(_) | Expr::Constant(_) |
         Expr::Infix(_, _, _) | Expr::Negate(_)) => {
            flattened.defs.push(Definition(LetBinding(
                pat.clone(),
                Box::new(expr.clone()),
            )));
        },
        (Pattern::Constant(pat),
         Expr::Variable(_) | Expr::Constant(_) |
                 Expr::Infix(_, _, _) | Expr::Negate(_)) => {
            flattened.exprs.push(Expr::Infix(
                InfixOp::Equal,
                Box::new(Expr::Constant(*pat).into()),
                Box::new(expr.clone()),
            ).into());
        },
        (Pattern::As(pat, _name), _) => {
            flatten_binding(pat, expr, flattened);
        },
        (Pattern::Product(pats), Expr::Product(exprs)) => {
            for (pat, expr) in pats.iter().zip(exprs.iter()) {
                flatten_binding(pat, expr, flattened);
            }
        }
        _ => unreachable!("encountered unexpected binding: {} = {}", pat, expr),
    }
}

/* Flatten the given equality down into the set of constraints it defines. */
fn flatten_equals(
    expr1: &TExpr,
    expr2: &TExpr,
    flattened: &mut Module,
) {
    match (&expr1.v, &expr2.v) {
        (Expr::Product(prod1), Expr::Product(prod2)) => {
            for (expr1, expr2) in prod1.iter().zip(prod2.iter()) {
                flatten_equals(expr1, expr2, flattened);
            }
        },
        (Expr::Variable(_) | Expr::Negate(_) |
         Expr::Infix(_, _, _) | Expr::Constant(_),
         Expr::Variable(_) | Expr::Negate(_) |
         Expr::Infix(_, _, _) | Expr::Constant(_)) => {
            flattened.exprs.push(Expr::Infix(
                InfixOp::Equal,
                Box::new(expr1.clone()),
                Box::new(expr2.clone())
            ).into());
        },
        _ => unreachable!("encountered unexpected equality: {} = {}", expr1, expr2),
    }
}

/* Flatten the given expression down into the set of constraints it defines. */
fn flatten_expression(
    expr: &TExpr,
    flattened: &mut Module,
) -> TExpr {
    match &expr.v {
        Expr::Sequence(seq) => {
            let mut val = None;
            for expr in seq {
                val = Some(flatten_expression(expr, flattened));
            }
            val.expect("encountered empty sequence").clone()
        },
        Expr::Infix(InfixOp::Equal, expr1, expr2) => {
            let expr1 = flatten_expression(expr1, flattened);
            let expr2 = flatten_expression(expr2, flattened);
            flatten_equals(&expr1, &expr2, flattened);
            Expr::Product(vec![]).into()
        },
        Expr::Infix(op, expr1, expr2) => {
            let expr1 = flatten_expression(expr1, flattened);
            let expr2 = flatten_expression(expr2, flattened);
            Expr::Infix(*op, Box::new(expr1), Box::new(expr2)).into()
        },
        Expr::Product(prod) => {
            let mut exprs = vec![];
            for expr in prod {
                exprs.push(flatten_expression(expr, flattened));
            }
            Expr::Product(exprs).into()
        },
        Expr::Negate(expr1) =>
            Expr::Negate(Box::new(flatten_expression(expr1, flattened))).into(),
        Expr::Constant(_) | Expr::Variable(_) => expr.clone(),
        Expr::LetBinding(binding, body) => {
            let val = flatten_expression(&*binding.1, flattened);
            flatten_binding(&binding.0, &val, flattened);
            flatten_expression(body, flattened)
        }
        Expr::Function(_) | Expr::Application(_, _) =>
            unreachable!("functions must already by inlined and eliminated"),
    }
}

/* Flatten the given definition down into the set of constraints it defines. */
fn flatten_definition(
    def: &Definition,
    flattened: &mut Module,
) {
    let val = flatten_expression(&*def.0.1, flattened);
    flatten_binding(&def.0.0, &val, flattened);
}

/* Flatten the given module down into the set of constraints it defines. */
pub fn flatten_module(
    module: &Module,
    flattened: &mut Module,
) {
    for def in &module.defs {
        flatten_definition(def, flattened);
    }
    for expr in &module.exprs {
        flatten_expression(expr, flattened);
    }
}

/* Make an equality expression to constrain the values that satify the circuit.
 * Simultaneously also make a variable definition to enable provers to generate
 * the necessary auxiliary variables. */
fn push_constraint_def(module: &mut Module, out: Pattern, expr: TExpr) {
    module.exprs.push(Expr::Infix(
        InfixOp::Equal,
        Box::new(out.to_expr()),
        Box::new(expr.clone())
    ).into());
    module.defs.push(Definition(LetBinding(out, Box::new(expr))));
}

/* Flatten the given expression down to a single term and place the definitions
 * of its parts into the given module. The parts always take the following form:
 * term1 = -term2 or term1 = term2 OP term3 */
fn flatten_expr_to_3ac(
    out: Option<Pattern>,
    expr: &TExpr,
    flattened: &mut Module,
    gen: &mut VarGen,
) -> Pattern {
    match (out, &expr.v) {
        (None, Expr::Constant(val)) => Pattern::Constant(*val),
        (None, Expr::Variable(var)) => Pattern::Variable(var.clone()),
        (Some(pat),
         Expr::Constant(_) | Expr::Variable(_)) => {
            push_constraint_def(flattened, pat.clone(), expr.clone());
            pat
        },
        (out, Expr::Negate(n)) => {
            let out1_term = flatten_expr_to_3ac(None, n, flattened, gen);
            let rhs = Expr::Negate(Box::new(out1_term.to_expr()));
            let out_var = Variable::new(gen.generate_id());
            let out = out.unwrap_or(Pattern::Variable(out_var.clone()));
            push_constraint_def(flattened, out.clone(), rhs.into());
            out
        },
        (out, Expr::Infix(InfixOp::Exponentiate, e1, e2)) => {
            match e2.v {
                Expr::Constant(0) =>
                    flatten_expr_to_3ac(out, &Expr::Constant(1).into(), flattened, gen),
                Expr::Constant(1) =>
                    flatten_expr_to_3ac(out, e1, flattened, gen),
                Expr::Constant(v2) if v2 > 0 => {
                    // Compute the base once and for all
                    let out1_term = flatten_expr_to_3ac(None, e1, flattened, gen);
                    // Compute roughly the sqrt of this expression
                    let sqrt = Expr::Infix(
                        InfixOp::Exponentiate,
                        Box::new(out1_term.to_expr()),
                        Box::new(Expr::Constant(v2/2).into())
                    );
                    let out2_term = flatten_expr_to_3ac(None, &sqrt.into(), flattened, gen);
                    // Now square the value to obtain roughly this expression
                    let mut rhs = infix_op(
                        InfixOp::Multiply,
                        out2_term.to_expr(),
                        out2_term.to_expr()
                    );
                    // Multiply by the base once more in order to obtain
                    // original value
                    if v2%2 == 1 {
                        rhs = infix_op(
                            InfixOp::Multiply,
                            rhs,
                            out1_term.to_expr()
                        );
                    }
                    flatten_expr_to_3ac(out, &rhs, flattened, gen)
                },
                Expr::Constant(v2) => {
                    // Compute the reciprocal of this expression
                    let recip = Expr::Infix(
                        InfixOp::Exponentiate,
                        Box::new(*e1.clone()),
                        Box::new(Expr::Constant(-v2).into())
                    );
                    let out1_term = flatten_expr_to_3ac(None, &recip.into(), flattened, gen);
                    // Now invert the value to obtain this expression
                    let rhs = infix_op(
                        InfixOp::Divide,
                        Expr::Constant(1).into(),
                        out1_term.to_expr()
                    );
                    flatten_expr_to_3ac(out, &rhs, flattened, gen)
                }
                _ => panic!("variables are not permitted in expression exponents"),
            }
        },
        (out, Expr::Infix(op, e1, e2)) => {
            let out1_term = flatten_expr_to_3ac(None, e1, flattened, gen);
            let out2_term = flatten_expr_to_3ac(None, e2, flattened, gen);
            let rhs = infix_op(
                *op,
                out1_term.to_expr(),
                out2_term.to_expr(),
            );
            let out_var = Variable::new(gen.generate_id());
            let out = out.unwrap_or(Pattern::Variable(out_var.clone()));
            push_constraint_def(flattened, out.clone(), rhs);
            out
        },
        _ => panic!("encountered unexpected expression: {}", expr),
    }
}

/* Flatten the given definition into three-address form. */
fn flatten_def_to_3ac(
    def: &Definition,
    flattened: &mut Module,
    gen: &mut VarGen,
) {
    flatten_expr_to_3ac(Some(def.0.0.clone()), &*def.0.1, flattened, gen);
}

/* Flatten all definitions and expressions in this module into three-address
 * form. */
pub fn flatten_module_to_3ac(
    module: &Module,
    flattened: &mut Module,
    gen: &mut VarGen,
) {
    for def in &module.defs {
        flatten_def_to_3ac(def, flattened, gen);
    }
    for expr in &module.exprs {
        if let Expr::Infix(InfixOp::Equal, lhs, rhs) = &expr.v {
            // Flatten this equality constraint into a series of definitions.
            // The last inserted definition is always an encoding of an equality
            // constraint.
            match (lhs, &lhs.v, rhs, &rhs.v) {
                (_, Expr::Variable(var), ohs, _) |
                (ohs, _, _, Expr::Variable(var)) => {
                    flatten_expr_to_3ac(
                        Some(Pattern::Variable(var.clone())),
                        ohs,
                        flattened,
                        gen
                    );
                },
                (_, Expr::Constant(val), ohs, _) |
                (ohs, _, _, Expr::Constant(val)) => {
                    flatten_expr_to_3ac(
                        Some(Pattern::Constant(*val)),
                        ohs,
                        flattened,
                        gen
                    );
                },
                (_, _, _, _) => {
                    let lhs = flatten_expr_to_3ac(None, lhs, flattened, gen);
                    let rhs = flatten_expr_to_3ac(None, rhs, flattened, gen);
                    flatten_expr_to_3ac(
                        Some(lhs),
                        &rhs.to_expr(),
                        flattened,
                        gen
                    );
                }
            }
            // Remove the last definition because it is solely an equality
            // constraint.
            flattened
                .defs
                .pop()
                .expect("a definition should have been made for the current expression");
        }
    }
}

/* Compile the given module down into three-address codes. */
pub fn compile(mut module: Module) -> Module {
    let mut vg = VarGen::new();
    let mut globals = HashMap::new();
    number_module_variables(&mut module, &mut globals, &mut vg);
    infer_module_types(&mut module, &globals, &mut HashMap::new(), &mut vg);
    apply_module_functions(&mut module, &mut vg);
    let mut types = HashMap::new();
    infer_module_types(&mut module, &globals, &mut types, &mut vg);
    // Expand all tuple variables
    expand_module_variables(&mut module, &mut types, &mut vg);
    // Unitize all function expressions
    unitize_module_functions(&mut module, &mut types);
    println!("{}\n", module);
    // Start generating arithmetic constraints
    let mut constraints = Module::default();
    flatten_module(&module, &mut constraints);
    println!("{}\n", constraints);
    let mut module_3ac = Module::default();
    flatten_module_to_3ac(&constraints, &mut module_3ac, &mut vg);
    module_3ac
}
