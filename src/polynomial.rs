use crate::ast::*;
use crate::ast::Expr;
use crate::typecheck::Type;
use std::collections::{HashMap, HashSet};
use std::cmp::Ordering;
use std::mem::swap;
use num_bigint::BigInt;
use ark_ff::{One, Zero};

pub fn create_polynomials(module: &mut Module) {
    println!("Original module:\n{}", module);
    println!("Original constraints: {}", module.exprs.len());
    substitute_to_plonk3(module);
    println!("module\n{}", module);
    println!("new constraint count: {}", module.exprs.len());
    
}

pub fn retain(expr: &TExpr, retain_set: &mut HashSet<VariableId>) {
    match &expr.v {
        Expr::Variable(var1) => {
            retain_set.insert(var1.id);
        },
        Expr::Infix(InfixOp::Equal, _, rhs) => {
            retain(rhs, retain_set);
        },
        Expr::Infix(_, expr1, expr2) => {
            retain(expr1, retain_set);
            retain(expr2, retain_set);
        },
        _ => {},
    }
}

/* finds equations of the form:
 *   [pub var] = [priv var]
 * and rearranges it so that the [priv var] occurs on the left.
 * This prevents public variables from being substituted away. */
pub fn swap_named_variables(module: &mut Module) {
    for eq in &mut module.exprs {
        if let Expr::Infix(InfixOp::Equal, ref mut lhs, ref mut rhs) = eq.v {
            if let (Expr::Variable(left_var), Expr::Variable(_)) = (&lhs.v, &rhs.v) {
                if module.pubs.contains(&left_var) || left_var.name.is_some() {
                        swap(lhs, rhs);
                }
            }
        }
    }
}

pub fn degree(expr: &TExpr) -> usize {
    match &expr.v {
        Expr::Constant(_) => 
            0,
        Expr::Variable(_) => 
            1,
        Expr::Infix(InfixOp::Multiply, expr1, expr2) =>
            degree(expr1) + degree(expr2),
        Expr::Infix(_, expr1, expr2) =>
            std::cmp::max(degree(expr1), degree(expr2)),
        _ => panic!("unsupported expression: {}", expr),
    }
}

pub fn construct_substitution_map(module: &mut Module) -> HashMap<VariableId, TExpr> {
    //println!("\t* Generating substitution map...");
    let mut substitutions = HashMap::new();
    for eq in &mut module.exprs {
        reduce_expr(eq);
        if let Expr::Infix(InfixOp::Equal, lhs, rhs) = &eq.v {
            match (&lhs.v, &rhs.v) {
                (Expr::Variable(left_var), Expr::Variable(right_var))
                    if (left_var.name.is_some() && right_var.name.is_none()) => {
                        // never add [named] -> [temp] paths to map
                    },
                (Expr::Variable(left_var), _) => {
                    substitutions.insert(left_var.id, *rhs.clone());
                },
                _ => {},
            }
        }
    }
    substitutions
}

pub fn eliminate_expressions(module: &mut Module, retain_set: &HashSet<VariableId>) {
    println!("\t* Eliminating unused expressions...");
    module.exprs.retain(|expr|
        match &expr.v {
            Expr::Infix(InfixOp::Equal, lhs, rhs) =>
                if are_identical(lhs, rhs) {
                    false
                } else if let Expr::Variable(left_var) = &lhs.v {
                    retain_set.contains(&left_var.id) || left_var.name.is_some()
                } else {
                    true
                },
            _ => true,
        }
    );
}

pub fn substitute_definitions(module: &mut Module, substitutions: &HashMap<VariableId, TExpr>) {
    println!("  * Substituting into definitions...");
    for def in &mut module.defs {
        let Definition(LetBinding(_, rhs)) = def;
        substitute_expr(rhs, &substitutions);
        reduce_expr(rhs);
    }
}

pub fn eliminate_definitions(module: &mut Module, retain_set: &HashSet<VariableId>) {
    println!("  * Eliminating unused definitions...");
    module.defs.retain(|def|
        if let Pat::Variable(var) = &def.0.0.v {
            retain_set.contains(&var.id)
        } else {
            true
        }
    );
}

/* makes only those substitutions which do not overfill a width-3 constraint */
pub fn substitute_to_plonk3(module: &mut Module) {
    println!("* Transforming into width-3 plonk constraints...");
    let mut substitutions = construct_substitution_map(module);
    let mut retain_set = HashSet::new();
    transform_plonk3(module, &mut substitutions, &mut retain_set);
    eliminate_expressions(module, &retain_set);
    substitute_definitions(module, &substitutions);
    eliminate_definitions(module, &retain_set);
}

pub fn sort_term_pair(expr1: &mut TExpr, expr2: &mut TExpr) {
    if compare_terms(expr1, expr2) == Ordering::Greater {
        swap(expr1, expr2);
    }
}

pub fn sort_factor_pair(expr1: &mut TExpr, expr2: &mut TExpr) {
    if compare_factors(expr1, expr2) == Ordering::Greater {
        swap(expr1, expr2);
    }
}

pub fn sort_terms(expr: &mut TExpr) {
    match &mut expr.v {
        Expr::Infix(InfixOp::Add, head, tail) =>
            if let Expr::Infix(InfixOp::Add, tailhead, _) = &mut tail.v {
                sort_term_pair(head, tailhead);
                sort_terms(tail)
            } else {
                sort_term_pair(head, tail);
            },
        Expr::Infix(InfixOp::Subtract, head, tail) =>
            if let Expr::Infix(InfixOp::Subtract, _, headtail) = &mut head.v {
                sort_term_pair(headtail, tail);
                sort_terms(head)
            },
        Expr::Infix(InfixOp::Multiply, head, tail) =>
            if let Expr::Infix(InfixOp::Multiply, tailhead, _) = &mut tail.v {
                sort_factor_pair(head, tailhead);
                sort_factors(tail)
            } else {
                sort_factor_pair(head, tail);
            },
        Expr::Infix(InfixOp::Equal, _, rhs) =>
            sort_terms(rhs),
        _ => {},
    }   
}

pub fn sort_factors(expr: &mut TExpr) {
    if let Expr::Infix(InfixOp::Multiply, head, tail) = &mut expr.v {
        if let Expr::Infix(InfixOp::Multiply, tailhead, _) = &mut tail.v {
            sort_factor_pair(head, tailhead);
            sort_factors(tail)
        } else {
            sort_factor_pair(head, tail);
        }
    } 
}

pub fn assoc(expr: &mut TExpr) {
    match &mut expr.v {
        Expr::Infix(InfixOp::Add | InfixOp::Subtract, expr1, expr2) => 
            match (&mut expr1.v, &mut expr2.v) {
                (Expr::Infix(InfixOp::Add | InfixOp::Subtract, _, _), Expr::Constant(_)) |
                (Expr::Infix(InfixOp::Add | InfixOp::Subtract, _, _), Expr::Variable(_)) =>
                    // c1 + expr2
                    // v1 + expr2
                    assoc(expr1),
                (_, Expr::Infix(InfixOp::Add | InfixOp::Subtract, _, _)) => {
                    // (expr11 + expr12) + expr2   ->   expr11 + (expr12 + expr2)
                    assoc(expr1);
                    swap(expr1, expr2);
                },
                _ => {},
            },
        Expr::Infix(InfixOp::Multiply, expr1, expr2) => {
                match (&mut expr1.v, &mut expr2.v) {
                    // c1*expr2
                    // v1*expr2
                    (Expr::Constant(_), Expr::Infix(InfixOp::Multiply, _, _)) |
                    (Expr::Variable(_), Expr::Infix(InfixOp::Multiply, _, _)) =>
                        assoc(expr2),
                    // (expr11*expr12)*expr2   ->   expr11*(expr12*expr2)
                    (Expr::Infix(InfixOp::Multiply, _, _), _) => {
                        assoc(expr2);
                        swap(expr1, expr2);
                    },
                    _ => {},
                }
            },
        Expr::Infix(InfixOp::Equal, _, rhs) =>
                assoc(rhs),
            _ => {},
    }
}

pub fn assoc_right(expr: &mut TExpr) {
    match &mut expr.v {
        Expr::Infix(InfixOp::Add, expr1, expr2) => 
            match (&mut expr1.v, &mut expr2.v) {
                (Expr::Constant(_), Expr::Infix(InfixOp::Add, _, _)) |
                (Expr::Variable(_), Expr::Infix(InfixOp::Add, _, _)) =>
                    // c1 + expr2
                    // v1 + expr2
                    assoc_right(expr2),
                (Expr::Infix(InfixOp::Add, _, _), _) => {
                    // (expr11 + expr12) + expr2   ->   expr11 + (expr12 + expr2)
                    assoc_right(expr2);
                    swap(expr1, expr2);
                },
                _ => {},
            },
        Expr::Infix(InfixOp::Multiply, expr1, expr2) => {
            match (&mut expr1.v, &mut expr2.v) {
                // c1*expr2
                // v1*expr2
                (Expr::Constant(_), Expr::Infix(InfixOp::Multiply, _, _)) |
                (Expr::Variable(_), Expr::Infix(InfixOp::Multiply, _, _)) =>
                    assoc_right(expr2),
                // (expr11*expr12)*expr2   ->   expr11*(expr12*expr2)
                (Expr::Infix(InfixOp::Multiply, _, exp_r12), _) => {
                    assoc_right(expr2);
                    swap(expr1, expr2);
                },
                _ => {},
            }
        },
        Expr::Infix(InfixOp::Equal, _, rhs) =>
            assoc_right(rhs),
        _ => {},
    }
}

/* uses the distributive property to expand an expression into a sum of products */
pub fn distribute_expr(expr: &mut TExpr) {
    match &mut expr.v {
        Expr::Infix(InfixOp::Multiply, expr1, expr2) => {
            distribute_expr(expr1);
            distribute_expr(expr2);
            match &mut expr1.v {
                Expr::Infix(InfixOp::Multiply, _, _) => {},
                Expr::Infix(op, expr11, expr12) => {
                    // (expr11 +/- expr12)*expr2 -> expr11*expr2 +/- expr12*expr2
                    let mut left = Expr::Infix(InfixOp::Multiply, Box::new(*expr2.clone()), Box::new(*expr11.clone())).type_expr(Some(Type::Int));
                    let mut right = Expr::Infix(InfixOp::Multiply, Box::new(*expr2.clone()), Box::new(*expr12.clone())).type_expr(Some(Type::Int));  
                    distribute_expr(&mut left);
                    distribute_expr(&mut right);
                    expr.v = Expr::Infix(*op, Box::new(left), Box::new(right));
                },
                _ => match &mut expr2.v {
                    Expr::Infix(InfixOp::Multiply, _, _) => {},
                    Expr::Infix(op, expr21, expr22) => {
                        // expr1*(expr21 +/- expr22) -> (expr1*expr21) +/- (expr1*expr22)
                        let mut left = Expr::Infix(InfixOp::Multiply, Box::new(*expr1.clone()), Box::new(*expr21.clone())).type_expr(Some(Type::Int));
                        let mut right = Expr::Infix(InfixOp::Multiply, Box::new(*expr1.clone()), Box::new(*expr22.clone())).type_expr(Some(Type::Int));
                        distribute_expr(&mut left);
                        distribute_expr(&mut right);
                        expr.v = Expr::Infix(*op, Box::new(left), Box::new(right));
                    },
                    _ => {},
                },
            };
        },
        Expr::Infix(_, expr1, expr2) => {
            distribute_expr(expr1);
            distribute_expr(expr2);
        },
        _ => {},
    }
}


/* Recursively makes any available substitutions in the map */
pub fn substitute_expr(
    expr: &mut TExpr,
    substitutions: &HashMap<VariableId, TExpr>,
) {
    match &mut expr.v {
        Expr::Variable(v) if substitutions.contains_key(&v.id) => {
            *expr = substitutions[&v.id].clone();
            substitute_expr(expr, substitutions);
        },
        Expr::Infix(InfixOp::Equal, expr1, expr2) => {
            substitute_expr(expr2, substitutions);
        },
        Expr::Infix(_, expr1, expr2) => {
            substitute_expr(expr1, substitutions);
            substitute_expr(expr2, substitutions);
        },
        Expr::Negate(expr1) => {
            substitute_expr(expr1, substitutions);
        },
        _ => {},
    };
}

pub fn is_subset(expr1: &TExpr, expr2: &TExpr, substitutions: &HashMap<VariableId, TExpr>) -> bool {
    match &expr1.v {
        Expr::Constant(_) =>
            true,
        Expr::Variable(v1) =>
             contains_variable(expr2, expr1, substitutions),
        Expr::Negate(e1) =>
            is_subset(e1, expr2, substitutions),
        Expr::Infix(_, e1, e2) =>
            is_subset(e1, expr2, substitutions) &&
            is_subset(e2, expr2, substitutions),
        _ =>
            false,
    }
}

pub fn assign_coefficients(expr: &mut TExpr) {
    match &mut expr.v {
        Expr::Variable(v) => {
            let one = Expr::Constant(BigInt::from(1)).type_expr(Some(Type::Int));
            expr.v = Expr::Infix(InfixOp::Multiply, Box::new(one), Box::new(expr.clone()));
        },
        Expr::Negate(expr1) => {
            let neg_one = Expr::Constant(BigInt::from(-1)).type_expr(Some(Type::Int));
            expr.v = Expr::Infix(InfixOp::Multiply, Box::new(neg_one), expr1.clone());
        },
        Expr::Infix(InfixOp::Multiply, expr1, _) =>
            assign_coefficients(expr1),
        Expr::Infix(InfixOp::Add, expr1, expr2) => {
            assign_coefficients(expr1);
            assign_coefficients(expr2);
        },
        Expr::Infix(op, expr1, expr2) 
            if matches!(op, InfixOp::Subtract) => {
                *op = InfixOp::Add;
                assign_coefficients(expr1);
                let neg_one = Expr::Constant(BigInt::from(-1)).type_expr(Some(Type::Int));
                expr2.v = Expr::Infix(InfixOp::Multiply, Box::new(neg_one), expr2.clone());
        },
        Expr::Infix(InfixOp::Equal, expr1, expr2) => {
            assign_coefficients(expr1);
            assign_coefficients(expr2);
        },
        _ => {},
    }
}

/* Uses a match to detect whether an expression contains a particular variable, following all paths
 * through the substitution map */
pub fn contains_variable(expr: &TExpr, var_expr: &TExpr, substitutions: &HashMap<VariableId, TExpr>) -> bool {
    //println!("checking if {} is contained in {}", var_expr, expr);
    if let Expr::Variable(var) = &var_expr.v {
        match &expr.v {
            Expr::Constant(_) => {
                //println!("\tfalse");
                 false},
            Expr::Variable(m) if substitutions.contains_key(&m.id) =>
                contains_variable(&substitutions[&m.id], var_expr, substitutions),
            Expr::Variable(m) => {
                //println!("\t{}", m.id == var.id);
                 m.id == var.id},
            Expr::Negate(expr1) => 
                contains_variable(expr1, var_expr, substitutions),
            Expr::Infix(_, expr1, expr2) => 
                contains_variable(expr1, var_expr, substitutions) || contains_variable(expr2, var_expr, substitutions),
            _ => panic!("only constants, variables, and infix allowed at this point: {}", expr),
        }
    } else {
        //println!("\tfalse");
         false
    }
}

/* Detects if two expressions are identical (a*b is not identical to b*a ) */
pub fn are_identical(expr1: &TExpr, expr2: &TExpr) -> bool {
    match (&expr1.v, &expr2.v) {
        (Expr::Constant(c1), Expr::Constant(c2)) =>
            c1 == c2,
        (Expr::Variable(v1), Expr::Variable(v2)) =>
            v1.id == v2.id,
        (Expr::Negate(e1), Expr::Negate(e2)) =>
            are_identical(e1, e2),
        (Expr::Infix(_, e11, e12), Expr::Infix(_, e21, e22)) =>
            are_identical(e11, e21) && are_identical(e12, e22),
        _ => false,
    }
}

pub fn collect_terms(expr: &mut TExpr) {
    match &mut expr.v {
        Expr::Infix(op, expr1, expr2) =>
            match (&op, &mut expr1.v, &mut expr2.v) {
                (InfixOp::Add, Expr::Variable(v1), Expr::Variable(v2)) 
                    if v1.id == v2.id => {
                        *op = InfixOp::Multiply;
                        expr1.v = Expr::Constant(BigInt::from(2));
                    },
                (InfixOp::Add, Expr::Infix(InfixOp::Multiply, expr11, expr12), Expr::Variable(v2)) =>
                    if let (Expr::Constant(c), Expr::Variable(v1)) = (&mut expr11.v, &expr12.v) {
                        if v1.id == v2.id {
                            *c += 1;
                            *expr = *expr1.clone();
                        }
                    },
                (InfixOp::Add, Expr::Variable(v1), Expr::Infix(InfixOp::Multiply, expr21, expr22)) =>
                    if let (Expr::Constant(c), Expr::Variable(v2)) = (&mut expr21.v, &expr22.v) {
                        if v1.id == v2.id {
                            *c += 1;
                            *expr = *expr2.clone();
                        }
                    },
                (InfixOp::Add, Expr::Infix(InfixOp::Multiply, expr11, expr12), Expr::Infix(InfixOp::Multiply, expr21, expr22)) =>
                    if let (Expr::Constant(c1), Expr::Variable(v1), Expr::Constant(c2), Expr::Variable(v2)) = (&mut expr11.v, &expr12.v, &expr21.v, &expr22.v) {
                        if v1.id == v2.id {
                            *c1 += c2;
                            *expr = *expr1.clone();
                        }
                    },
                (InfixOp::Subtract, Expr::Variable(v1), Expr::Variable(v2)) 
                    if v1.id == v2.id => {
                        *op = InfixOp::Multiply;
                        expr.v = Expr::Constant(BigInt::from(0));
                    },
                (InfixOp::Subtract, Expr::Infix(InfixOp::Multiply, expr11, expr12), Expr::Variable(v2)) =>
                    if let (Expr::Constant(c), Expr::Variable(v1)) = (&mut expr11.v, &expr12.v) {
                        if v1.id == v2.id {
                            *c -= 1;
                            *expr = *expr1.clone();
                        }
                    },
                (InfixOp::Subtract, Expr::Variable(v1), Expr::Infix(InfixOp::Multiply, expr21, expr22)) =>
                    if let (Expr::Constant(c), Expr::Variable(v2)) = (&mut expr21.v, &expr22.v) {
                        if v1.id == v2.id {
                            *c -= 1;
                            *expr = *expr2.clone();
                        }
                    },
                (InfixOp::Subtract, Expr::Infix(InfixOp::Multiply, expr11, expr12), Expr::Infix(InfixOp::Multiply, expr21, expr22)) =>
                    if let (Expr::Constant(c1), Expr::Variable(v1), Expr::Constant(c2), Expr::Variable(v2)) = (&mut expr11.v, &expr12.v, &expr21.v, &expr22.v) {
                        if v1.id == v2.id {
                            *c1 -= c2;
                            *expr = *expr1.clone();
                        }
                    },
                (InfixOp::Multiply, Expr::Infix(InfixOp::Multiply, expr11, expr12), Expr::Infix(InfixOp::Multiply, expr21, expr22)) =>
                    if let (Expr::Constant(c1), Expr::Variable(v1), Expr::Constant(c2), Expr::Variable(v2)) = (&mut expr11.v, &expr12.v, &expr21.v, &expr22.v) {
                        if v1.id == v2.id {
                            *c1 *= c2;
                            *expr = *expr1.clone();
                        }
                    },
                _ => {
                    collect_terms(expr1);
                    collect_terms(expr2);
                },
            }
        _ => {},        
    }
}

pub fn reduce_expr(expr: &mut TExpr) {
    match &mut expr.v {
        Expr::Constant(_) | Expr::Variable(_) => {},
        Expr::Negate(expr1) => {
            if let Expr::Constant(c) = &expr1.v {
                expr.v = Expr::Constant(c*-1);
            }
        },
        Expr::Infix(op, expr1, expr2) => {
            reduce_expr(expr1);
            reduce_expr(expr2);
            match (op, &mut expr1.v, &mut expr2.v) {
                // c1 + c2 -> eval(c1+c2)
                (InfixOp::Add, Expr::Constant(c1), Expr::Constant(c2)) => {
                    *c1 += c2.clone();
                    expr.v = expr1.v.clone();
                }
                // c1 - c2 -> eval(c1-c2)
                (InfixOp::Subtract, Expr::Constant(c1), Expr::Constant(c2)) => {
                    *c1 -= c2.clone();
                    expr.v = expr1.v.clone();
                }
                // c1*c2 -> eval(c1*c2)
                (InfixOp::Multiply, Expr::Constant(c1), Expr::Constant(c2)) => {
                    *c1 *= c2.clone();
                    expr.v = expr1.v.clone();
                }
                // 0 + v2 -> v2
                (InfixOp::Add, Expr::Constant(c1), _) if c1.is_zero() =>
                    expr.v = expr2.v.clone(),
                // v1 + 0 -> v1
                (InfixOp::Add, _, Expr::Constant(c2)) if c2.is_zero() =>
                    expr.v = expr1.v.clone(),
                // 0 - v2 -> -v2
                (InfixOp::Subtract, Expr::Constant(c1), _) if c1.is_zero() =>
                    expr.v = Expr::Negate(Box::new(expr2.v.clone().type_expr(Some(Type::Int)))),
                // v1 - 0 -> v1
                (InfixOp::Subtract, _, Expr::Constant(c2)) if c2.is_zero() =>
                    expr.v = expr1.v.clone(),
                // c1*(e2 + e3) -> c1*e2 + c1*e3
                (InfixOp::Multiply, Expr::Constant(_), Expr::Infix(InfixOp::Add, expr21, expr22)) => {
                    expr21.v = Expr::Infix(InfixOp::Multiply, expr1.clone(), expr21.clone());
                    expr22.v = Expr::Infix(InfixOp::Multiply, expr1.clone(), expr22.clone());
                    reduce_expr(expr21);
                    reduce_expr(expr22);
                    expr.v = expr2.v.clone();
                },
                // c1*(e2 - e3) -> c1*e2 - c1*e3
                (InfixOp::Multiply, Expr::Constant(_), Expr::Infix(InfixOp::Subtract, expr21, expr22)) => {
                    expr21.v = Expr::Infix(InfixOp::Multiply, expr1.clone(), expr21.clone());
                    expr22.v = Expr::Infix(InfixOp::Multiply, expr1.clone(), expr22.clone());
                    reduce_expr(expr21);
                    reduce_expr(expr22);
                    expr.v = expr2.v.clone();
                },
                // c1*(c2*e3) -> eval(c1*c2)*e3
                // c1*(e2*e3) -> (c1*e2)*e3
                (InfixOp::Multiply, Expr::Constant(c1), Expr::Infix(InfixOp::Multiply, expr21, expr22)) =>
                    if let Expr::Constant(c2) = &expr21.v {
                        *c1 *= c2.clone();
                        expr2.v = expr22.v.clone();
                    } else {
                        swap(expr1, expr22);    // e3*(e2*c1)                       
                        swap(expr21, expr22);   // e3*(c1*e2)
                        swap(expr1, expr2);     // (c1*e2)*e3
                        reduce_expr(expr1);
                    }
                // 1*v2 -> v2
                (InfixOp::Multiply, Expr::Constant(c1), _) if c1.is_one()=>
                    expr.v = expr2.v.clone(),
                // v1*1 -> v1
                (InfixOp::Multiply, _, Expr::Constant(c2)) if c2.is_one() =>
                    expr.v = expr1.v.clone(),
                // 0*v2 -> 0
                (InfixOp::Multiply, Expr::Constant(c1), _) if c1.is_zero() =>
                    expr.v = Expr::Constant(Zero::zero()),
                // v1*0 -> 0
                (InfixOp::Multiply, _, Expr::Constant(c2)) if c2.is_zero() =>
                    expr.v = Expr::Constant(Zero::zero()),
                _ => {},
            }
        }
        _ => panic!("only constants, variables, and infix allowed at this point: {}", expr),
    }
}

/* Decides if a given expression is constant by looking up available substitutions from the map*/
pub fn is_constant(expr: &TExpr, substitutions: &HashMap<VariableId, TExpr>) -> bool {
    match &expr.v {
        Expr::Constant(_) => true,
        Expr::Variable(v) if substitutions.contains_key(&v.id) =>
            is_constant(&substitutions[&v.id], substitutions),
        Expr::Negate(expr1) =>
            is_constant(expr1, substitutions),
        Expr::Infix(_, expr1, expr2) =>
            is_constant(expr1, substitutions) &&
            is_constant(expr2, substitutions),
        _ => false,
    }
}

pub fn transform_plonk3(module: &mut Module, substitutions: &mut HashMap<VariableId, TExpr>, retain_set: &mut HashSet<VariableId>) {
    println!("  * Transforming constraints to Plonk3 form...");
    for eq in &mut module.exprs {
        if let Expr::Infix(InfixOp::Equal, lhs, rhs) = &mut eq.v {
            substitute_copy(lhs, substitutions);
            substitute_constant(lhs, substitutions);
            match &lhs.v {
                Expr::Constant(_) =>
                    substitute_triple(lhs, rhs, substitutions, &module.pubs),
                Expr::Variable(v) if module.pubs.contains(v) =>
                    substitute_triple(lhs, rhs, substitutions, &module.pubs),
                _ => substitute_plonk3(lhs, rhs, substitutions, &module.pubs)
            }
        }
        reduce_expr(eq);
        retain(eq, retain_set);
    }
}

pub fn substitute_constant(expr: &mut TExpr, substitutions: &HashMap<VariableId, TExpr>) {
    if let Expr::Variable(v) = &expr.v {
        if substitutions.contains_key(&v.id) {
            if is_constant(&substitutions[&v.id], substitutions) {
                substitute_expr(expr, substitutions)
            }
        }
    }
}

pub fn substitute_copy(expr: &mut TExpr, substitutions: &HashMap<VariableId, TExpr>) {
    if let Expr::Variable(v1) = &expr.v {
        if substitutions.contains_key(&v1.id) {
            if let Expr::Variable(v2) = &substitutions[&v1.id].v {
                if !(v1.name.is_some() && v2.name.is_none()) {
                    expr.v = substitutions[&v1.id].v.clone()
                }
            }
        }
    }
}

pub fn substitute_triple(out: &TExpr, expr: &mut TExpr, substitutions: &mut HashMap<VariableId, TExpr>, pubs: &Vec<Variable>) {
    match &mut expr.v {
        Expr::Variable(v1) if substitutions.contains_key(&v1.id) => {
            match &substitutions[&v1.id].v {
                Expr::Constant(_) => {
                    expr.v = substitutions[&v1.id].v.clone();
                },
                Expr::Variable(v2) => {   
                    if !(v1.name.is_some() && v2.name.is_none())
                    && !contains_variable(&substitutions[&v1.id], out, substitutions) {
                        expr.v = substitutions[&v1.id].v.clone();
                        substitute_triple(out, expr, substitutions, pubs);
                    }
                },
                Expr::Negate(_) => {                   
                    if !contains_variable(&substitutions[&v1.id], out, substitutions) {
                        expr.v = substitutions[&v1.id].v.clone();
                        substitute_triple(out, expr, substitutions, pubs);
                    }
                },
                Expr::Infix(_, expr1, expr2) 
                    if matches!((&expr1.v, &expr2.v), (Expr::Constant(_), Expr::Variable(_))) => {
                        if !contains_variable(&substitutions[&v1.id], out, substitutions) {
                            expr.v = substitutions[&v1.id].v.clone();
                            substitute_triple(out, expr, substitutions, pubs);
                        }
                    },
                Expr::Infix(_, expr1, expr2) 
                    if matches!((&expr1.v, &expr2.v), (Expr::Variable(_), Expr::Constant(_))) => {
                        if !contains_variable(&substitutions[&v1.id], out, substitutions) {
                            expr.v = substitutions[&v1.id].v.clone();
                            substitute_triple(out, expr, substitutions, pubs);
                        }
                    },
                Expr::Infix(_, expr1, expr2) =>
                    if !contains_variable(&substitutions[&v1.id], out, substitutions) {
                        expr.v = substitutions[&v1.id].v.clone();
                        substitute_plonk3(out, expr, substitutions, pubs);
                    }
                _ => {},
                }
            },
        Expr::Infix(_, expr1, expr2) => {
            substitute_plonk3(out, expr1, substitutions, pubs);
            substitute_single(out, expr2, substitutions);
            reduce_expr(expr);
        },       
        _ => {},
    }
}

pub fn substitute_single(out: &TExpr, expr: &mut TExpr, substitutions: &mut HashMap<VariableId, TExpr>) {
    match &mut expr.v {
        Expr::Variable(v1) if substitutions.contains_key(&v1.id) => {
            match &substitutions[&v1.id].v {
                Expr::Constant(_) => {
                    expr.v = substitutions[&v1.id].v.clone();
                },
                Expr::Variable(v2) => {   
                    if !(v1.name.is_some() && v2.name.is_none())
                    && !contains_variable(&substitutions[&v1.id], out, substitutions) {
                        expr.v = substitutions[&v1.id].v.clone();
                        substitute_single(out, expr, substitutions);
                    }
                },
                Expr::Negate(_) => {                   
                    if !contains_variable(&substitutions[&v1.id], out, substitutions) {
                        expr.v = substitutions[&v1.id].v.clone();
                        substitute_single(out, expr, substitutions);
                    }
                },
                Expr::Infix(_, expr1, expr2) 
                    if matches!((&expr1.v, &expr2.v), (Expr::Constant(_), Expr::Variable(_))) => {
                        if !contains_variable(&substitutions[&v1.id], out, substitutions) {
                            expr.v = substitutions[&v1.id].v.clone();
                            substitute_single(out, expr, substitutions);
                        }
                    },
                Expr::Infix(_, expr1, expr2) 
                    if matches!((&expr1.v, &expr2.v), (Expr::Variable(_), Expr::Constant(_))) => {
                        if !contains_variable(&substitutions[&v1.id], out, substitutions) {
                            expr.v = substitutions[&v1.id].v.clone();
                            substitute_single(out, expr, substitutions);
                        }
                    },
                _ => {},
                }
            },
        Expr::Infix(_, expr1, expr2) => {
            substitute_single(out, expr1, substitutions);
            substitute_single(out, expr2, substitutions);
            reduce_expr(expr);
        },       
        _ => {},
    }
}

pub fn substitute_common(out: &TExpr, expr1: &mut TExpr, expr2: &mut TExpr, substitutions: &mut HashMap<VariableId, TExpr>) {
    match &mut expr1.v {
        Expr::Variable(v1) if substitutions.contains_key(&v1.id) => {
            if is_subset(&expr2, &substitutions[&v1.id], substitutions) 
            && !contains_variable(&substitutions[&v1.id], out, substitutions) {
                substitute_expr(expr1, substitutions)
            }
        },
        Expr::Negate(expr11) => {
            substitute_common(out, expr11, expr2, substitutions)
        },
        Expr::Infix(_, expr11, expr12)
            if matches!(&expr11.v, Expr::Constant(_)) => {
                substitute_common(out, expr12, expr2, substitutions);
            },
        Expr::Infix(_, expr11, expr12)
            if matches!(&expr12.v, Expr::Constant(_)) => {
                substitute_common(out, expr11, expr2, substitutions);
            },          
        _ => {},
    }
}

pub fn substitute_plonk3(out: &TExpr, expr: &mut TExpr, substitutions: &mut HashMap<VariableId, TExpr>, pubs: &Vec<Variable>) {
    match &mut expr.v {
        Expr::Variable(v2) if substitutions.contains_key(&v2.id) => {
            if !contains_variable(&substitutions[&v2.id], out, substitutions) {
                expr.v = substitutions[&v2.id].v.clone();
                substitute_plonk3(out, expr, substitutions, pubs);
        }},
        Expr::Negate(expr1) => {
            substitute_plonk3(out, expr1, substitutions, pubs);
        },
        Expr::Infix(_, expr1, expr2)
            if matches!(&expr1.v, Expr::Constant(_)) => {
                substitute_plonk3(out, expr2, substitutions, pubs);},
        Expr::Infix(_, expr1, expr2)
            if matches!(&expr2.v, Expr::Constant(_)) => {
                substitute_plonk3(out, expr1, substitutions, pubs);},
        Expr::Infix(_, expr1, expr2)
            if matches!(&expr1.v, Expr::Variable(v1) if {
                pubs.contains(v1)
            }) => {
                substitute_plonk3(out, expr2, substitutions, pubs)
            },
        Expr::Infix(_, expr1, expr2)
            if matches!(&expr2.v, Expr::Variable(v2) if {
                pubs.contains(v2)
            }) => {
                substitute_plonk3(out, expr1, substitutions, pubs)
            },
        Expr::Infix(InfixOp::Add, expr1, expr2) => {
            substitute_single(out, expr1, substitutions);
            substitute_single(out, expr2, substitutions);
            substitute_common(out, expr1, expr2, substitutions);
            substitute_common(out, expr2, expr1, substitutions);
            collect_terms(expr);
            reduce_expr(expr);
        },
        Expr::Infix(InfixOp::Subtract, expr1, expr2) => {
            substitute_single(out, expr1, substitutions);
            substitute_single(out, expr2, substitutions);
            substitute_common(out, expr1, expr2, substitutions);
            substitute_common(out, expr2, expr1, substitutions);
            collect_terms(expr);
            reduce_expr(expr);
        },
        Expr::Infix(InfixOp::Multiply, expr1, expr2) => {
            substitute_single(out, expr1, substitutions);
            substitute_single(out, expr2, substitutions);
            collect_terms(expr);
            reduce_expr(expr);
        },
        _ => {},
    }
}

/* Used to order the terms in an equation or expression to match the usual ordering of Plonk constraints.
 * Terms are ordered by the reverse of their degree so that high-degree terms are sorted to the front of 
 * an expression. Terms with the same degree are ordered by the id of their first variable id. */
pub fn compare_terms(a: &TExpr, b: &TExpr) -> Ordering {
    match (&a.v, &b.v) {
        (Expr::Constant(_), Expr::Constant(_)) => Ordering::Equal,
        (Expr::Constant(_), Expr::Variable(_)) => Ordering::Greater,
        (Expr::Variable(_), Expr::Constant(_)) => Ordering::Less,
        (Expr::Constant(_), Expr::Infix(_, _, _)) => Ordering::Greater,
        (Expr::Infix(_, _, _), Expr::Constant(_)) => Ordering::Less,
        (Expr::Variable(_), Expr::Infix(_, _, _)) => Ordering::Greater,
        (Expr::Infix(_, _, _), Expr::Variable(_)) => Ordering::Less,
        (Expr::Variable(var1), Expr::Variable(var2)) => var1.id.cmp(&var2.id),
        (Expr::Infix(_, expr11, expr12), Expr::Infix(_, expr21, expr22)) =>
            // split off the heads, if they are equal compare the tails
            match compare_terms(&expr11, &expr21) {
                Ordering::Greater => Ordering::Greater,
                Ordering::Less => Ordering::Less,
                Ordering::Equal => compare_terms(&expr12, &expr22),
            },
        _ => panic!("only constants, variables, and products allowed here")
    }
}

/* This is used to sort the factors in a product. Constants should be less than variables
 * so that they are sorted to the front of any product, and variables ought to be sorted
 * by their id. */
pub fn compare_factors(a: &TExpr, b: &TExpr) -> Ordering {
    match (&a.v, &b.v) {
        (Expr::Constant(_), Expr::Constant(_)) => Ordering::Equal,
        (Expr::Constant(_), _) => Ordering::Less,
        (_, Expr::Constant(_)) => Ordering::Greater,
        (Expr::Variable(var1), Expr::Variable(var2)) => var1.id.cmp(&var2.id),
        (Expr::Variable(_), Expr::Infix(_, expr1, _)) =>
            compare_factors(a, expr1),
        (Expr::Infix(_, expr1, _), Expr::Variable(_)) =>
            compare_factors(expr1, b),
        _ => panic!("only constants, variables, and products allowed here")
    }
}