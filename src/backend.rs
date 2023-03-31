use num_bigint::BigInt;
use crate::ast::{TExpr, Variable, Expr, InfixOp};

pub trait BackendCompiler<F, P> 
where
    F: PartialEq,
{
    // // Evaluate the given expression sourcing any variables from the given maps.
    // fn evaluate_applications<F>(
    //     expr: &TExpr,
    //     defs: &mut HashMap<VariableId, TExpr>,
    //     assigns: &mut HashMap<VariableId, F>,
    // ) -> F;

    // fn synthesize_applications<R>(expr: &TExpr) -> R;
    type Return;

    fn invert(x: F) -> F;
    fn to_bigint(x: F)-> BigInt;
    fn to_field(c: &BigInt) -> F;

    fn add_vvv(v1: &Variable, v2: &Variable, v3: &Variable) -> Self::Return;
    fn add_vvc(v1: &Variable, v2: &Variable, c3: &BigInt) -> Self::Return;
    fn add_cvv(c1: &BigInt, v2: &Variable, v3: &Variable) -> Self::Return;
    fn eq_vc(v1: &Variable, c2: &BigInt) -> Self::Return;
    fn eq_vv(v1: &Variable, v2: &Variable) -> Self::Return;
    fn eq_cc(c1: &BigInt, c2: &BigInt) -> Self::Return;
    fn mul_vvv(v1: &Variable, v2: &Variable, v3: &Variable) -> Self::Return;
    fn mul_vvc(v1: &Variable, v2: &Variable, c3: &BigInt) -> Self::Return;
    fn mul_cvc(c1: &BigInt, v2: &Variable, c3: &BigInt) -> Self::Return;
    fn mul_cvv(c1: &BigInt, v2: &Variable, v3: &Variable) -> Self::Return;
    

    fn zero() -> F {
        Self::to_field(&BigInt::from(0))
    }

    // arith synth
    fn arithmetic_synth(expr: & TExpr) -> Self::Return {
        if let Expr::Infix(InfixOp::Equal, lhs, rhs) = &expr.v {
            match (&lhs.v, &rhs.v) {
                // Variables on the LHS
                // v1 = v2
                (Expr::Variable(v1), Expr::Variable(v2)) =>
                    Self::eq_vv(v1, v2),
                // v1 = c2
                (Expr::Variable(v1), Expr::Constant(c2)) =>
                    Self::eq_vc(v1, c2),

                // Negations

                // v1 = -c2
                // v1 = -v2
                (Expr::Variable(v1), Expr::Negate(e2)) =>
                    match &e2.v {
                        Expr::Constant(c2) => Self::eq_vc(v1, &-c2),
                        Expr::Variable(v2) => Self::add_cvv(&BigInt::from(0), v1, v2),
                    _ => unreachable!(),
                },

                // Additions

                // v1 = c2 + c3
                // v1 = v2 + c3
                // v1 = c2 + v3
                // v1 = v2 + v3
                (Expr::Variable(v1), Expr::Infix(InfixOp::Add, e2, e3)) =>
                    match (&e2.v, &e3.v) {
                        (Expr::Constant(c2), Expr::Constant(c3)) =>
                            Self::eq_vc(v1, &(c2+c3)),
                        (Expr::Variable(v2), Expr::Constant(c3)) =>
                            Self::add_vvc(v1, v2, c3),
                        (Expr::Constant(c2), Expr::Variable(v3)) =>
                            Self::add_vvc(v1, v3, c2),
                        (Expr::Variable(v2), Expr::Variable(v3)) =>
                            Self::add_vvv(v1, v2, v3),
                        _ => unreachable!(),
                    },

                // Subtractions

                // v1 = c2 - c3
                // v1 = v2 - c3
                // v1 = c2 - v3
                // v1 = v2 - v3
                (Expr::Variable(v1), Expr::Infix(InfixOp::Subtract, e2, e3)) =>
                    match (&e2.v, &e3.v) {
                        (Expr::Constant(c2), Expr::Constant(c3)) =>
                            Self::eq_vc(v1, &(c2-c3)),
                        (Expr::Variable(v2), Expr::Constant(c3)) =>
                            Self::add_vvc(v2, v1, c3),
                        (Expr::Constant(c2), Expr::Variable(v3)) =>
                            Self::add_cvv(c2, v1, v3),
                        (Expr::Variable(v2), Expr::Variable(v3)) =>
                            Self::add_vvv(v2, v1, v3),
                        _ => unreachable!(),
                    },

                // Multiplications

                // v1 = c2 * c3
                // v1 = v2 * c3
                // v1 = c2 * v3
                // v1 = v2 * v3
                (Expr::Variable(v1), Expr::Infix(InfixOp::Multiply, e2, e3)) =>
                    match (&e2.v, &e3.v) {
                        (Expr::Constant(c2), Expr::Constant(c3)) =>
                            Self::eq_vc(v1, &(c2*c3)),
                        (Expr::Variable(v2), Expr::Constant(c3)) =>
                            Self::mul_vvc(v1, v2, c3),
                        (Expr::Constant(c2), Expr::Variable(v3)) =>
                            Self::mul_vvc(v1, v3, c2),
                        (Expr::Variable(v2), Expr::Variable(v3)) =>
                            Self::mul_vvv(v1, v2, v3),
                        _ => unreachable!(),
                    },

                // Divisions

                // v1 = c2 / c3
                // v1 = v2 / c3
                // v1 = c2 / v3
                // v1 = v2 / v3
                (Expr::Variable(v1), Expr::Infix(InfixOp::Divide, e2, e3)) =>
                    match (&e2.v, &e3.v) {
                        (Expr::Constant(c2), Expr::Constant(c3)) =>
                            Self::mul_cvc(c2, v1, c3),
                        (Expr::Variable(v2), Expr::Constant(c3)) =>
                            Self::mul_vvc(v2, v1, c3),
                        (Expr::Constant(c2), Expr::Variable(v3)) =>
                            Self::mul_cvv(c2, v1, v3),
                        (Expr::Variable(v2), Expr::Variable(v3)) =>
                            Self::mul_vvv(v2, v1, v3),
                        _ => unreachable!(),
                    },
                
                // Division or Zero
                
                // v1 = c2 | c3
                // v1 = v2 | c3
                // v1 = c2 | v3
                // v1 = v2 | v3
                (Expr::Variable(v1), Expr::Infix(InfixOp::DivideZ, e2, e3)) =>
                    match (&e2.v, &e3.v) {
                        (Expr::Constant(c2), Expr::Constant(c3)) => {
                            let op2: F = Self::to_field(c3);
                            let c = if op2 == Self::zero() {
                                BigInt::from(0)
                            } else {
                                Self::to_bigint(Self::invert(op2))
                            };
                            Self::eq_vc(v1, &(c2*c))
                        },
                        (Expr::Variable(v2), Expr::Constant(c3)) => {
                            let op2: F = Self::to_field(c3);
                            let c = if op2 == Self::zero() {
                                BigInt::from(0)
                            } else {
                                Self::to_bigint(Self::invert(op2))
                            };
                            Self::mul_vvc(v2, v1, &c)
                        },
                        (Expr::Constant(c2), Expr::Variable(v3)) =>
                            Self::mul_cvv(c2, v1, v3),
                        (Expr::Variable(v2), Expr::Variable(v3)) =>
                            Self::mul_vvv(v2, v1, v3),
                        _ => unreachable!(),
                    },

                
                // Now for constants on the LHS
                // c1 = v2
                (Expr::Constant(c1),Expr::Variable(v2)) =>
                    Self::eq_vc(v2, c1),
                // c1 = c2
                (Expr::Constant(c1),Expr::Constant(c2)) =>
                    Self::eq_cc(c1, c2),

                // Negations
                
                // c1 = -c2
                // c1 = -v2
                (Expr::Constant(c1),Expr::Negate(e2)) =>
                    match &e2.v {
                        Expr::Constant(c2) => 
                            Self::eq_cc(c1, &-c2),
                        Expr::Variable(v2) =>
                            Self::eq_vc(v2, &-c1),
                        _ => unreachable!(),
                    },

                // Additions

                // c1 = c2 + c3
                // c1 = v2 + c3
                // c1 = c2 + v3
                // c1 = v2 + v3
                (Expr::Constant(c1), Expr::Infix(InfixOp::Add, e2, e3)) =>
                    match (&e2.v, &e3.v) {
                        (Expr::Constant(c2), Expr::Constant(c3)) =>
                            Self::eq_cc(c1, &(c2+c3)),
                        (Expr::Variable(v2), Expr::Constant(c3)) =>
                            Self::eq_vc(v2, &(c1-c3)),
                        (Expr::Constant(c2), Expr::Variable(v3)) =>
                            Self::eq_vc(v3, &(c1-c2)),
                        (Expr::Variable(v2), Expr::Variable(v3)) =>
                            Self::add_cvv(c1, v2, v3),
                        _ => unreachable!(),
                    },

                // Subtractions

                // c1 = c2 - c3
                // c1 = v2 - c3
                // c1 = c2 - v3
                // c1 = v2 - v3
                (Expr::Constant(c1), Expr::Infix(InfixOp::Subtract, e2, e3)) =>
                    match (&e2.v, &e3.v) {
                        (Expr::Constant(c2), Expr::Constant(c3)) =>
                            Self::eq_cc(c1, &(c2-c3)),
                        (Expr::Variable(v2), Expr::Constant(c3)) =>
                            Self::eq_vc(v2, &(c1+c3)),
                        (Expr::Constant(c2), Expr::Variable(v3)) =>
                            Self::eq_vc(v3, &(c2-c1)),
                        (Expr::Variable(v2), Expr::Variable(v3)) =>
                            Self::add_vvc(v2, v3, c1),
                        _ => unreachable!(),
                    },

                // Multiplications

                // c1 = c2 * c3
                // c1 = v2 * c3
                // c1 = c2 * v3
                // c1 = v2 * v3
                (Expr::Constant(c1), Expr::Infix(InfixOp::Multiply, e2, e3)) =>
                    match (&e2.v, &e3.v) {
                        (Expr::Constant(c2), Expr::Constant(c3)) =>
                            Self::eq_cc(c1, &(c2*c3)),
                        (Expr::Variable(v2), Expr::Constant(c3)) =>
                            Self::mul_cvc(c1, v2, c3),
                        (Expr::Constant(c2), Expr::Variable(v3)) =>
                            Self::mul_cvc(c1, v3, c2),
                        (Expr::Variable(v2), Expr::Variable(v3)) =>
                            Self::mul_cvv(c1, v2, v3),
                        _ => unreachable!(),
                    },

                // Divisions

                // c1 = c2 / c3
                // c1 = v2 / c3
                // c1 = c2 / v3
                // c1 = v2 / v3
                (Expr::Constant(c1), Expr::Infix(InfixOp::Divide, e2, e3)) =>
                    match (&e2.v, &e3.v) {
                        (Expr::Constant(c2), Expr::Constant(c3)) =>
                            Self::eq_cc(c2, &(c3*c1)),
                        (Expr::Variable(v2), Expr::Constant(c3)) =>
                            Self::eq_vc(v2, &(c3*c1)),
                        (Expr::Constant(c2), Expr::Variable(v3)) =>
                            Self::mul_cvc(c2, v3, c1),
                        (Expr::Variable(v2), Expr::Variable(v3)) =>
                            Self::mul_vvc(v2, v3, c1),
                        _ => unreachable!(),
                    },
                
                // Division or Zero
                
                // c1 = c2 | c3
                // c1 = v2 | c3
                // c1 = c2 | v3
                // c1 = v2 | v3
                (Expr::Constant(c1), Expr::Infix(InfixOp::DivideZ, e2, e3)) =>
                    match (&e2.v, &e3.v) {
                        (Expr::Constant(c2), Expr::Constant(c3)) => {
                            let op2: F = Self::to_field(c3);
                            let c = if op2 == Self::zero() {
                                BigInt::from(0)
                            } else {
                                Self::to_bigint(Self::invert(op2))
                            };
                            Self::eq_cc(c1, &(c2*&c))
                        },
                        (Expr::Variable(v2), Expr::Constant(c3)) => {
                            let op2: F = Self::to_field(c3);
                            let c = if op2 == Self::zero() {
                                BigInt::from(0)
                            } else {
                                Self::to_bigint(Self::invert(op2))
                            };
                            Self::eq_vc(v2, &(c1*&c))
                        },
                        (Expr::Constant(c2), Expr::Variable(v3)) =>
                            Self::mul_cvc(c2, v3, c1),
                        (Expr::Variable(v2), Expr::Variable(v3)) =>
                            Self::mul_vvc(v2, v3, c1),
                        _ => unreachable!(),
                    },            
                _ => panic!("unsupported constraint encountered: {}", expr)
            }
        } else {
            // CALL OTHER SYNTH
            Self::eq_cc(&BigInt::from(0), &BigInt::from(0))
        }
    }
}