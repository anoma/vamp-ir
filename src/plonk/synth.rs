use crate::ast::Variable;
use crate::ast::{Expr, InfixOp, Module, Pat, TExpr, VariableId};
use crate::backend::BackendCompiler;
use crate::transform::{collect_module_variables, FieldOps};
use ark_ec::TEModelParameters;
use ark_ff::PrimeField;
use num_bigint::{BigInt, BigUint, Sign, ToBigInt};
use num_traits::Signed;
use plonk_core::circuit::Circuit;
use plonk_core::constraint_system::StandardComposer;
use plonk_core::error::Error;
use plonk_core::proof_system::pi::PublicInputs;
use std::collections::{BTreeMap, HashMap};
use std::marker::PhantomData;
use std::rc::Rc;

struct Plonk3<F> {
    v1: Option<VariableId>,
    v2: Option<VariableId>,
    v3: Option<VariableId>,
    q_m: F,
    q_l: F,
    q_r: F,
    q_o: F,
    q_c: F,
}

struct PrimeFieldBincode<T>(T)
where
    T: PrimeField;

impl<T> bincode::Encode for PrimeFieldBincode<T>
where
    T: PrimeField,
{
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> core::result::Result<(), bincode::error::EncodeError> {
        let biguint: BigUint = self.0.into();
        biguint.to_u32_digits().encode(encoder)
    }
}

impl<T> bincode::Decode for PrimeFieldBincode<T>
where
    T: PrimeField,
{
    fn decode<D: bincode::de::Decoder>(
        decoder: &mut D,
    ) -> core::result::Result<Self, bincode::error::DecodeError> {
        let digits = Vec::<u32>::decode(decoder)?;
        T::try_from(BigUint::new(digits)).map(Self).map_err(|_| {
            bincode::error::DecodeError::OtherString(
                "cannot convert from BigUint to PrimeField type".to_string(),
            )
        })
    }
}

// Make field elements from signed values
pub fn make_constant<F: PrimeField>(c: &BigInt) -> F {
    let magnitude = F::from(c.magnitude().clone());
    if c.is_positive() {
        magnitude
    } else {
        -magnitude
    }
}

/* Evaluate the given expression sourcing any variables from the given maps. */
fn evaluate_expr<F>(
    expr: &TExpr,
    defs: &mut HashMap<VariableId, TExpr>,
    assigns: &mut HashMap<VariableId, F>,
) -> F
where
    F: PrimeField,
{
    match &expr.v {
        Expr::Constant(c) => make_constant(c),
        Expr::Variable(v) => {
            if let Some(val) = assigns.get(&v.id) {
                // First look for existing variable assignment
                *val
            } else {
                // Otherwise compute variable from first principles
                let val = evaluate_expr(&defs[&v.id].clone(), defs, assigns);
                assigns.insert(v.id, val);
                val
            }
        }
        Expr::Negate(e) => -evaluate_expr(e, defs, assigns),
        Expr::Infix(InfixOp::Add, a, b) => {
            evaluate_expr(a, defs, assigns) + evaluate_expr(b, defs, assigns)
        }
        Expr::Infix(InfixOp::Subtract, a, b) => {
            evaluate_expr(a, defs, assigns) - evaluate_expr(b, defs, assigns)
        }
        Expr::Infix(InfixOp::Multiply, a, b) => {
            evaluate_expr(a, defs, assigns) * evaluate_expr(b, defs, assigns)
        }
        Expr::Infix(InfixOp::Divide, a, b) => {
            evaluate_expr(a, defs, assigns) / evaluate_expr(b, defs, assigns)
        }
        Expr::Infix(InfixOp::DivideZ, a, b) => {
            let denom = evaluate_expr(b, defs, assigns);
            if denom == F::zero() {
                F::zero()
            } else {
                evaluate_expr(a, defs, assigns) / denom
            }
        }
        Expr::Infix(InfixOp::IntDivide, a, b) => {
            (Into::<BigUint>::into(evaluate_expr(a, defs, assigns))
                / Into::<BigUint>::into(evaluate_expr(b, defs, assigns)))
            .into()
        }
        Expr::Infix(InfixOp::Modulo, a, b) => {
            (Into::<BigUint>::into(evaluate_expr(a, defs, assigns))
                % Into::<BigUint>::into(evaluate_expr(b, defs, assigns)))
            .into()
        }
        _ => unreachable!("encountered unexpected expression: {}", expr),
    }
}

#[derive(Default)]
pub struct PrimeFieldOps<F>
where
    F: PrimeField,
{
    phantom: PhantomData<F>,
}

impl<F> FieldOps for PrimeFieldOps<F>
where
    F: PrimeField,
{
    /* Evaluate the given negation expression in the given prime field. */
    fn canonical(&self, a: BigInt) -> BigInt {
        let b = make_constant::<F>(&a);
        Into::<BigUint>::into(b).to_bigint().unwrap()
    }
    /* Evaluate the given negation expression in the given prime field. */
    fn negate(&self, a: BigInt) -> BigInt {
        let b = make_constant::<F>(&a);
        Into::<BigUint>::into(-b).to_bigint().unwrap()
    }
    /* Evaluate the given infix expression in the given prime field. */
    fn infix(&self, op: InfixOp, a: BigInt, b: BigInt) -> BigInt {
        let c = make_constant::<F>(&a);
        let d = make_constant::<F>(&b);
        match op {
            InfixOp::Add => Into::<BigUint>::into(c + d).to_bigint().unwrap(),
            InfixOp::Subtract => Into::<BigUint>::into(c - d).to_bigint().unwrap(),
            InfixOp::Multiply => Into::<BigUint>::into(c * d).to_bigint().unwrap(),
            InfixOp::Divide => Into::<BigUint>::into(c / d).to_bigint().unwrap(),
            InfixOp::DivideZ => {
                Into::<BigUint>::into(if d == F::zero() { F::zero() } else { c / d })
                    .to_bigint()
                    .unwrap()
            }
            InfixOp::IntDivide => a / b,
            InfixOp::Modulo => a % b,
            InfixOp::Exponentiate => {
                let (sign, limbs) = b.to_u64_digits();
                Into::<BigUint>::into(if sign == Sign::Minus {
                    F::one() / c.pow(limbs)
                } else {
                    c.pow(limbs)
                })
                .to_bigint()
                .unwrap()
            }
            InfixOp::Equal => panic!("cannot evaluate equals expression"),
        }
    }
}

pub struct PlonkModule<F, P>
where
    F: PrimeField,
    P: TEModelParameters<BaseField = F>,
{
    pub module: Rc<Module>,
    variable_map: HashMap<VariableId, F>,
    phantom: PhantomData<P>,
}

impl<F, P> bincode::Encode for PlonkModule<F, P>
where
    F: PrimeField,
    P: TEModelParameters<BaseField = F>,
{
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> core::result::Result<(), bincode::error::EncodeError> {
        let mut encoded_variable_map = HashMap::new();
        for (k, v) in self.variable_map.clone() {
            encoded_variable_map.insert(k, PrimeFieldBincode(v));
        }
        encoded_variable_map.encode(encoder)?;
        self.module.encode(encoder)?;
        Ok(())
    }
}

impl<F, P> bincode::Decode for PlonkModule<F, P>
where
    F: PrimeField,
    P: TEModelParameters<BaseField = F>,
{
    fn decode<D: bincode::de::Decoder>(
        decoder: &mut D,
    ) -> core::result::Result<Self, bincode::error::DecodeError> {
        let encoded_variable_map = HashMap::<VariableId, PrimeFieldBincode<F>>::decode(decoder)?;
        let mut variable_map = HashMap::new();
        for (k, v) in encoded_variable_map {
            variable_map.insert(k, v.0);
        }
        let module = Rc::new(Module::decode(decoder)?);
        Ok(PlonkModule {
            module,
            variable_map,
            phantom: PhantomData,
        })
    }
}

impl<F, P> PlonkModule<F, P>
where
    F: PrimeField,
    P: TEModelParameters<BaseField = F>,
{
    fn one() -> F {
        F::one()
    }

    /* Make new circuit with default assignments to all variables in module. */
    pub fn new(module: Rc<Module>) -> PlonkModule<F, P> {
        let mut variables = HashMap::new();
        collect_module_variables(&module, &mut variables);
        let mut variable_map = HashMap::new();
        for variable in variables.keys() {
            variable_map.insert(*variable, F::default());
        }
        PlonkModule {
            module,
            variable_map,
            phantom: PhantomData,
        }
    }

    /* Populate input and auxiliary variables from the given program inputs. */
    pub fn populate_variables(&mut self, mut field_assigns: HashMap<VariableId, F>) {
        // Get the definitions necessary to populate auxiliary variables
        let mut definitions = HashMap::new();
        for def in &self.module.defs {
            if let Pat::Variable(var) = &def.0 .0.v {
                definitions.insert(var.id, *def.0 .1.clone());
            }
        }
        // Start deriving witnesses
        for (var, value) in &mut self.variable_map {
            let var_expr = Expr::Variable(crate::ast::Variable::new(*var)).type_expr(None);
            *value = evaluate_expr(&var_expr, &mut definitions, &mut field_assigns);
        }
    }

    /* Annotate the given public inputs with the variable names contained in
     * this module. This function assumes that the public variables in this
     * module and the public inputs in the argument occur in the same order. */
    pub fn annotate_public_inputs(
        &self,
        intended_pi_pos: &Vec<usize>,
        pi: &PublicInputs<F>,
    ) -> HashMap<VariableId, (Variable, F)> {
        // First map public input positions to values
        let mut pi_map = BTreeMap::new();
        for (pos, val) in pi.get_pos().zip(pi.get_vals()) {
            pi_map.insert(*pos, *val);
        }
        // Next, annotate the public inputs with this module's variables
        let mut annotated = HashMap::new();
        for (var, pos) in self.module.pubs.iter().zip(intended_pi_pos) {
            let val = pi_map.get(pos).copied().unwrap_or(F::zero());
            annotated.insert(var.id, (var.clone(), val));
        }
        annotated
    }
}

impl<F, P> BackendCompiler<F, P, Plonk3<F>> for PlonkModule<F, P>
where
    F: PrimeField,
    P: TEModelParameters<BaseField = F>,
{
    fn invert(x: F) -> F {
        x.inverse().unwrap()
    }

    fn to_bigint(x: F) -> BigInt {
        x.into_repr().into().into()
    }

    fn to_field(c: &BigInt) -> F {
        let magnitude = F::from(c.magnitude().clone());
        if c.is_positive() {
            magnitude
        } else {
            -magnitude
        }
    }

    // v1 = v2 + v3
    // 0 = 0*v2*v3 + 1*v2 + 1*v3 + (-1)*v1 + 0*c
    fn add_vvv(v1: &Variable, v2: &Variable, v3: &Variable) -> Plonk3<F> {
        Plonk3 {
            v1: Some(v1.id),   // w_o
            v2: Some(v2.id),   // w_l
            v3: Some(v3.id),   // w_r
            q_m: Self::zero(), // q_m
            q_l: Self::one(),  // q_l
            q_r: Self::one(),  // q_r
            q_o: -Self::one(), // q_o
            q_c: Self::zero(), // q_c
        }
    }

    // v1 = v2 + c3
    fn add_vvc(v1: &Variable, v2: &Variable, c3: &BigInt) -> Plonk3<F> {
        Plonk3 {
            v1: Some(v1.id),         // w_o
            v2: Some(v2.id),         // w_l
            v3: None,                // w_r
            q_m: Self::zero(),       // q_m
            q_l: Self::one(),        // q_l
            q_r: Self::zero(),       // q_r
            q_o: -Self::one(),       // q_o
            q_c: Self::to_field(c3), // q_c
        }
    }

    // c1 = v2 + v3
    fn add_cvv(c1: &BigInt, v2: &Variable, v3: &Variable) -> Plonk3<F> {
        Plonk3 {
            v1: None,                 // w_o
            v2: Some(v2.id),          // w_l
            v3: Some(v3.id),          // w_r
            q_m: Self::zero(),        // q_m
            q_l: Self::one(),         // q_l
            q_r: Self::one(),         // q_r
            q_o: Self::zero(),        // q_o
            q_c: -Self::to_field(c1), // q_c
        }
    }

    // v1 = c2
    fn eq_vc(v1: &Variable, c2: &BigInt) -> Plonk3<F> {
        Plonk3 {
            v1: Some(v1.id),         // w_o
            v2: None,                // w_l
            v3: None,                // w_r
            q_m: Self::zero(),       // q_m
            q_l: Self::zero(),       // q_l
            q_r: Self::zero(),       // q_r
            q_o: -Self::one(),       // q_o
            q_c: Self::to_field(c2), // q_c
        }
    }

    // v1 = v2
    fn eq_vv(v1: &Variable, v2: &Variable) -> Plonk3<F> {
        Plonk3 {
            v1: Some(v1.id),   // w_o
            v2: Some(v2.id),   // w_l
            v3: None,          // w_r
            q_m: Self::zero(), // q_m
            q_l: Self::one(),  // q_l
            q_r: Self::zero(), // q_r
            q_o: -Self::one(), // q_o
            q_c: Self::zero(), // q_c
        }
    }

    // c1 = c2
    fn eq_cc(c1: &BigInt, c2: &BigInt) -> Plonk3<F> {
        Plonk3 {
            v1: None,                        // w_o
            v2: None,                        // w_l
            v3: None,                        // w_r
            q_m: Self::zero(),               // q_m
            q_l: Self::zero(),               // q_l
            q_r: Self::zero(),               // q_r
            q_o: Self::zero(),               // q_o
            q_c: Self::to_field(&(c2 - c1)), // q_c
        }
    }

    // v1 = v2 * v3
    fn mul_vvv(v1: &Variable, v2: &Variable, v3: &Variable) -> Plonk3<F> {
        Plonk3 {
            v1: Some(v1.id),   // w_o
            v2: Some(v2.id),   // w_l
            v3: Some(v3.id),   // w_r
            q_m: Self::one(),  // q_m
            q_l: Self::zero(), // q_l
            q_r: Self::zero(), // q_r
            q_o: -Self::one(), // q_o
            q_c: Self::zero(), // q_c
        }
    }

    // v1 = v2 * c3
    fn mul_vvc(v1: &Variable, v2: &Variable, c3: &BigInt) -> Plonk3<F> {
        Plonk3 {
            v1: Some(v1.id),         // w_o
            v2: Some(v2.id),         // w_l
            v3: None,                // w_r
            q_m: Self::zero(),       // q_m
            q_l: Self::to_field(c3), // q_l
            q_r: Self::zero(),       // q_r
            q_o: -Self::one(),       // q_o
            q_c: Self::zero(),       // q_c
        }
    }

    // c1 = v2 * c3
    fn mul_cvc(c1: &BigInt, v2: &Variable, c3: &BigInt) -> Plonk3<F> {
        Plonk3 {
            v1: None,                 // w_o
            v2: Some(v2.id),          // w_l
            v3: None,                 // w_r
            q_m: Self::zero(),        // q_m
            q_l: Self::to_field(c3),  // q_l
            q_r: Self::zero(),        // q_r
            q_o: Self::zero(),        // q_o
            q_c: -Self::to_field(c1), // q_c
        }
    }

    // c1 = v2 * v3
    fn mul_cvv(c1: &BigInt, v2: &Variable, v3: &Variable) -> Plonk3<F> {
        Plonk3 {
            v1: None,                 // w_o
            v2: Some(v2.id),          // w_l
            v3: Some(v3.id),          // w_r
            q_m: Self::one(),         // q_m
            q_l: Self::zero(),        // q_l
            q_r: Self::zero(),        // q_r
            q_o: Self::zero(),        // q_o
            q_c: -Self::to_field(c1), // q_c
        }
    }

    fn zero() -> F {
        Self::to_field(&BigInt::from(0))
    }
}

impl<F, P> Circuit<F, P> for PlonkModule<F, P>
where
    F: PrimeField,
    P: TEModelParameters<BaseField = F>,
{
    const CIRCUIT_ID: [u8; 32] = [0xff; 32];

    fn gadget(&mut self, composer: &mut StandardComposer<F, P>) -> Result<(), Error> {
        let mut inputs = BTreeMap::new();
        for (var, field_elt) in &self.variable_map {
            inputs.insert(var, composer.add_input(*field_elt));
        }
        let zero = composer.zero_var();
        // It is assumed that the generated PublicInputs will share the same
        // order as this module's public variables
        for var in &self.module.pubs {
            composer.arithmetic_gate(|gate| {
                gate.witness(inputs[&var.id], zero, Some(zero))
                    .add(-F::one(), F::zero())
                    .pi(self.variable_map[&var.id])
            });
        }
        for expr in &self.module.exprs {
            let Plonk3 {
                v1,
                v2,
                v3,
                q_m,
                q_l,
                q_r,
                q_o,
                q_c,
            } = Self::arithmetic_synth(&expr);
            let w_o = v1.map_or(composer.zero_var(), |id| inputs[&id]);
            let w_l = v2.map_or(composer.zero_var(), |id| inputs[&id]);
            let w_r = v3.map_or(composer.zero_var(), |id| inputs[&id]);

            composer.arithmetic_gate(|gate| {
                gate.witness(w_l, w_r, Some(w_o))
                    .add(q_l, q_r)
                    .mul(q_m)
                    .out(q_o)
                    .constant(q_c)
            });
        }
        Ok(())
    }

    fn padded_circuit_size(&self) -> usize {
        // The with_expected_size function adds the following gates:
        // 1 gate to constrain the zero variable to equal 0
        // 3 gates to add blinging factors to the circuit polynomials
        const BUILTIN_GATE_COUNT: usize = 4;
        (self.module.exprs.len() + self.module.pubs.len() + BUILTIN_GATE_COUNT).next_power_of_two()
    }
}
