use std::{
    collections::{HashMap, HashSet},
    fs,
    ops::Neg,
    path::PathBuf,
};

use ark_serialize::Write;

use num_traits::Num;

use crate::ast::Variable;
use crate::error::Error;
use crate::error::Error::{InvalidVariableAssignmentValue, MissingVariableAssignment};
use crate::{
    ast::{Module, Pat, VariableId},
    transform::collect_module_variables,
};

/// Convert named circuit assignments to assignments of vamp-ir variableIds.
/// Useful for calling vamp-ir Halo2Module::populate_variable_assignments.
pub(crate) fn get_circuit_assignments<T>(
    module: &Module,
    named_assignments: &HashMap<String, T>,
) -> Result<HashMap<VariableId, T>, Error>
where
    T: Clone,
{
    let mut input_variables = HashMap::new();
    collect_module_variables(module, &mut input_variables);
    // Defined variables should not be requested from user
    for def in &module.defs {
        if let Pat::Variable(var) = &def.0 .0.v {
            input_variables.remove(&var.id);
        }
    }

    input_variables
        .iter()
        .filter_map(|(id, expected_var)| {
            expected_var.name.as_deref().map(|var_name| {
                named_assignments
                    .get(var_name)
                    .cloned()
                    .ok_or_else(|| MissingVariableAssignment {
                        var_name: var_name.to_string(),
                    })
                    .map(|assignment| (*id, assignment))
            })
        })
        .collect()
}

/* Read satisfying inputs to the given program from a file. */
pub fn read_inputs_from_file<F>(path_to_inputs: &PathBuf) -> Result<HashMap<String, F>, Error>
where
    F: Clone + Num + Neg<Output = F>,
    <F as num_traits::Num>::FromStrRadixErr: std::fmt::Debug,
{
    let contents = fs::read_to_string(path_to_inputs).expect("Could not read inputs file");

    // Read the user-supplied inputs from the file
    let named_assignments: HashMap<String, String> =
        json5::from_str(&contents).expect("Could not parse JSON5");

    named_assignments
        .into_iter()
        .map(|(var_name, str_value)| {
            let n = parse_prefixed_num::<F>(&str_value).map_err(|_| {
                InvalidVariableAssignmentValue {
                    var_name: var_name.clone(),
                }
            })?;
            Ok((var_name.clone(), n))
        })
        .collect::<Result<HashMap<String, F>, Error>>()
}

/* Prompt for satisfying inputs to the given program. */
pub fn prompt_inputs<F>(annotated: &Module) -> HashMap<VariableId, F>
where
    F: Num + Neg<Output = F>,
    <F as num_traits::Num>::FromStrRadixErr: std::fmt::Debug,
{
    let mut input_variables = HashMap::new();
    collect_module_variables(annotated, &mut input_variables);
    // Defined variables should not be requested from user
    for def in &annotated.defs {
        if let Pat::Variable(var) = &def.0 .0.v {
            input_variables.remove(&var.id);
        }
    }
    // Collect all public variables in order to enable annotations
    let mut public_variables = HashSet::new();
    for var in &annotated.pubs {
        public_variables.insert(var.id);
    }

    let named_input_variables: Vec<(VariableId, Variable)> = input_variables
        .into_iter()
        .filter(|(_id, var)| var.name.is_some())
        .collect();

    let mut var_assignments: HashMap<VariableId, F> = HashMap::new();

    // Solicit input variables from user and solve for choice point values
    for (id, var) in named_input_variables {
        let visibility = if public_variables.contains(&id) {
            "(public)"
        } else {
            "(private)"
        };
        print!("** {var} {visibility}: ");
        std::io::stdout().flush().expect("flush failed!");
        let mut input_line = String::new();
        std::io::stdin()
            .read_line(&mut input_line)
            .expect("failed to read input");
        let x = parse_prefixed_num(input_line.trim()).expect("input not an integer");
        var_assignments.insert(id, x);
    }
    var_assignments
}

/* Parse signed integer literals beginning with at most one occurrence of 0x
 * (indicating a radix of 16), 0o (radix 8), or 0b (radix 2). */
pub fn parse_prefixed_num<T>(string: &str) -> Result<T, T::FromStrRadixErr>
where
    T: Num + Neg<Output = T>,
{
    // Process the number's sign
    let (pos, magnitude) = if let Some(rest) = string.strip_prefix('-') {
        (false, rest)
    } else if let Some(rest) = string.strip_prefix('+') {
        (true, rest)
    } else {
        (true, string)
    };
    // Process the number's radix
    let magnitude = if let Some(rest) = magnitude.strip_prefix("0b") {
        T::from_str_radix(rest, 2)
    } else if let Some(rest) = magnitude.strip_prefix("0o") {
        T::from_str_radix(rest, 8)
    } else if let Some(rest) = magnitude.strip_prefix("0x") {
        T::from_str_radix(rest, 16)
    } else {
        T::from_str_radix(magnitude, 10)
    }?;
    // Combine magnitude and sign
    Ok(if pos { magnitude } else { -magnitude })
}

// Config to be shuffled around clis.
pub struct Config {
    pub quiet: bool,
}

// Macro for a potentially quiet print line.
#[macro_export]
macro_rules! qprintln {
    ($config:expr, $($arg:tt)*) => {
        if !$config.quiet {
            println!($($arg)*);
        }
    }
}
