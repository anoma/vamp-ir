use crate::ast::Module;
use crate::transform::{compile_repl, FieldOps};

use crate::plonk::synth::PrimeFieldOps as PlonkPrimeFieldOps;
use ark_bls12_381::Fr;

use crate::halo2::synth::PrimeFieldOps as Halo2PrimeFieldOps;
use halo2_proofs::pasta::Fp;

use std::fs;

use clap::Args;
use std::path::PathBuf;

use crate::util::Config;

#[derive(Args)]
pub struct REPL {
    /// Path to source file that is being loaded into repl
    #[arg(short, long)]
    source: Option<PathBuf>,
    /// Field to compute with
    #[arg(short, long)]
    field: String,
}

pub fn repl_cmd(source: &Option<PathBuf>, field_ops: &dyn FieldOps) {
    let mut module: Module = Module::default();

    if let Some(path) = source {
        let unparsed_file: String = fs::read_to_string(path).expect("cannot read file");
        module = Module::parse(&unparsed_file).unwrap();
        println!("Entering REPL with module loaded from file.");
        // println!("{}", module);
    } else {
        println!("Entering REPL with no module loaded.");
    }

    compile_repl(&mut module, field_ops)
}

pub fn repl(args: &REPL) {
    match args.field.as_str() {
        "Halo2" => repl_cmd(&args.source, &Halo2PrimeFieldOps::<Fp>::default()),
        "Plonk" => repl_cmd(&args.source, &PlonkPrimeFieldOps::<Fr>::default()),
        field_str => {
            if let Ok(_) = field_str.parse::<usize>() {
                //repl_cmd(&args.source, &fin_field(n))
                eprintln!("Arbitrary finite fields not implemented");
            } else {
                eprintln!("Invalid field value");
                return;
            }
        }
    };
}
