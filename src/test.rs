#[cfg(test)]
mod test {
    use std::{collections::HashMap, fs::File};

    #[test]
    fn range_plonk() {
        use crate::plonk::cli::*;

        let setup = Setup {
            max_degree: 10,
            output: "test_params.pp".into(),
            unchecked: true,
        };
        setup_plonk_cmd(&setup);

        let compile = PlonkCompile {
            universal_params: "test_params.pp".into(),
            source: "./tests/range.pir".into(),
            output: "range.plonk".into(),
            unchecked: true,
        };
        compile_plonk_cmd(&compile);

        let inputs_file = File::create("range.inputs").expect("Could not create inputs file");
        let mut input_map = HashMap::new();
        input_map.insert("x", "10");
        
        serde_json::to_writer(inputs_file, &input_map).unwrap(); 

        let prove = PlonkProve {
            universal_params: "test_params.pp".into(),
            circuit: "range.plonk".into(),
            output: "range.proof".into(),
            unchecked: true,
            inputs: Some("range.inputs".into()),
        };
        prove_plonk_cmd(&prove);

        let verify = PlonkVerify {
            universal_params: "test_params.pp".into(),
            circuit: "range.plonk".into(),
            proof: "range.proof".into(),
            unchecked: true,
        };
        verify_plonk_cmd(&verify);
    }

}