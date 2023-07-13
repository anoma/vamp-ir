use std::fs;
use vamp_ir;
use vamp_ir::halo2::api::compile;
use vamp_ir::util::Config;
use walkdir::{DirEntry, WalkDir};

#[test]
fn compile_test_programs() {
    // files which are slow to compile or stack overflow in debug mode
    let exclusions = ["sha256.pir", "blake2s.pir", "alu.pir", "if32.pir"];

    let files_to_test: Vec<DirEntry> = WalkDir::new("tests")
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .filter(|entry| {
            let f_name = entry.file_name().to_string_lossy();
            f_name.ends_with(".pir") && !exclusions.contains(&f_name.as_ref())
        })
        .collect();

    assert_ne!(files_to_test.len(), 0);

    for entry in files_to_test {
        let source = fs::read_to_string(&entry.into_path()).expect("could not read file");
        assert!(compile(source, &Config { quiet: true }).is_ok())
    }
}
