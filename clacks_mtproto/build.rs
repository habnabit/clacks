extern crate clacks_tl_codegen;

use std::io::{self, Read, Write};
use std::{fs, path};

const TL_DIR: &str = "tl";
const OUTPUT_FILE: &str = "src/mtproto.rs";

fn main_result() -> io::Result<()> {
    println!("cargo:rerun-if-changed={}", TL_DIR);
    let mut files = fs::read_dir(TL_DIR)?
        .map(|r| r.map(|d| d.path()))
        .collect::<Result<Vec<path::PathBuf>, _>>()?;
    files.sort();
    let mut input = String::new();
    for file in files {
        fs::File::open(&file)?.read_to_string(&mut input)?;
        println!("cargo:rerun-if-changed={}", file.to_string_lossy());
    }
    let code = clacks_tl_codegen::generate_code_for(&input)?;
    fs::File::create(OUTPUT_FILE)?.write_all(code.as_bytes())?;
    Ok(())
}

fn main() {
    main_result().unwrap();
}
