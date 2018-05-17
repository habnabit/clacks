extern crate clacks_tl_codegen;
#[cfg(feature = "rustfmt-codegen")]
extern crate rustfmt_nightly;

use std::io::{self, Read, Write};
use std::{fs, path};

const TL_DIR: &str = "tl";
const OUTPUT_FILE: &str = "src/mtproto.rs";

#[cfg(feature = "rustfmt-codegen")]
fn reformat(source: String) -> io::Result<String> {
    let mut config: rustfmt_nightly::Config = Default::default();
    {
        let mut set = config.set();
        set.error_on_line_overflow(false);
        //set.array_width(200);
        //set.fn_call_width(200);
        set.max_width(200);
        //set.struct_lit_width(200);
        //set.struct_variant_width(200);
    }
    let outputs = match rustfmt_nightly::format_input::<io::Sink>(rustfmt_nightly::Input::Text(source), &config, None) {
        Ok((_, outputs, _)) => outputs,
        Err((e, _)) => return Err(e),
    };
    let (_, string_buf) = outputs.into_iter()
        .next()
        .unwrap();
    Ok(format!("{}", string_buf))
}

#[cfg(not(feature = "rustfmt-codegen"))]
fn reformat(source: String) -> io::Result<String> {
    Ok(source)
}

fn main_result() -> io::Result<()> {
    let mut files = fs::read_dir(TL_DIR)?
        .map(|r| r.map(|d| d.path()))
        .collect::<Result<Vec<path::PathBuf>, _>>()?;
    files.sort();
    let mut input = String::new();
    for file in files {
        fs::File::open(&file)?.read_to_string(&mut input)?;
        println!("cargo:rerun-if-changed={}", file.to_string_lossy());
    }
    let code = clacks_tl_codegen::generate_code_for(&input);
    let code = reformat(code)?;
    fs::File::create(OUTPUT_FILE)?.write_all(code.as_bytes())?;
    Ok(())
}

fn main() {
    main_result().unwrap();
}
