extern crate clacks_tl_codegen;

use std::io::Read;

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    println!("{}", clacks_tl_codegen::generate_code_for(&input));
}
