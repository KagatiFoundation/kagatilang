pub mod compiler;

use std::io::Error;

use compiler::Compiler;

pub fn compile_file(file_name: &str) -> Result<(), Error> {
    let mut comp = Compiler::new();
    comp.compile(file_name)
}