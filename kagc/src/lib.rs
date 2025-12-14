pub mod compiler;

use std::io::Error;

use compiler::CompilerPipeline;

pub fn compile_file(file_name: &str) -> Result<(), Error> {
    let mut comp = CompilerPipeline::new();
    comp.compile(file_name)
}