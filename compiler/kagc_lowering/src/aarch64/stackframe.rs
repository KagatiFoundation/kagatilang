/*
MIT License

Copyright (c) 2023 Kagati Foundation

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

use kagc_ast::*;
use kagc_ctx::CompilerCtx;
use kagc_symbol::FunctionInfo;

use super::aarch64_lowerer::AARCH64_ALIGN_SIZE;

pub fn compute_stack_size(kctx: &CompilerCtx, func_ast: &AST, func_name: &str) -> Result<usize, ()> {
    // check if this function calls other functions
    let calls_fns: bool = func_ast.left.as_ref()
        .map_or(
            false, 
            |body| body.contains_operation(ASTOperation::AST_FUNC_CALL)
        );
    
    let func_info: &FunctionInfo = kctx.func_table.get(func_name)
            .unwrap_or_else(|| panic!("Function '{}' not found in function table", func_name));

    let mut stack_size: usize = func_info.local_syms.count() * AARCH64_ALIGN_SIZE;
    if calls_fns {
        stack_size += 2 * AARCH64_ALIGN_SIZE * 2;
    }
    Ok(align_to_16(stack_size))
}

// align values to addresses divisible by 8
fn _align_to_8(value: usize) -> usize {
    (value + 8 - 1) & !7
}

fn align_to_16(value: usize) -> usize {
    (value + 16 - 1) & !15
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_sth() {

    }

}