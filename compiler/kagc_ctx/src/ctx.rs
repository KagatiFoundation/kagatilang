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

use std::{cell::RefCell, rc::Rc};

use kagc_comp_unit::ctx::FileCtx;
use kagc_const::pool::ConstPool;
use kagc_errors::diagnostic::DiagnosticBag;
use kagc_scope::ctx::ScopeCtx;

use crate::builder::CompilerCtxBuilder;

#[derive(Debug)]
pub struct CompilerCtx {
    pub const_pool: ConstPool,

    pub diagnostics: DiagnosticBag,

    pub files: FileCtx,

    pub scope: Rc<RefCell<ScopeCtx>>
}

impl CompilerCtx {
    pub fn new(
        scope_ctx: Rc<RefCell<ScopeCtx>>, 
        file_ctx: FileCtx,
        const_pool: ConstPool,
        diagnostics: DiagnosticBag
    ) -> Self {
        Self {
            const_pool,
            diagnostics,
            files: file_ctx,
            scope: scope_ctx
        }
    }
}

impl Default for CompilerCtx {
    fn default() -> Self {
        CompilerCtxBuilder::new(ScopeCtx::default()).build()
    }
}