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

use kagc_comp_unit::source_map::SourceMap;
use kagc_const::pool::ConstPool;
use kagc_errors::diagnostic::DiagnosticBag;
use kagc_scope::ctx::ScopeCtx;
use kagc_symbol::Sym;

pub struct CompilerCtx<'tcx> {
    pub const_pool: ConstPool,

    pub diagnostics: DiagnosticBag,

    pub source_map: Rc<RefCell<SourceMap<'tcx>>>,
    pub scope: Rc<RefCell<ScopeCtx<'tcx>>>
}

impl<'tcx> CompilerCtx<'tcx> {
    pub fn new(
        scope_ctx: Rc<RefCell<ScopeCtx<'tcx>>>, 
        source_map: Rc<RefCell<SourceMap<'tcx>>>,
        const_pool: ConstPool,
        diagnostics: DiagnosticBag,
        sym_arena: &'tcx typed_arena::Arena<Sym<'tcx>>
    ) -> Self {
        Self {
            const_pool,
            diagnostics,
            source_map,
            scope: scope_ctx
        }
    }
}