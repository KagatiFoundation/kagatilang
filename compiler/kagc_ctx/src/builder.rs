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

use crate::CompilerCtx;

pub struct CompilerCtxBuilder {
    source_map: Option<Rc<RefCell<SourceMap>>>,
    scope_ctx: Option<Rc<RefCell<ScopeCtx>>>,
    diagnostics: Option<DiagnosticBag>,
    const_pool: Option<ConstPool>
}

impl CompilerCtxBuilder {
    #[allow(clippy::new_without_default)]
    pub fn new(scope_ctx: ScopeCtx) -> Self {
        Self {
            source_map: None,
            scope_ctx: Some(Rc::new(RefCell::new(scope_ctx))),
            diagnostics: None,
            const_pool: None
        }
    }

    pub fn source_map(mut self, source_map: Rc<RefCell<SourceMap>>) -> Self {
        self.source_map = Some(source_map);
        self
    }

    pub fn scope_context(mut self, scope_ctx: Rc<RefCell<ScopeCtx>>) -> Self {
        self.scope_ctx = Some(scope_ctx);
        self
    }

    pub fn diagnostics(mut self, diags: DiagnosticBag) -> Self {
        self.diagnostics = Some(diags);
        self
    }

    pub fn constants_pool(mut self, const_pool: ConstPool) -> Self {
        self.const_pool = Some(const_pool);
        self
    }

    pub fn build(self) -> CompilerCtx {
        CompilerCtx::new(
            self.scope_ctx.unwrap(),
            self.source_map.unwrap_or_default(),
            self.const_pool.unwrap_or_default(),
            self.diagnostics.unwrap_or_default(),
        )
    }
}