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

use kagc_comp_unit::ctx::FileCtx;
use kagc_const::pool::ConstPool;
use kagc_errors::diagnostic::DiagnosticBag;
use kagc_scope::ctx::ScopeCtx;

use crate::CompilerCtx;

pub struct CompilerCtxBuilder {
    file_ctx: Option<FileCtx>,
    scope_ctx: Option<ScopeCtx>,
    diagnostics: Option<DiagnosticBag>,
    const_pool: Option<ConstPool>
}

impl CompilerCtxBuilder {
    #[allow(clippy::new_without_default)]
    pub fn new(scope_ctx: ScopeCtx) -> Self {
        Self {
            file_ctx: None,
            scope_ctx: Some(scope_ctx),
            diagnostics: None,
            const_pool: None
        }
    }

    pub fn file_context(mut self, file_ctx: FileCtx) -> Self {
        self.file_ctx = Some(file_ctx);
        self
    }

    pub fn scope_context(mut self, scope_ctx: ScopeCtx) -> Self {
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
            self.file_ctx.unwrap_or_default(),
            self.const_pool.unwrap_or_default(),
            self.diagnostics.unwrap_or_default(),
        )
    }
}