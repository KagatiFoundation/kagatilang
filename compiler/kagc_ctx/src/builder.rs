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