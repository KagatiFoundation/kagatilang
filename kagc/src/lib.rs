// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

pub mod compiler;
mod comp_unit;

use std::io::Error;

use compiler::CompilerPipeline;
use kagc_comp_unit::{source::SourceFile, source_map::SourceMap};
use kagc_const::pool::ConstPool;
use kagc_errors::diagnostic::DiagnosticBag;
use kagc_scope::ctx::ScopeCtx;
use kagc_symbol::Sym;
use kagc_types::{record::RecordType, str_interner::StringInterner};

pub fn compile_file(file_name: &str) -> Result<(), Error> {
    let str_arena = typed_arena::Arena::<String>::new();
    let file_arena = typed_arena::Arena::<SourceFile>::new();
    let sym_arena = typed_arena::Arena::<Sym<'_>>::new();
    let rec_arena = typed_arena::Arena::<RecordType<'_>>::new();
    let scope_ctx = ScopeCtx::new(&sym_arena, &rec_arena);
    let const_pool = ConstPool::default();
    let diags = DiagnosticBag::default();
    let source_map = SourceMap::new(&file_arena);
    let str_interner = StringInterner::new(&str_arena);
    let mut compiler = CompilerPipeline::new(
        &scope_ctx,
        &str_interner,
        &diags,
        &source_map,
        &const_pool
    );
    compiler.compile(file_name)
}