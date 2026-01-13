// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_ast_lowering::AstToMirLowerer;
use kagc_backend::codegen_asm::aarch64::Aarch64CodeGenerator;
use kagc_comp_unit::ImportResolver;
use kagc_comp_unit::source_map::{FileId, SourceMap};
use kagc_const::pool::ConstPool;
use kagc_errors::diagnostic::DiagnosticBag;
use kagc_mir::module::MirModule;
use kagc_lexer::Tokenizer;
use kagc_parser::Parser;
use kagc_parser::options::ParserOptions;
use kagc_scope::ctx::ScopeCtx;
use kagc_ctx::StringInterner;
use kagc_sema::TypeChecker;
use kagc_sema::binder::NameBinder;
use kagc_utils::bug;

use crate::comp_unit::CompUnit;

pub struct CompilerPipeline<'tcx> {
    compilation_units: HashMap<String, CompUnit<'tcx>>,
    compile_order: Vec<String>,
    
    // Shared global compiler state
    diagnostics: &'tcx DiagnosticBag,
    const_pool: &'tcx ConstPool,
    scope_ctx: &'tcx ScopeCtx<'tcx>,
    source_map: &'tcx SourceMap<'tcx>,
    
    str_interner: &'tcx StringInterner<'tcx>,
    str_arena: &'tcx typed_arena::Arena<String>
}

impl<'tcx> CompilerPipeline<'tcx> {
    pub fn new(
        scope_ctx: &'tcx ScopeCtx<'tcx>,
        str_interner: &'tcx StringInterner<'tcx>,
        diagnostics: &'tcx DiagnosticBag,
        source_map: &'tcx SourceMap<'tcx>,
        const_pool: &'tcx ConstPool
    ) -> Self {
        Self {
            compile_order: vec![],
            compilation_units: HashMap::new(),
            diagnostics,
            const_pool,
            scope_ctx,
            source_map,
            str_interner,
            str_arena: str_interner.arena
        }
    }

    pub fn compile(&mut self, entry_file: &str) -> Result<(), std::io::Error> {
        self.resolve_dependencies(entry_file);

        let mut ty_checker = TypeChecker::new(self.scope_ctx, self.diagnostics);
        let mut ast_lowerer = AstToMirLowerer::new(self.scope_ctx, self.const_pool);

        for unit_key in &self.compile_order {
            let Some(unit) = self.compilation_units.get_mut(unit_key) else {
                bug!("No compilation unit is present with key '{unit_key}'");
            };
        
            let mut name_binder = NameBinder::new(self.scope_ctx, self.diagnostics, &unit.asts);
            name_binder.bind();

            ty_checker.check(&mut unit.asts);
            ast_lowerer.lower(&mut unit.asts);
        }
        
        let mir_mod = ast_lowerer.ir_builder.build2();
        self._compile_mir_modules_into_asm(&[mir_mod]);

        self.diagnostics.report_all(self.source_map);
        Ok(())
    }

    fn compile_path_into_unit(&mut self, file_path: &str) -> Option<CompUnit<'tcx>> {
        if self.compilation_units.contains_key(file_path) {
            return None;
        }

        let Ok(file) = ImportResolver::resolve(
            file_path, 
            self.str_arena
        )
        else {
            panic!("File not found: '{file_path}'");
        };

        let file_id = self.source_map.insert(file);
        Self::tokenize_and_parse(
            self.source_map, 
            self.diagnostics, 
            self.str_interner, 
            file_id
        )
    }

    fn tokenize_and_parse(
        source_map: &'tcx SourceMap<'tcx>, 
        diagnostics: &'tcx DiagnosticBag, 
        str_interner: &'tcx StringInterner<'tcx>,
        file_id: FileId,
    ) -> Option<CompUnit<'tcx>> {
        let Some(source_file) = source_map.get(file_id) else {
            panic!("Invalid file id '{file_id:#?}'");
        };

        let mut lexer = Tokenizer::new(
            diagnostics, 
            str_interner
        );
        let tokens = lexer.tokenize(source_file.content);

        let mut parser = Parser::new(
            ParserOptions { }, 
            diagnostics, 
            tokens
        );
        Some(CompUnit { asts: parser.parse() })
    }

    fn resolve_dependencies(&mut self, file_path: &str) {
        // Prevent circular imports and redundant parsing
        if self.compilation_units.contains_key(file_path) {
            return;
        }

        let Some(unit) = self.compile_path_into_unit(file_path) else {
            return;
        };

        for import in unit.extract_imports() {
            self.resolve_dependencies(import.path);
        }

        self.compilation_units.insert(file_path.to_string(), unit);
        self.compile_order.push(file_path.to_string());
    }

    fn _compile_mir_modules_into_asm(&mut self, modules: &[MirModule]) {
        let mut cg = Aarch64CodeGenerator::new(self.const_pool);
        for module in modules.iter() {
            cg.generate_module_code(module);
        }
        cg.dump_globals();
    }
}