// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

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
use kagc_types::str_interner::StringInterner;

use crate::comp_unit::CompUnit;

pub struct CompilerPipeline<'tcx> {
    units: HashMap<String, CompUnit<'tcx>>,
    compiler_order: Vec<String>,
    
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
            compiler_order: vec![],
            units: HashMap::new(),
            diagnostics,
            const_pool,
            scope_ctx,
            source_map,
            str_interner,
            str_arena: str_interner.arena
        }
    }

    fn compile_all(&self) {

    }

    fn collect_compilable_files(&self) {

    }

    fn parse_and_register_symbols(&self) {

    }

    pub fn compile(&mut self, entry_file: &str) -> Result<(), std::io::Error> {
        if self.units.contains_key(entry_file) {
           return Ok(());
        }
        
        let Some(unit) = self.compile_unit_recursive(entry_file) else {
            panic!("Could not parse file '{entry_file}'");
        };

        self.units.insert(entry_file.to_string(), unit);
        self.compiler_order.push(entry_file.to_string());

        // symbol resolver
        // let mut resolver = Resolver::new(
        //     self.scope_ctx, 
        //     &mut self.const_pool, 
        //     &self.diagnostics,
        //     &mut asts
        // );

        // type checker
        // let mut analyzer = TypeChecker::new(self.scope_ctx, &self.diagnostics);

        // let mut modules: Vec<MirModule> = vec![];
        
        for unit_file in &self.compiler_order {
            // resolver.resolve(&mut asts);

            /*
            if let Some(unit) = self.units.get_mut(unit_file) {
                // set this so that the compiler passes(resolver, semantic analysis, and code generation) 
                // know which file is currently being processed
                self.source_map.borrow_mut().current = FileId(unit.meta_id);
                resolv.resolve(&mut unit.asts);
                analyzer.start_analysis(&mut unit.asts);

                if self.diagnostics.has_errors() {
                    println!("{:#?}", self.diagnostics);
                    // self.ctx.borrow().diagnostics.report_all(&self.ctx.borrow().source_map, unit);
                    std::process::exit(1)
                }

                // AST to IR generator
                let mut lowerer = AstToMirLowerer::new(&self.scope_ctx, &self.const_pool);
                if lowerer.lower_irs(&mut unit.asts).is_ok() {
                    let mir_module = lowerer.ir_builder.build();
                    modules.push(mir_module);
                }
                else {
                    bug!("cannot lower MIR into LIR");
                }
            }
        */
        }
        // self.compile_mir_modules_into_asm(&modules);
        Ok(())
    }

    fn compile_unit_recursive(&mut self, file_path: &str) -> Option<CompUnit<'tcx>> {
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
        let asts = parser.parse();

        if diagnostics.has_errors() {
            diagnostics.extend(parser.diagnostics.clone());
            // diagnostics.report_all(&self.source_map);
            println!("{:#?}", diagnostics);
            std::process::exit(1);
        }

        let unit = CompUnit { asts };
        let imports = unit.extract_imports();

        for import in &imports {
        //     if let Some(im_unit) = self.compile_unit_recursive(import)? {
        //         self.units.insert(import.path.to_string(), im_unit);
        //         self.compiler_order.push(import.path.to_string());
        //     }
        }

        Some(unit)
    }

    fn _compile_mir_modules_into_asm(&mut self, modules: &[MirModule]) {
        let mut cg = Aarch64CodeGenerator::new(self.const_pool);
        for module in modules.iter() {
            cg.generate_module_code(module);
        }
        cg.dump_globals();
    }
}