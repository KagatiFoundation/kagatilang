// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use kagc_backend::codegen_asm::aarch64::Aarch64CodeGenerator;
use kagc_comp_unit::CompilationUnit;
use kagc_comp_unit::ImportResolver;
use kagc_comp_unit::source_map::FileId;
use kagc_ctx::builder::CompilerCtxBuilder;
use kagc_ctx::CompilerCtx;
use kagc_mir::module::MirModule;
use kagc_lexer::Tokenizer;
use kagc_ast_lowering::AstToMirLowerer;
use kagc_parser::builder::ParserBuilder;
use kagc_parser::session::ParserSession;
use kagc_scope::ctx::builder::ScopeCtxBuilder;
use kagc_scope::manager::ScopeManager;
use kagc_scope::scope::Scope;
use kagc_sema::resolver::Resolver;
use kagc_sema::SemanticAnalyzer;
use kagc_utils::bug;

#[derive(Debug, Clone)]
pub struct CompilerPipeline {
    pub ctx: Rc<RefCell<CompilerCtx>>,
    units: HashMap<String, CompilationUnit>,
    compiler_order: Vec<String>,
}

impl CompilerPipeline {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let scope_ctx = ScopeCtxBuilder::new()
            .scope_manager({
                let mut scope_mgr: ScopeManager = ScopeManager::default();
                scope_mgr.push((0, Scope::default())); // root scope
                scope_mgr
            })
            .scope_id_counter(1) // since zero(0) is for the root scope
            .build();

        let ctx = CompilerCtxBuilder::new(scope_ctx).build();

        Self {
            ctx: Rc::new(RefCell::new(ctx)),
            compiler_order: vec![],
            units: HashMap::new(),
        }
    }
}

impl CompilerPipeline {
    pub fn add_unit(&mut self, path: String, unit: CompilationUnit) {
        self.units.insert(path, unit);
    }

    pub fn has_unit(&self, path: &str) -> bool {
        self.units.contains_key(path)
    }

    pub fn compile(&mut self, entry_file: &str) -> Result<(), std::io::Error> {
        let unit = self.compile_unit_recursive(entry_file)?.unwrap();
        self.units.insert(entry_file.to_string(), unit);
        self.compiler_order.push(entry_file.to_string());

        // Semantic analyzer
        let mut analyzer = SemanticAnalyzer::new(self.ctx.clone());
        // Symbol resolver
        let mut resolv = Resolver::new(self.ctx.clone());
        let mut modules: Vec<MirModule> = vec![];
        for unit_file in &self.compiler_order {
            if let Some(unit) = self.units.get_mut(unit_file) {
                // set this so that the compiler passes(resolver, semantic analysis, and code generation) 
                // know which file is currently being processed
                self.ctx.borrow_mut().source_map.borrow_mut().current = FileId(unit.meta_id);
                resolv.resolve(&mut unit.asts);
                analyzer.start_analysis(&mut unit.asts);

                if self.ctx.borrow().diagnostics.has_errors() {
                    println!("{:#?}", self.ctx.borrow().diagnostics);
                    // self.ctx.borrow().diagnostics.report_all(&self.ctx.borrow().source_map, unit);
                    std::process::exit(1)
                }

                // AST to IR generator
                let mut lowerer = AstToMirLowerer::new(self.ctx.clone());
                if lowerer.lower_irs(&mut unit.asts).is_ok() {
                    let mir_module = lowerer.ir_builder.build();
                    modules.push(mir_module);
                }
                else {
                    bug!("cannot lower MIR into LIR");
                }
            }
        }
        self.compile_mir_modules_into_asm(&modules);
        Ok(())
    }

    fn compile_mir_modules_into_asm(&mut self, modules: &[MirModule]) {
        let mut cg = Aarch64CodeGenerator::new(self.ctx.clone());
        for module in modules.iter() {
            cg.generate_module_code(module);
        }
        cg.dump_globals();
    }

    fn compile_unit_recursive(&mut self, file_path: &str) -> Result<Option<CompilationUnit>, std::io::Error> {
        if self.has_unit(file_path) {
            return Ok(None);
        }

        let file = ImportResolver::resolve(file_path)?;
        let mut parser_session = ParserSession::from_source_file(
            file.clone(), 
            self.ctx.borrow().scope.clone(),
            self.ctx.borrow().source_map.clone()
        );
        let soruce_file_id = parser_session.file_id;
        let mut parser = ParserBuilder::new()
            .session(&mut parser_session)
            .lexer(Tokenizer::new())
            .build();

        let asts = parser.parse();
        let mut unit = CompilationUnit::from_source(file, soruce_file_id.0);
        unit.tokens = Some(parser.tokens());
        unit.next_stage();

        let mut ctx = self.ctx.borrow_mut();
        if parser.diagnostics().has_errors() {
            ctx.diagnostics.extend(parser.diagnostics().clone());
            println!("{:#?}", ctx.diagnostics);
            // ctx.diagnostics.report_all(&ctx.source_map, &unit);
            std::process::exit(1);
        }
        drop(ctx);

        unit.asts.extend(asts);
        unit.next_stage();
        let imports = unit.extract_imports();

        for import in &imports {
            if let Some(im_unit) = self.compile_unit_recursive(&import.path)? {
                self.units.insert(import.path.clone(), im_unit);
                self.compiler_order.push(import.path.clone());
            }
        }
        unit.imports = imports;
        Ok(Some(unit))
    }
}