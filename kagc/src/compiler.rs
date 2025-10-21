// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use kagc_backend::codegen_asm::aarch64::Aarch64CodeGenerator;
use kagc_backend::CodeGenerator;
use kagc_comp_unit::file_pool::FileMeta;
use kagc_comp_unit::CompilationUnit;
use kagc_comp_unit::ImportResolver;
use kagc_ctx::builder::CompilerCtxBuilder;
use kagc_ctx::CompilerCtx;
use kagc_mir::function::FunctionId;
use kagc_mir::module::MirModule;
use kagc_mir_lowering::MirToLirLowerer;
use kagc_lexer::Tokenizer;
use kagc_ast_lowering::AstToMirLowerer;
use kagc_parser::builder::ParserBuilder;
use kagc_parser::SharedParserCtx;
use kagc_scope::ctx::builder::ScopeCtxBuilder;
use kagc_scope::manager::ScopeManager;
use kagc_scope::scope::Scope;
use kagc_sema::resolver::Resolver;
use kagc_sema::SemanticAnalyzer;
use kagc_utils::bug;

#[derive(Debug, Clone)]
pub struct Compiler {
    pub ctx: Rc<RefCell<CompilerCtx>>,

    units: HashMap<String, CompilationUnit>,

    compiler_order: Vec<String>,

    shared_pctx: Rc<RefCell<SharedParserCtx>>
}

impl Compiler {
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
            shared_pctx: Rc::new(RefCell::new(SharedParserCtx::default()))
        }
    }
}

impl Compiler {
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
                self.ctx.borrow_mut().files.current = unit.meta_id;
                resolv.resolve(&mut unit.asts);
                analyzer.start_analysis(&mut unit.asts);

                if self.ctx.borrow().diagnostics.has_errors() {
                    self.ctx.borrow().diagnostics.report_all(&self.ctx.borrow().files, unit);
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
        let mut mir_lowerer = MirToLirLowerer::default();
        let mut cg = Aarch64CodeGenerator::default();

        for (mod_id, module) in modules.iter().enumerate() {
            let mut module_funcs: Vec<FunctionId> = module.functions.keys().cloned().collect();
            module_funcs.sort_by_key(|fid| fid.0);

            println!("// module {mod_id}");
            for func_id in module_funcs {
                let func = &module.functions[&func_id];
                let func_lowered = mir_lowerer.lower_function(func);
                cg.gen_function(&func_lowered); 
            }
        }
    }

    fn compile_unit_recursive(&mut self, file_path: &str) -> Result<Option<CompilationUnit>, std::io::Error> {
        if self.has_unit(file_path) {
            return Ok(None);
        }

        let file = ImportResolver::resolve(file_path)?;
        let file_pool_idx = self.ctx.borrow_mut().files.insert(
            FileMeta { 
                name: file.name.clone(), 
                abs_path: file.path.clone() 
            }
        );

        let mut unit = CompilationUnit::from_source(file, file_pool_idx.unwrap());
        let mut lexer = Tokenizer::new();
        let tokens = lexer.tokenize(unit.source.content.clone());
    
        unit.tokens = Some(Rc::new(tokens));
        unit.next_stage();

        let mut parser = ParserBuilder::new()
            .context(self.ctx.clone())
            .shared_context(self.shared_pctx.clone())
            .compile_unit(&mut unit)
            .file(unit.meta_id)
            .build();

        let asts = parser.parse();
        let ctx = self.ctx.borrow();
        if ctx.diagnostics.has_errors() {
            ctx.diagnostics.report_all(&ctx.files, &unit);
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

pub mod parser_test_utils {
    use std::cell::RefCell;
    use std::io::Write;
    use std::rc::Rc;

    use kagc_ast::AST;
    use kagc_comp_unit::source::loader::SourceFileLoader;
    use kagc_comp_unit::CompilationUnit;
    use kagc_ctx::CompilerCtx;
    use kagc_lexer::Tokenizer;
    use kagc_parser::builder::ParserBuilder;
    use kagc_parser::SharedParserCtx;

    fn make_comp_unit_from_raw_source(filename: &str, content: &str) -> std::io::Result<CompilationUnit> {
        let mut tmp_path = std::env::temp_dir();
        tmp_path.push(filename);

        if let Some(parent) = tmp_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let mut file = std::fs::File::create(&tmp_path)?;
        file.write_all(content.as_bytes())?;
        file.flush()?;

        let source_file = SourceFileLoader::load(&tmp_path)?;
        Ok(CompilationUnit::from_source(source_file, 0))
    }

    pub fn parse_from_source(ctx: Rc<RefCell<CompilerCtx>>, source: &str) -> Vec<AST> {
        let mut comp_unit = make_comp_unit_from_raw_source(
            "parse_test_tmp.kag", 
            source
        ).expect("Coultdn't create a temp file! Aborting...");

        let mut lexer = Tokenizer::new();
        let tokens = lexer.tokenize(comp_unit.source.content.clone());
        comp_unit.tokens = Some(Rc::new(tokens));

        let mut parser = ParserBuilder::new()
            .context(ctx)
            .shared_context(Rc::new(RefCell::new(SharedParserCtx::default())))
            .compile_unit(&mut comp_unit)
            .build();
        parser.parse()
    }
}

#[cfg(test)]
mod test_compiler {
    use std::{cell::RefCell, rc::Rc};

    use kagc_ctx::builder::CompilerCtxBuilder;
    use kagc_mir_lowering::MirToLirLowerer;
    use kagc_ast_lowering::AstToMirLowerer;
    use kagc_mir::function::FunctionId;
    use kagc_scope::ctx::ScopeCtx;
    use kagc_sema::resolver::Resolver;
    use kagc_sema::SemanticAnalyzer;
    use crate::compiler::parser_test_utils::parse_from_source;

    #[test]
    fn test_ast_lowering_ldr_instruction_correctness() {
        let ctx = Rc::new(
            RefCell::new(
                CompilerCtxBuilder::new(ScopeCtx::default()).build()
            )
        );
        let mut asts = parse_from_source(
            ctx.clone(),
            "def main() -> void {
                let a = 12;
                let b = 12;
                let c = a + b;
            }"
        );
        let mut resolv = Resolver::new(ctx.clone());
        let mut lowerer = AstToMirLowerer::new(ctx.clone());
        let mut analyzer = SemanticAnalyzer::new(ctx.clone());
        let mut mir_to_lir = MirToLirLowerer::default();
        
        resolv.resolve(&mut asts);
        analyzer.start_analysis(&mut asts);
        
        if lowerer.lower_irs(&mut asts).is_ok() {
            let irs = lowerer.ir_builder.build();
            let func1 = &irs.functions[&FunctionId(0)];
            let func1 = mir_to_lir.lower_function(func1);
            println!("{func1:#?}");
        }
    }
}