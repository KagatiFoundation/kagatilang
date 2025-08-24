use std::{
    cell::RefCell, 
    collections::HashMap, 
    rc::Rc
};

use kagc_comp_unit::{
    file_pool::FileMeta, 
    source::*, 
    *
};

use kagc_ctx::{builder::CompilerCtxBuilder, CompilerCtx};
use kagc_ir::ir_asm::aarch64::Aarch64IRToASM;
use kagc_lexer::Tokenizer;

use kagc_lowering::{
    aarch64::aarch64_lowerer::Aarch64CodeGen, 
    CodeGen
};
use kagc_parser::{
    builder::ParserBuilder, 
    SharedParserCtx
};
use kagc_scope::{
    ctx::builder::ScopeCtxBuilder, 
    manager::ScopeManager, 
    scope::Scope
};
use kagc_sema::{
    resolver::Resolver, 
    SemanticAnalyzer
};
use kagc_target::asm::aarch64::Aarch64RegManager2;

#[derive(Debug, Clone)]
pub struct Compiler {
    ctx: Rc<RefCell<CompilerCtx>>,

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
        // Garbage collection code
        let gc_unit = self.compile_unit_recursive("internal/kgc")?.unwrap();
        let gc_unit_path = gc_unit.source.path.clone();
        self.units.insert(gc_unit.source.path.to_string(), gc_unit);
        self.compiler_order.push(gc_unit_path);

        let unit = self.compile_unit_recursive(entry_file)?.unwrap();
        self.units.insert(entry_file.to_string(), unit);
        self.compiler_order.push(entry_file.to_string());

        // Semantic analyzer
        let mut analyzer = SemanticAnalyzer::new(self.ctx.clone());

        // Symbol resolver
        let mut resolv = Resolver::new(self.ctx.clone());

        // Register manager for Aarch64
        let rm = Rc::new(RefCell::new(Aarch64RegManager2::new()));

        // AST to IR generator
        let mut lowerer = Aarch64CodeGen::new(rm.clone(), self.ctx.clone());

        let mut final_irs = vec![];

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

                final_irs.extend(lowerer.gen_ir(&mut unit.asts));
            }
        }

        // IR to Aarch64 ASM generator
        let mut cg = Aarch64IRToASM::new(self.ctx.clone(), rm.clone());
        let code = cg.gen_asm(&mut final_irs);
        println!("{code}");
        Ok(())
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
        unit.stage = ParsingStage::Tokenized;

        let mut parser = ParserBuilder::new()
            .context(self.ctx.clone())
            .shared_context(self.shared_pctx.clone())
            .compile_unit(&mut unit)
            .file(unit.meta_id)
            .build();

        let asts = parser.parse();

        if self.ctx.borrow().diagnostics.has_errors() {
            self.ctx.borrow().diagnostics.report_all(&self.ctx.borrow().files, &unit);
            std::process::exit(1)
        }

        unit.asts.extend(asts);

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