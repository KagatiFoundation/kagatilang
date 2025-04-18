use std::{
    cell::RefCell, collections::HashMap, rc::Rc
};

use kagc_comp_unit::{
    source::{
        loader::*, 
        *
    }, 
    *
};

use kagc_ctx::CompilerCtx;
use kagc_ir::ir_asm::aarch64::Aarch64IRToASM;
use kagc_lexer::Tokenizer;

use kagc_lowering::{aarch64::aarch64_lowerer::Aarch64CodeGen, CodeGen};
use kagc_parser::Parser;
use kagc_sema::SemanticAnalyzer;
use kagc_target::asm::aarch64::Aarch64RegManager2;

#[derive(Debug, Clone)]
pub struct Compiler {
    pub ctx: Rc<RefCell<CompilerCtx>>,

    pub units: HashMap<String, CompilationUnit>,

    pub compiler_order: Vec<String>
}

impl Compiler {
    pub fn add_unit(&mut self, path: String, unit: CompilationUnit) {
        self.units.insert(path, unit);
    }

    pub fn has_unit(&self, path: &str) -> bool {
        self.units.contains_key(path)
    }

    pub fn compile(&mut self, entry_file: &str) -> Result<(), std::io::Error> {
        let unit = self.compile_unit_recursive(entry_file)?;
        self.units.insert(entry_file.to_string(), unit);
        self.compiler_order.push(entry_file.to_string());

        let mut analyzer = SemanticAnalyzer::new(self.ctx.clone());

        let rm = Rc::new(RefCell::new(Aarch64RegManager2::new()));

        let mut lowerer = Aarch64CodeGen::new(rm.clone(), self.ctx.clone());

        let mut cg = Aarch64IRToASM::new(self.ctx.clone(), rm.clone());

        let mut final_irs = vec![];

        for unit_file in &self.compiler_order {
            if let Some(unit) = self.units.get_mut(unit_file) {
                analyzer.start_analysis(&mut unit.asts);
                final_irs.extend(lowerer.gen_ir(&mut unit.asts));
            }
        }

        let code = cg.gen_asm(&mut final_irs);
        println!("{code}");

        Ok(())
    }

    fn compile_unit_recursive(&mut self, file_path: &str) -> Result<CompilationUnit, std::io::Error> {
        if self.has_unit(file_path) {
            panic!("duplicate");
        }

        let file = SourceFileLoader::load(file_path)?;
        let mut unit = CompilationUnit::from_source(file);
    
        let mut lexer = Tokenizer::new();
        let tokens = lexer.tokenize(unit.source.content.clone());
    
        unit.tokens = Some(Rc::new(tokens));
        unit.stage = ParsingStage::Tokenized;

        let mut parser = Parser::new(true, self.ctx.clone());
        let asts = parser.parse(&mut unit);
        unit.asts.extend(asts);

        let imports = unit.extract_imports();

        for import in &imports {
            let im_unit = self.compile_unit_recursive(&import.path)?;
            self.units.insert(import.path.clone(), im_unit);
            self.compiler_order.push(import.path.clone());
        }

        unit.imports = imports;

        Ok(unit)
    }
}