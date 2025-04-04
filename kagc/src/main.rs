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

use core::panic;
use std::{cell::RefCell, rc::Rc};

use kagc_ast::*;
use kagc_codegen::{aarch64::aarch64_gen::*, CodeGen};
use kagc_ctx::CompilerCtx;
use kagc_ir::ir_asm::aarch64::Aarch64IRToASM;
use kagc_lexer::Tokenizer;
use kagc_parser::Parser;
use kagc_sema::SemanticAnalyzer;
use kagc_symbol::*;
use kagc_target::asm::aarch64::*;
use kagc_token::Token;

fn main() {
    // tokenizer
    let tokener: Rc<RefCell<Tokenizer>> = Rc::new(RefCell::new(Tokenizer::new()));

    // parser
    let parsr: Rc<RefCell<Parser>> = Rc::new(RefCell::new(Parser::new(false)));

    // main symbol table
    let mut symt: Symtable<Symbol> = Symtable::new();

    // main function table
    let mut funct: FunctionInfoTable = FunctionInfoTable::new();

    // example source file
    let mut file1: SourceFile = SourceFile::new("/Users/rigelstar/Desktop/KagatiFoundation/bichara/examples/main.bic");
    let mut source_files: Vec<&mut SourceFile> = vec![&mut file1];

    // compiler context
    let ctx: Rc<RefCell<CompilerCtx>> = Rc::new(RefCell::new(CompilerCtx::new(&mut symt, &mut funct)));

    // semantic analyzer
    let mut s_analyzer: SemanticAnalyzer = SemanticAnalyzer::new(Rc::clone(&ctx));

    // register manager
    let rm: Rc<RefCell<Aarch64RegManager2>> = Rc::new(RefCell::new(Aarch64RegManager2::new()));

    // aarch64 code generator
    let mut cg: Aarch64CodeGen = Aarch64CodeGen::new(Rc::clone(&rm), Rc::clone(&ctx));

    for sf in &mut source_files {
        let read_res: Result<i32, std::io::Error> = sf.read();
        
        if let Err(e) = read_res {
            panic!("Error reading a source file: {:?}", e);
        }
    }

    for sf in &mut source_files {
        sf.tokenize(Rc::clone(&tokener));
    }

    {
        let mut parser_borrow = parsr.borrow_mut();

        for sf in &mut source_files {
            let tokens: Vec<Token> = sf.tokens.clone().unwrap();

            ctx.borrow_mut().current_file = Some(sf);

            let mut parse_result: Vec<AST> = parser_borrow.parse_with_ctx(Rc::clone(&ctx), tokens);

            if !parser_borrow.has_parsing_errors() {
                s_analyzer.start_analysis(&mut parse_result);

                let mut node_irs: Vec<kagc_ir::ir_instr::IR> = cg.gen_ir(&mut parse_result);
                
                let mut asm_gen: Aarch64IRToASM<'_> = Aarch64IRToASM::new(Rc::clone(&ctx), Rc::clone(&rm));
                
                let output: Vec<String> = asm_gen.gen_asm(&mut node_irs);
                
                output.iter().for_each(|op| println!("{op}"));
            }
        }

        std::mem::drop(parser_borrow);
    }
}