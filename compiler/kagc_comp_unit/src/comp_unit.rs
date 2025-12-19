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

use std::rc::Rc;

use kagc_ast::{import::Import, ASTKind, ASTOperation, Stmt, AST};
use kagc_token::Token;

use crate::source::{
    ParsingStage, 
    SourceFile
};

/// Represents a single source file's state throughout the compilation pipeline.
///
/// A `CompilationUnit` encapsulates all the intermediate data derived from
/// processing a single source file, from its raw source code to the parsed
/// abstract syntax trees (ASTs).
///
/// It is created from a [`SourceFile`] and is gradually enriched as the compiler
/// advances through its stages:
///
/// 1. **Loading** – The raw file contents are read into `source`.
/// 2. **Lexing** – The source text is tokenized into `tokens`.
/// 3. **Parsing** – Tokens are transformed into an AST representation (`asts`).
/// 4. **Semantic Analysis** – The ASTs are type-checked and linked with imported units.
/// 5. **Code Generation** – Lowered IR or machine code is generated from the validated AST.
///
/// A `CompilationUnit` also stores:
/// - `meta_id` – An internal unique identifier for bookkeeping.
/// - `imports` – A list of modules or files this unit depends on.
/// - `stage` – Tracks how far along in the compilation process this unit is.
/// - `tokens` – The tokenized form of the source, shared via `Rc` for reuse.
/// - `asts` – The parsed representation of this unit (and optionally its imports).
///
/// Multiple `CompilationUnit`s are managed together by the compiler to support
/// multi-file projects, incremental compilation, and dependency resolution.
#[derive(Debug, Clone)]
pub struct CompilationUnit {
    pub source: SourceFile,
    pub meta_id: usize,
    pub imports: Vec<Import>,
    pub stage: ParsingStage,
    pub tokens: Option<Rc<Vec<Token>>>,
    pub asts: Vec<AST>
}

impl CompilationUnit {
    pub fn from_source(source: SourceFile, meta_id: usize) -> CompilationUnit {
        Self {
            source,
            meta_id,
            stage: ParsingStage::Unprocessed,
            tokens: None,
            imports: vec![],
            asts: vec![],
        }
    }

    pub fn extract_imports(&self) -> Vec<Import> {
        let mut imports = vec![]; 

        for ast in &self.asts {
            match ast.operation {
                ASTOperation::AST_IMPORT => {
                    if let ASTKind::StmtAST(Stmt::Import(import)) = &ast.kind {
                        imports.push(
                            Import { path: import.path.clone() }
                        );
                    }
                },

                _ => continue
            }
        }
        imports
    }

    pub fn next_stage(&mut self) -> ParsingStage {
        if self.stage == ParsingStage::Loaded {
            self.stage = ParsingStage::Unprocessed;
        }
        else if self.stage == ParsingStage::Unprocessed {
            self.stage = ParsingStage::Tokenized;
        }
        else if self.stage == ParsingStage::Tokenized {
            self.stage = ParsingStage::Parsed;
        }
        self.stage
    }
}