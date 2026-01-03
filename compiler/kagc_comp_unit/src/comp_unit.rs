// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_ast::{import::Import, ASTKind, ASTOperation, Stmt, AST};

use crate::source::SourceFile;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ParsingStageError {
    FileNotFound,
    TokenizationError,
    ParsingError,
    FileReadingError,
    None,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ParsingStage {
    /// Nothing has been done till now. Even the file hasn't
    /// been loaded into memory.
    Unprocessed,

    /// File has been loaded into memory.
    Loaded,

    /// Tokens has been generated for this file.
    Tokenized,

    Parsed,

    /// Some kind of error has occured during reading this file
    /// or during token generation.
    Error(ParsingStageError),
}

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
pub struct CompilationUnit<'tcx> {
    pub source: SourceFile<'tcx>,
    pub meta_id: usize,
    pub imports: Vec<Import<'tcx>>,
    pub stage: ParsingStage,
    pub asts: Vec<AST<'tcx>>
}

impl<'tcx> CompilationUnit<'tcx> {
    pub fn from_source(source: SourceFile<'tcx>, meta_id: usize) -> CompilationUnit<'tcx> {
        Self {
            source,
            meta_id,
            stage: ParsingStage::Unprocessed,
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
                            Import { path: import.path }
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