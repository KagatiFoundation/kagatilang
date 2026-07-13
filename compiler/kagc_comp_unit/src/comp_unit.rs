// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_ast::import::Import;
use kagc_ast::{NodeKind, AstOp, Stmt, AstNode};

pub struct CompUnit<'tcx> {
    pub asts: Vec<AstNode<'tcx>>,
}

impl<'tcx> CompUnit<'tcx> {
    pub fn extract_imports(&self) -> Vec<Import<'tcx>> {
        let mut imports = vec![]; 

        for ast in &self.asts {
            match ast.op {
                AstOp::Import => {
                    if let NodeKind::StmtAST(Stmt::Import(import)) = &ast.kind {
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
}