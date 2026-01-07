use kagc_ast::{AstNode, NodeKind, AstOp, Stmt};
use kagc_ast::import::Import;

pub struct CompUnit<'tcx> {
    pub asts: Vec<AstNode<'tcx>>,
}

impl<'tcx> CompUnit<'tcx> {
    pub fn extract_imports(&self) -> Vec<Import> {
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