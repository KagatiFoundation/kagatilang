use kagc_ast::{AST, ASTKind, ASTOperation, Stmt};
use kagc_ast::import::Import;

pub struct CompUnit<'tcx> {
    pub asts: Vec<AST<'tcx>>,
}

impl<'tcx> CompUnit<'tcx> {
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
}