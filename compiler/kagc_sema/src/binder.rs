// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_ast::*;
use kagc_utils::bug;
use kagc_errors::code::ErrCode;
use kagc_scope::ctx::ScopeCtx;
use kagc_scope::scope::ScopeType;
use kagc_symbol::{Sym, SymId, SymTy};
use kagc_symbol::function::{Func, FuncId};
use kagc_types::record::{RecordFieldType, RecordType};
use kagc_errors::diagnostic::{Diagnostic, DiagnosticBag, Severity};

pub struct NameBinder<'r, 'tcx> where 'tcx: 'r {
    _local_offset: usize,
    diagnostics: &'r DiagnosticBag,
    scope: &'tcx ScopeCtx<'tcx>,
    ast_nodes: &'tcx Vec<AstNode<'tcx>>
}

pub type BindingResult = Option<SymId>;

impl<'r, 'tcx> NameBinder<'r, 'tcx> where 'tcx: 'r {
    pub fn new(
        scope: &'tcx ScopeCtx<'tcx>,
        diags: &'r DiagnosticBag,
        asts: &'tcx mut Vec<AstNode<'tcx>>
    ) -> Self {
        Self {
            diagnostics: diags,
            scope,
            _local_offset: 0,
            ast_nodes: asts
        }
    }

    pub fn bind(&mut self) {
        for node in self.ast_nodes.iter() {
            self.bind_sym(node);
        }
    }

    fn bind_sym(&self, node: &'tcx AstNode<'tcx>) -> BindingResult  {
        match node.op {
            AstOp::If => self.bind_if_stmt(node),
            AstOp::VarDecl => self.bind_let_stmt(node),
            AstOp::Func => self.bind_func_decl_stmt(node),
            AstOp::RecDecl => self.bind_record_decl_stmt(node),
            AstOp::Loop => {
                if let Some(left) = &node.left {
                    return self.bind_sym(left);
                }
                None
            }
            AstOp::Block => {
                if let Some(stmt) = node.as_stmt() {
                    if let Some(block) = stmt.as_block() {
                        for s in &block.statements {
                            let _ = self.bind_sym(s)?;
                        }
                    }
                }
                None
            }
            AstOp::Glue => {
                if let Some(left) = node.left.as_ref() {
                    let _ = self.bind_sym(left)?;
                }
                if let Some(right) = node.right.as_ref() {
                    let _ = self.bind_sym(right)?;
                } 
                None
            }
            _ => None
        }
    }

    fn bind_if_stmt(&self, node: &'tcx AstNode<'tcx>) -> BindingResult {
        node.expect_if_stmt();

        if let Some(mid_tree) = &node.mid {
            self.scope.push(node.id, ScopeType::If);
            self.bind_sym(mid_tree)?; // mid tree is the 'if' block
            self.scope.pop(); // exit if-block's scope
        }

        if let Some(right_tree) = &node.right {
            // right tree is the 'else' block
            if let Some(else_block_tree) = &right_tree.left {
                self.scope.push(else_block_tree.id, ScopeType::If);
                self.bind_sym(else_block_tree)?;
                self.scope.pop();
            }
        }
        None
    }

    fn bind_func_decl_stmt(&self, node: &'tcx AstNode<'tcx>) -> BindingResult {
        let func_decl = node.expect_func_decl_stmt();
        let sym = Sym::new(
            func_decl.name, 
            func_decl.ty, 
            SymTy::Function, 
            func_decl.storage_class, 
            func_decl.id
        );

        let insert_res = self
            .scope
            .root()
            .add_sym(sym);

        if let Err(sym) = insert_res {
            self.diagnostics.push(
                Diagnostic {
                    code: Some(ErrCode::SEM2001),
                    severity: Severity::Error,
                    primary_span: node.meta.span,
                    secondary_spans: Vec::with_capacity(0),
                    message: format!("symbol '{}' already defined", sym.name),
                    notes: Vec::with_capacity(0)
                }
            );
        };

        let sym_id = insert_res.ok().unwrap().id.get(); // safe to unwrap as insert succeeded

        let func = Func::new(
            func_decl.name,
            func_decl.ty,
            func_decl.storage_class,
            func_decl.locals.clone(),
            func_decl.func_param_types.clone()
        );

        let insert_res = self.scope.declare_fn(func_decl.name, func);
        if let Err(func) = insert_res {
            self.diagnostics.push(
                Diagnostic {
                    code: Some(ErrCode::SEM2001),
                    severity: Severity::Error,
                    primary_span: node.meta.span,
                    secondary_spans: Vec::with_capacity(0),
                    message: format!("function '{}' already defined", func.name),
                    notes: Vec::with_capacity(0)
                }
            );
        };
        self.scope.push(node.id, ScopeType::Function);

        // loop through function's body to find new symbols
        if let Some(func_body) = &node.left {
            let _ = self.bind_sym(func_body)?;
        }
        Some(sym_id)
    }

    fn bind_let_stmt(&self, node: &'tcx AstNode<'tcx>) -> BindingResult {
        let stmt = match &node.data {
            NodeKind::StmtAST(Stmt::VarDecl(stmt)) => stmt,
            _ => bug!("Invalid node"),
        };
        let sym = Sym::new(
            stmt.sym_name, 
            stmt.ty, 
            stmt.symbol_type, 
            stmt.class, 
            FuncId(stmt.func_id)
        );
        let id = self.scope.declare_sym(sym);

        if let Ok(sym) = id {
            Some(sym.id.get())
        }
        else {
            self.diagnostics.push(
                Diagnostic {
                    code: Some(ErrCode::SEM2001),
                    severity: Severity::Error,
                    primary_span: node.meta.span,
                    secondary_spans: Vec::with_capacity(0),
                    message: "symbol already defined".to_string(),
                    notes: Vec::with_capacity(0)
                }
            );
            None
        }
    }

    /*
    /// These function should be in TypeChecker
    /// 
    fn validate_record_let_binding(&self, rec_name: &'tcx str, value_node: &AST) -> BindingResult {
        let rec = self.scope.lookup_record(rec_name).ok_or_else(|| {
            Diagnostic {
                code: Some(ErrCode::SEM2000),
                severity: Severity::Error,
                primary_span: value_node.meta.span,
                secondary_spans: Vec::with_capacity(0),
                message: "record not found".to_string(),
                notes: Vec::with_capacity(0)
            }
        })?;
        if let ASTKind::ExprAST(Expr::RecordCreation(rec_create)) = &value_node.kind {
            for field in &rec_create.fields {
                let found = rec.fields.iter().find(|ac_field| ac_field.name == field.name);
                if found.is_none() {
                    self.diagnostics.push(
                        Diagnostic {
                            code: Some(ErrCode::REC4000),
                            severity: Severity::Error,
                            primary_span: value_node.meta.span,
                            secondary_spans: Vec::with_capacity(0),
                            message: "unknown record field".to_string(),
                            notes: Vec::with_capacity(0)
                        }
                    );
                }
            }
            for field in &rec.fields {
                let found = rec_create.fields.iter().find(|ac_field| ac_field.name == field.name);
                if found.is_none() {
                    self.diagnostics.push(
                        Diagnostic {
                            code: Some(ErrCode::REC4001),
                            severity: Severity::Error,
                            primary_span: value_node.meta.span,
                            secondary_spans: Vec::with_capacity(0),
                            message: "missing record field".to_string(),
                            notes: Vec::with_capacity(0)
                        }
                    );
                }
            }
        }
        None
    }

    fn require_symbol_exists(&self, symbol_name: &'tcx str, meta: &NodeMeta) -> Result<(), Diagnostic> {
        if self.scope.deep_lookup(None, symbol_name).is_none() {
            return Err(Diagnostic {
                code: Some(ErrCode::SEM2000),
                severity: Severity::Error,
                primary_span: meta.span,
                secondary_spans: Vec::with_capacity(0),
                message: "symbol not found".to_string(),
                notes: Vec::with_capacity(0)
            });
        }
        Ok(())
    }
    */

    fn bind_record_decl_stmt(&self, node: &'tcx AstNode<'tcx>) -> BindingResult {
        if let NodeKind::StmtAST(Stmt::Record(stmt)) = &node.data {
            let record_entry = RecordType {
                name: stmt.name,
                size: 0,
                __alignment: stmt.alignment,
                fields: stmt.fields.iter().enumerate().map(|(idx, field)| {
                    RecordFieldType {
                        name: field.name,
                        ty: field.ty,
                        rel_stack_off: idx
                    }
                }).collect::<Vec<RecordFieldType>>()
            };
            if self.scope.create_record(stmt.name, record_entry).is_none() {
                self.diagnostics.push(
                    Diagnostic {
                        code: Some(ErrCode::SEM2001),
                        severity: Severity::Error,
                        primary_span: node.meta.span,
                        secondary_spans: Vec::with_capacity(0),
                        message: "symbol already defined".to_string(),
                        notes: Vec::with_capacity(0)
                    }
                );
                return None;
            }
            return None;
        }
        bug!("Cannot create record!!!")
    }
}