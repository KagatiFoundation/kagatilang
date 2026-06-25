// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_ast::*;
use kagc_utils::bug;
use kagc_errors::code::ErrCode;
use kagc_scope::ScopeCtx;
use kagc_scope::ScopeType;
use kagc_symbol::{StorageClass, Sym, SymId, SymTy};
use kagc_symbol::function::{Func, FuncId};
use kagc_types::record::{RecordFieldType, RecordType};
use kagc_errors::diagnostic::{Diagnostic, DiagnosticBag, Severity};

pub struct NameBinder<'r, 'tcx> where 'tcx: 'r {
    _local_offset: usize,
    diagnostics: &'r DiagnosticBag,
    scope: &'tcx ScopeCtx<'tcx>,
    ast_nodes: &'r Vec<AstNode<'tcx>>,
	current_function_id: Option<FuncId>,
}

pub type BindingResult = Option<SymId>;

impl<'r, 'tcx> NameBinder<'r, 'tcx> where 'tcx: 'r {
    pub fn new(
        scope: &'tcx ScopeCtx<'tcx>,
        diags: &'r DiagnosticBag,
        asts: &'r Vec<AstNode<'tcx>>,
    ) -> Self {
        Self {
            diagnostics: diags,
            scope,
            _local_offset: 0,
            ast_nodes: asts,
			current_function_id: None,
        }
    }

    pub fn bind(&mut self) {
        for node in self.ast_nodes.iter() {
            self.bind_sym(node);
        }
    }

    fn bind_sym(&mut self, node: &AstNode<'tcx>) -> BindingResult  {
        match node.op {
            AstOp::If => self.bind_if_stmt(node),
            AstOp::VarDecl => self.bind_let_stmt(node),
            AstOp::Func => self.bind_func_decl_stmt(node),
            AstOp::RecDecl => self.bind_record_decl_stmt(node),
            AstOp::Block => self.bind_block_stmt(node, ScopeType::Block),
            AstOp::Loop => self.bind_loop_stmt(node),
            _ => None
        }
    }

    fn bind_block_stmt(&mut self, node: &AstNode<'tcx>, scope_type: ScopeType) -> BindingResult {
        let node_id = node.id;
        let block_stmt = node.expect_block_stmt();

        self.scope.push(node_id, scope_type); // create a new scope and enter

        for stmt in &block_stmt.statements {
            let _ = self.bind_sym(stmt);
        }

        self.scope.pop(); // exit the scope
        None
    }

    fn bind_if_stmt(&mut self, node: &AstNode<'tcx>) -> BindingResult {
        node.expect_if_stmt();

        if let Some(mid_tree) = &node.mid {
            self.bind_block_stmt(mid_tree, ScopeType::If);
        }
        if let Some(right_tree) = &node.right {
            // right tree is the 'else' block
            if let Some(else_block_tree) = &right_tree.left {
                self.bind_block_stmt(else_block_tree, ScopeType::If);
            }
        }
        None
    }

    fn bind_func_decl_stmt(&mut self, node: &AstNode<'tcx>) -> BindingResult {
        let func_decl = node.expect_func_decl_stmt();
        let sym = Sym::new(
            func_decl.name, 
            func_decl.ty, 
            SymTy::Function, 
            func_decl.storage_class, 
			FuncId(0xFFFFFFFF)
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

		if let Ok(sym) = insert_res {
			sym.function_id.replace(FuncId(insert_res.ok().unwrap().id.get().0));
		}

        let sym_id = insert_res.ok().unwrap().id.get(); // safe to unwrap as insert succeeded

		self.current_function_id = Some(FuncId(sym_id.0));

        let mut func_params = vec![];
        for param in &func_decl.params {
            func_params.push(param.name);
        }

        let func = Func::new(
            func_decl.name,
            func_decl.ty,
            func_decl.storage_class,
            func_params,
            func_decl.param_types.clone()
        );

        self.scope.push(node.id, ScopeType::Function);

        for param in &func_decl.params {
            let param_sym = Sym::new(
                param.name, 
                param.ty, 
                SymTy::Variable, 
                StorageClass::PARAM,
                func.id.get()
            );
            if let Err(sym) = self.scope.declare_sym(param_sym) {
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
            }
        }

        if let Err(func) = self.scope.declare_fn(func_decl.name, func) {
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

        // loop through function's body to find new symbols
        if let Some(func_body) = &node.left {
            let func_body_stmt = func_body.expect_block_stmt();
            for stmt in &func_body_stmt.statements {
                self.bind_sym(stmt);
            }
            self.scope.pop();
        }

		self.current_function_id = None; // done with the function

        Some(sym_id)
    }

    fn bind_loop_stmt(&mut self, node: &AstNode<'tcx>) -> BindingResult {
        node.expect_loop_stmt();
        if let Some(left) = &node.left {
            return self.bind_block_stmt(left, ScopeType::Loop);
        }
        None
    }

    fn bind_let_stmt(&self, node: &AstNode<'tcx>) -> BindingResult {
        let stmt = match &node.kind {
            NodeKind::StmtAST(Stmt::VarDecl(stmt)) => stmt,
            _ => bug!("Invalid node"),
        };

		let func_id = self.current_function_id.expect("current function id must not be None");

        let sym = Sym::new(
            stmt.sym_name, 
            stmt.ty, 
            stmt.symbol_type, 
            StorageClass::LOCAL, 
            func_id
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

    fn bind_record_decl_stmt(&self, node: &AstNode<'tcx>) -> BindingResult {
        if let NodeKind::StmtAST(Stmt::Record(stmt)) = &node.kind {
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
            if self.scope.create_record(stmt.name, record_entry).is_err() {
                self.diagnostics.push(
                    Diagnostic {
                        code: Some(ErrCode::SEM2001),
                        severity: Severity::Error,
                        primary_span: node.meta.span,
                        secondary_spans: Vec::with_capacity(0),
                        message: format!("symbol '{}' already defined", stmt.name),
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