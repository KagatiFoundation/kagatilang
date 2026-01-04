// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use indexmap::IndexMap;
use kagc_ast::*;
use kagc_errors::code::ErrCode;
use kagc_errors::diagnostic::{Diagnostic, DiagnosticBag, Severity};
use kagc_scope::ctx::ScopeCtx;
use kagc_scope::scope::{ScopeId, ScopeType};
use kagc_symbol::function::{Func, FuncId};
use kagc_symbol::{Sym, SymTy};
use kagc_const::pool::{ConstPool, KagcConst, OrderedMap, RecordConst};
use kagc_types::builtins::obj::KObjType;
use kagc_types::record::{RecordFieldType, RecordType};
use kagc_types::{LitValue, TyKind};
use kagc_utils::bug;

pub struct NameBinder<'r, 'tcx> where 'tcx: 'r {
    pub local_offset: usize,
    curr_func_id: Option<usize>,
    pub diagnostics: &'r DiagnosticBag,
    pub const_pool: &'r mut ConstPool,
    pub scope: &'tcx ScopeCtx<'tcx>,
    pub asts: &'r mut Vec<AST<'tcx>>
}

pub type BindingResult = Result<usize, Diagnostic>;

impl<'r, 'tcx> NameBinder<'r, 'tcx> where 'tcx: 'r {
    pub fn new(
        scope: &'tcx ScopeCtx<'tcx>,
        const_pool: &'r mut ConstPool,
        diags: &'r DiagnosticBag,
        asts: &'r mut Vec<AST<'tcx>>
    ) -> Self {
        Self {
            diagnostics: diags,
            const_pool,
            scope,
            local_offset: 0,
            curr_func_id: None,
            asts
        }
    }

    pub fn resolve(&mut self, nodes: &'r mut Vec<AST<'tcx>>) {
        for node in nodes {
            if let Err(e) = self.declare_symbol(node) {
                self.diagnostics.push(e);
            }
        }
    }

    fn declare_symbol(&mut self, node: &'r mut AST<'tcx>) -> BindingResult  {
        match node.operation {
            ASTOperation::AST_FUNCTION => self.declare_function_symbol(node),
            ASTOperation::AST_VAR_DECL => self.declare_let_binding(node),
            ASTOperation::AST_RECORD_DECL => self.declare_record(node),
            ASTOperation::AST_LOOP => {
                if !node.kind.is_stmt() {
                    bug!("Needed a LoopStmt--but found {:#?}", node);
                }
                if let Some(left) = &mut node.left {
                    return self.declare_symbol(left);
                }
                Ok(0xFFFFFFFF)
            }
            ASTOperation::AST_IF => {
                if !node.kind.is_stmt() {
                    bug!("Needed a IfStmt--but found {:#?}", node);
                }
                self.declare_if_else_tree(node)
            }
            ASTOperation::AST_FUNC_CALL => {
                if !node.kind.is_expr() {
                    bug!("Needed a FuncCallExpr--but found {:#?}", node);
                }
                if let Some(func_call) = node.kind.as_expr_mut() {
                    return self.resolve_literal_constant(func_call, false, "", &node.meta);
                }
                Ok(0xFFFFFFFF)
            }
            ASTOperation::AST_BLOCK => {
                if let Some(stmt) = node.as_stmt_mut() {
                    if let Some(block) = stmt.as_block_mut() {
                        for s in &mut block.statements {
                            let _ = self.declare_symbol(s)?;
                        }
                    }
                }
                Ok(0)
            }
            ASTOperation::AST_GLUE => {
                if let Some(left) = node.left.as_mut() {
                    let _ = self.declare_symbol(left)?;
                }
                if let Some(right) = node.right.as_mut() {
                    let _ = self.declare_symbol(right)?;
                } 
                Ok(0)
            }
            _ => Ok(0)
        }
    }

    fn declare_if_else_tree(&mut self, node: &'r mut AST<'tcx>) -> BindingResult {
        if !node.kind.is_stmt() {
            bug!("Needed a IfDecl--but found {:#?}", node);
        }
        if let ASTKind::StmtAST(Stmt::If(if_stmt)) = &mut node.kind {
            self.scope.enter(ScopeId(if_stmt.scope_id));
            self.scope.pop();
        }
        else {
            bug!("Provided Stmt {:#?} is not of type IfStmt! Aborting...", node);
        }

        if let Some(mid_tree) = &mut node.mid {
            self.declare_symbol(mid_tree)?; // mid tree is the 'if' block
            self.scope.pop(); // exit if-block's scope
        }

        if let Some(right_tree) = &mut node.right {
            // right tree is the 'else' block
            if let Some(else_block_tree) = &mut right_tree.left {
                if let ASTKind::StmtAST(Stmt::Scoping(scoping_stmt)) = &else_block_tree.kind {
                    self.scope.enter(ScopeId(scoping_stmt.scope_id));
                }
                self.declare_symbol(else_block_tree)?;
                self.scope.pop();
            }
        }
        Ok(0)
    }

    fn declare_function_symbol(&mut self, node: &'r mut AST<'tcx>) -> BindingResult {
        let func_decl = node.expect_func_decl_stmt_mut();
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
            return Err(Diagnostic {
                code: Some(ErrCode::SEM2001),
                severity: Severity::Error,
                primary_span: node.meta.span,
                secondary_spans: Vec::with_capacity(0),
                message: format!("symbol '{}' already defined", sym.name),
                notes: Vec::with_capacity(0)
            });
        };

        let func = Func::new(
            func_decl.name,
            func_decl.ty,
            func_decl.storage_class,
            func_decl.locals.clone(),
            func_decl.func_param_types.clone()
        );

        let insert_res = self.scope.declare_fn(func_decl.name, func);
        if let Err(func) = insert_res {
            return Err(Diagnostic {
                code: Some(ErrCode::SEM2001),
                severity: Severity::Error,
                primary_span: node.meta.span,
                secondary_spans: Vec::with_capacity(0),
                message: format!("function '{}' already defined", func.name),
                notes: Vec::with_capacity(0)
            });
        };

        let defined_func_id = insert_res.ok().unwrap().id.get(); // okay to unwrap
        self.curr_func_id = Some(defined_func_id.0);

        self.scope.push(ScopeType::Function);

        // loop through function's body to find new symbols
        if let Some(func_body) = &mut node.left {
            let _ = self.declare_symbol(func_body)?;
        }

        Ok(defined_func_id.0)
    }

    fn declare_let_binding(&mut self, node: &mut AST<'tcx>) -> BindingResult {
        let stmt = match &mut node.kind {
            ASTKind::StmtAST(Stmt::VarDecl(stmt)) => stmt,
            _ => bug!("Invalid node"),
        };
        // If it's a record type, validate fields
        if let SymTy::Record { name: rec_name } = &stmt.symbol_type {
            let value_node = node.left.as_ref().unwrap().clone();
            _ = self.validate_record_let_binding(rec_name, &value_node)?;
        }
        if let Some(left) = &mut node.left {
            self.validate_and_process_expr(left, stmt.sym_name)?;
        }

        stmt.func_id = self.scope.current_fn().0;
        let sym = Sym::new(stmt.sym_name, stmt.ty, stmt.symbol_type, stmt.class, FuncId(stmt.func_id));
        let id = self.scope.declare(sym);

        if let Ok(sym) = id {
            Ok(0) // return the symbol's symtable position
        }
        else {
            Err(
                Diagnostic {
                    code: Some(ErrCode::SEM2001),
                    severity: Severity::Error,
                    primary_span: node.meta.span,
                    secondary_spans: Vec::with_capacity(0),
                    message: "symbol already defined".to_string(),
                    notes: Vec::with_capacity(0)
                }
            )
        }
    }

    fn validate_and_process_expr(&mut self, ast: &mut AST<'tcx>, symbol_name: &'tcx str) -> BindingResult {
        if !ast.kind.is_expr() {
            bug!("Expected an Expr--but found {:#?}", ast);
        }
        if let ASTKind::ExprAST(expr) = &mut ast.kind {
            return self.resolve_literal_constant(expr, false, symbol_name, &ast.meta);
        }
        bug!("")
    }

    fn resolve_literal_constant(&mut self, expr: &mut Expr<'tcx>, parent_is_record: bool, symbol_name: &'tcx str, meta: &NodeMeta) -> BindingResult {
        if let Expr::LitVal(lit_expr) = expr {
            match lit_expr.ty {
                TyKind::Str => {
                    let raw_value = if let LitValue::RawStr(ref s) = lit_expr.value {
                        s
                    } 
                    else {
                        bug!("Expected RawStr variant but found something else");
                    };
                    let pool_idx = self.const_pool.insert(
                        KagcConst::Str(raw_value.to_string()),
                        KObjType::KStr,
                        self.curr_func_id
                    );
                    lit_expr.value = LitValue::PoolStr(pool_idx);
                    lit_expr.ty = TyKind::PoolStr;
                    return Ok(pool_idx);
                },
                TyKind::I64 => {
                    if !parent_is_record {
                        return Ok(0xFFFFFFFF);
                    }
                    let raw_value = match lit_expr.value {
                        LitValue::I32(value) => value as i64,
                        | LitValue::U8(value) => value as i64,
                        _ => bug!("Expected Int variant but found something else")
                    };
                    let pool_idx = self.const_pool.insert(
                        KagcConst::Int(raw_value), 
                        KObjType::KStr,
                        self.curr_func_id
                    );
                    lit_expr.ty = TyKind::I64;
                    lit_expr.value = LitValue::I64(raw_value);
                    return Ok(pool_idx);
                },
                _ => return Ok(0)
            }
        }
        else if let Expr::RecordCreation(rec_create) = expr {
            let record_const = self.build_record_const(rec_create, symbol_name, meta)?;
            if !parent_is_record {
                let record_idx = self.const_pool.insert(
                    KagcConst::Record(record_const),
                    KObjType::KRec,
                    self.curr_func_id,
                );
                rec_create.pool_idx = record_idx;
                return Ok(record_idx);
            } 
            else {
                return Ok(0xFFFFFFFF);
            }
        }
        else if let Expr::FuncCall(func_call) = expr {
            for arg in &mut func_call.args {
                let _ = self.resolve_literal_constant(&mut arg.1, false, "", meta)?;
            }
            return Ok(0xFFFFFFFF);
        }
        else if let Expr::Binary(bin) = expr {
            let _ = self.resolve_literal_constant(&mut bin.left, parent_is_record, symbol_name, meta)?;
            let _ = self.resolve_literal_constant(&mut bin.right, parent_is_record, symbol_name, meta)?;
            return Ok(0xFFFFFFFF);
        }
        else if let Expr::RecordFieldAccess(access) = expr {
            self.require_symbol_exists(access.rec_alias, meta)?;
            let symbol = self.scope.deep_lookup(None, access.rec_alias).unwrap(); // safe to unwrap because of require_symbol_exists call
            // extract record's name
            let record_name = if let SymTy::Record { name } = symbol.sym_ty.get() { name }
            else { bug!("record '{}' not found", access.rec_name); };
            //
            if let Some(record) = self.scope.lookup_record(record_name) {
                if !record.fields.iter().any(|field| field.name == access.field_chain[0]) {
                    bug!("field '{}' not present in '{}'", access.field_chain[0], record_name);
                }
            }
            return Ok(0xFFFFFFFF);
        }
        Ok(0xFFFFFFFF)
    }

    fn build_record_const(&mut self, rec_create: &mut RecordCreationExpr<'tcx>, symbol_name: &'tcx str, meta: &NodeMeta) -> Result<RecordConst, Diagnostic> {
        let mut indices = Vec::with_capacity(rec_create.fields.len());
        for field in &mut rec_create.fields {
            let pool_idx = self.resolve_literal_constant(&mut field.value, true, symbol_name, meta)?;
            indices.push((field.name.to_string(), pool_idx));
        }
        Ok(RecordConst {
            type_name: rec_create.name.to_string(),
            alias: symbol_name.to_string(),
            fields: OrderedMap(IndexMap::from_iter(indices)),
            alignment: 3 // on Mac M1 silicon chip
        })
    }

    fn validate_record_let_binding(&mut self, rec_name: &str, value_node: &AST) -> BindingResult {
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
                    return Err(
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
                    return Err(
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
        Ok(0)
    }

    fn declare_record(&mut self, node: &mut AST<'tcx>) -> BindingResult {
        if let ASTKind::StmtAST(Stmt::Record(stmt)) = &node.kind {
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
            if self.scope.create_record("", record_entry).is_none() {
                let already_defined_err = Diagnostic {
                    code: Some(ErrCode::SEM2001),
                    severity: Severity::Error,
                    primary_span: node.meta.span,
                    secondary_spans: Vec::with_capacity(0),
                    message: "symbol already defined".to_string(),
                    notes: Vec::with_capacity(0)
                };
                return Err(already_defined_err);
            }
            return Ok(0);
        }
        bug!("Cannot create record!!!")
    }

    fn require_symbol_exists(&self, symbol_name: &'tcx str, meta: &NodeMeta) -> Result<(), Diagnostic> {
        if self.scope.deep_lookup(None, symbol_name).is_none() {
            return Err(
                Diagnostic {
                    code: Some(ErrCode::SEM2000),
                    severity: Severity::Error,
                    primary_span: meta.span,
                    secondary_spans: Vec::with_capacity(0),
                    message: "symbol not found".to_string(),
                    notes: Vec::with_capacity(0)
                }
            );
        }
        Ok(())
    }
}