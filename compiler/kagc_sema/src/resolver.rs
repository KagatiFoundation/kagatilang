// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::RefCell;
use std::rc::Rc;
use indexmap::IndexMap;
use kagc_ast::{AST, ASTKind, ASTOperation, Expr, NodeMeta, RecordCreationExpr, Stmt};
use kagc_errors::code::ErrCode;
use kagc_errors::diagnostic::{Diagnostic, Severity};
use kagc_symbol::function::FunctionInfo;
use kagc_symbol::{Symbol, SymbolType};
use kagc_const::pool::{KagcConst, OrderedMap, RecordConst};
use kagc_ctx::CompilerCtx;
use kagc_types::builtins::obj::KObjType;
use kagc_types::record::{RecordFieldType, RecordType};
use kagc_types::{LitType, LitTypeVariant};
use kagc_utils::bug;

pub struct Resolver {
    pub ctx: Rc<RefCell<CompilerCtx>>,
    pub local_offset: usize,
    curr_func_id: Option<usize>
}

pub type ResolverResult = Result<usize, Diagnostic>;

impl Resolver {
    pub fn new(ctx: Rc<RefCell<CompilerCtx>>) -> Self {
        Self {
            ctx,
            local_offset: 0,
            curr_func_id: None
        }
    }

    pub fn resolve(&mut self, nodes: &mut Vec<AST>) {
        for node in nodes {
            if let Err(e) = self.declare_symbol(node) {
                self.ctx.borrow_mut().diagnostics.push(e);
            }
        }
    }

    fn declare_symbol(&mut self, node: &mut AST) -> ResolverResult  {
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

    fn declare_if_else_tree(&mut self, node: &mut AST) -> ResolverResult {
        if !node.kind.is_stmt() {
            bug!("Needed a IfDecl--but found {:#?}", node);
        }
        if let ASTKind::StmtAST(Stmt::If(if_stmt)) = &mut node.kind {
            self.ctx.borrow_mut().scope.borrow_mut().enter_scope(if_stmt.scope_id);
            self.ctx.borrow_mut().scope.borrow_mut().exit_scope();
        }
        else {
            bug!("Provided Stmt {:#?} is not of type IfStmt! Aborting...", node);
        }

        if let Some(mid_tree) = &mut node.mid {
            self.declare_symbol(mid_tree)?; // mid tree is the 'if' block
            self.ctx.borrow_mut().scope.borrow_mut().exit_scope(); // exit if-block's scope
        }

        if let Some(right_tree) = &mut node.right {
            // right tree is the 'else' block
            if let Some(else_block_tree) = &mut right_tree.left {
                if let ASTKind::StmtAST(Stmt::Scoping(scoping_stmt)) = &else_block_tree.kind {
                    self.ctx.borrow_mut().scope.borrow_mut().enter_scope(scoping_stmt.scope_id);
                }
                self.declare_symbol(else_block_tree)?;
                self.ctx.borrow_mut().scope.borrow_mut().exit_scope();
            }
        }
        Ok(0)
    }

    fn declare_function_symbol(&mut self, node: &mut AST) -> ResolverResult {
        if !node.kind.is_stmt() {
            bug!("cannot proceed without a FuncStmt");
        }

        if let Some(Stmt::FuncDecl(func_decl)) = &mut node.kind.as_stmt_mut() {
            let ctx_borrow = self.ctx.borrow_mut();
            let function_id: Option<usize> = {
                let sym = Symbol::new(
                    func_decl.name.clone(),
                    func_decl.return_type.clone(),
                    SymbolType::Function,
                    func_decl.storage_class,
                );
                let insert_pos: Option<usize> = ctx_borrow
                    .scope
                    .borrow_mut()
                    .root_scope_mut()
                    .declare(sym);
                insert_pos
            };
            // ^^^^ scope.declare() returns None if the symbol cannot be added indicating re-definition of the symbol
            if function_id.is_none() {
                let already_defined_err = Diagnostic {
                    code: Some(ErrCode::SEM2001),
                    severity: Severity::Error,
                    primary_span: node.meta.span,
                    secondary_spans: Vec::with_capacity(0),
                    message: "symbol already defined".to_string(),
                    notes: Vec::with_capacity(0)
                };
                return Err(already_defined_err);
            };

            // switch to function's scope
            let _ = ctx_borrow.scope.borrow_mut().enter_scope(func_decl.scope_id);
            let function_id = function_id.unwrap();
            self.curr_func_id = Some(function_id);

            // making sure the subsequent users of this FuncDeclStmt AST know 
            // what the function ID is
            func_decl.func_id = function_id;

            let func_info: FunctionInfo = FunctionInfo::new(
                func_decl.name.clone(),
                function_id,
                func_decl.return_type.clone(),
                func_decl.storage_class,
                func_decl.locals.clone(),
                func_decl.func_param_types.clone()
            );

            // create a new FunctionInfo
            ctx_borrow.scope.borrow_mut().declare_fn(func_info);

            // drop context borrow
            drop(ctx_borrow);

            // loop through function's body to find new symbols
            if let Some(func_body) = &mut node.left {
                let _ = self.declare_symbol(func_body)?;
            }

            // exit the function's scope
            self.ctx.borrow_mut().scope.borrow_mut().exit_scope();
            return Ok(function_id);
        }
        bug!("Not a function declaration statement!");
    }

    fn declare_let_binding(&mut self, node: &mut AST) -> ResolverResult {
        let stmt = match &mut node.kind {
            ASTKind::StmtAST(Stmt::VarDecl(stmt)) => stmt,
            _ => bug!("Invalid node"),
        };
        // If it's a record type, validate fields
        if let SymbolType::Record { name: rec_name } = &stmt.symbol_type {
            let value_node = node.left.as_ref().unwrap().clone();
            _ = self.validate_record_let_binding(rec_name, &value_node)?;
        }
        if let Some(left) = &mut node.left {
            self.validate_and_process_expr(left, &stmt.sym_name)?;
        }

        stmt.func_id = self.ctx.borrow_mut().scope.borrow().current_fn();
        let sym = Symbol::create(
            stmt.sym_name.clone(),
            stmt.value_type.clone(),
            stmt.symbol_type.clone(),
            0,
            stmt.class,
            stmt.local_offset.try_into().unwrap(),
            None,
            stmt.func_id,
        );
        let ctx_borrow = self.ctx.borrow();
        let mut scope_borrow = ctx_borrow.scope.borrow_mut();
        let id = scope_borrow
            .declare(sym)
            .ok_or({
                Diagnostic {
                    code: Some(ErrCode::SEM2001),
                    severity: Severity::Error,
                    primary_span: node.meta.span,
                    secondary_spans: Vec::with_capacity(0),
                    message: "symbol already defined".to_string(),
                    notes: Vec::with_capacity(0)
                }
            }
        )?;
        Ok(id)
    }

    fn validate_and_process_expr(&mut self, ast: &mut AST, symbol_name: &str) -> ResolverResult {
        if !ast.kind.is_expr() {
            bug!("Expected an Expr--but found {:#?}", ast);
        }
        if let ASTKind::ExprAST(expr) = &mut ast.kind {
            return self.resolve_literal_constant(expr, false, symbol_name, &ast.meta);
        }
        bug!("")
    }

    fn resolve_literal_constant(&mut self, expr: &mut Expr, parent_is_record: bool, symbol_name: &str, meta: &NodeMeta) -> ResolverResult {
        if let Expr::LitVal(lit_expr) = expr {
            match lit_expr.result_type {
                LitTypeVariant::RawStr => {
                    let raw_value = if let LitType::RawStr(ref s) = lit_expr.value {
                        s.clone()
                    } 
                    else {
                        bug!("Expected RawStr variant but found something else");
                    };
                    let pool_idx = self.ctx.borrow_mut().const_pool.insert(
                        KagcConst::Str(raw_value),
                        KObjType::KStr,
                        self.curr_func_id
                    );
                    lit_expr.value = LitType::PoolStr(pool_idx);
                    lit_expr.result_type = LitTypeVariant::PoolStr;
                    return Ok(pool_idx);
                },
                LitTypeVariant::I32
                | LitTypeVariant::U8 => {
                    if !parent_is_record {
                        return Ok(0xFFFFFFFF);
                    }
                    let raw_value = match lit_expr.value {
                        LitType::I32(value) => value as i64,
                        | LitType::U8(value) => value as i64,
                        _ => bug!("Expected Int variant but found something else")
                    };
                    let pool_idx = self.ctx.borrow_mut().const_pool.insert(
                        KagcConst::Int(raw_value), 
                        KObjType::KStr,
                        self.curr_func_id
                    );
                    lit_expr.result_type = LitTypeVariant::I64;
                    lit_expr.value = LitType::I64(raw_value);
                    return Ok(pool_idx);
                },
                _ => return Ok(0)
            }
        }
        else if let Expr::RecordCreation(rec_create) = expr {
            let record_const = self.build_record_const(rec_create, symbol_name, meta)?;
            if !parent_is_record {
                let record_idx = self.ctx.borrow_mut().const_pool.insert(
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
            self.require_symbol_exists(&access.rec_alias, meta)?;
            let ctx_borrow = self.ctx.borrow();
            let scope_borrow = ctx_borrow.scope.borrow();
            let symbol = scope_borrow.deep_lookup(&access.rec_alias).unwrap(); // safe to unwrap because of require_symbol_exists call
            // extract record's name
            let record_name = if let SymbolType::Record { name } = &symbol.sym_type { name }
            else { bug!("record '{}' not found", access.rec_name); };
            //
            if let Some(record) = ctx_borrow.scope.borrow().lookup_record(record_name) {
                if !record.fields.iter().any(|field| field.name == access.field_chain[0]) {
                    bug!("field '{}' not present in '{}'", access.field_chain[0], record_name);
                }
            }
            return Ok(0xFFFFFFFF);
        }
        Ok(0xFFFFFFFF)
    }

    fn build_record_const(&mut self, rec_create: &mut RecordCreationExpr, symbol_name: &str, meta: &NodeMeta) -> Result<RecordConst, Diagnostic> {
        let mut indices = Vec::with_capacity(rec_create.fields.len());
        for field in &mut rec_create.fields {
            let pool_idx = self.resolve_literal_constant(&mut field.value, true, symbol_name, meta)?;
            indices.push((field.name.clone(), pool_idx));
        }
        Ok(RecordConst {
            type_name: rec_create.name.clone(),
            alias: symbol_name.to_string(),
            fields: OrderedMap(IndexMap::from_iter(indices)),
            alignment: 3 // on Mac M1 silicon chip
        })
    }

    fn validate_record_let_binding(&mut self, rec_name: &str, value_node: &AST) -> ResolverResult {
        let ctx_borrow = self.ctx.borrow_mut();
        let scope_borrow = ctx_borrow.scope.borrow();
        let rec = scope_borrow.lookup_record(rec_name).ok_or_else(|| {
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

    fn declare_record(&mut self, node: &mut AST) -> ResolverResult {
        if let ASTKind::StmtAST(Stmt::Record(stmt)) = &node.kind {
            let record_entry = RecordType {
                name: stmt.name.clone(),
                size: 0,
                __alignment: stmt.alignment,
                fields: stmt.fields.iter().enumerate().map(|(idx, field)| {
                    RecordFieldType {
                        name: field.name.clone(),
                        typ: field.typ.clone(),
                        rel_stack_off: idx
                    }
                }).collect::<Vec<RecordFieldType>>()
            };
            if self.ctx.borrow_mut().scope.borrow_mut().create_record(record_entry).is_none() {
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

    fn require_symbol_exists(&self, symbol_name: &str, meta: &NodeMeta) -> Result<(), Diagnostic> {
        if self.ctx.borrow().scope.borrow().deep_lookup(symbol_name).is_none() {
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