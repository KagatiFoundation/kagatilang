// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::{cell::RefCell, rc::Rc};

use kagc_ast::*;
use kagc_ctx::CompilerCtx;
use kagc_errors::code::ErrCode;
use kagc_errors::diagnostic::Severity;
use kagc_errors::diagnostic::Diagnostic;
use kagc_symbol::Symbol;
use kagc_symbol::{function::INVALID_FUNC_ID, SymbolType};

use kagc_types::{is_type_coalescing_possible, LitTypeVariant};
use kagc_utils::bug;

use crate::type_checker::TypeChecker;

pub type SAResult = Result<LitTypeVariant, Diagnostic>;

pub struct SemanticAnalyzer {
    pub ctx: Rc<RefCell<CompilerCtx>>,
}

impl SemanticAnalyzer {
    pub fn new(ctx: Rc<RefCell<CompilerCtx>>) -> Self {
        Self {
            ctx,
        }
    }

    /// Start the analysis process
    /// 
    /// This starts an analysis process for the given list of nodes. 
    /// This function panics if it encounters any form of error.
    pub fn start_analysis(&mut self, nodes: &mut [AST]) {
        for node in nodes {
            if let Err(err) = self.analyze_node(node) {
                self.ctx.borrow_mut().diagnostics.push(err);
            }
        }
    }

    fn analyze_node(&mut self, node: &mut AST) -> SAResult {
        let ASTKind::StmtAST(_) = &mut node.kind else {
            panic!("{:#?} is not a supported type, yet!", node);
        };
        match node.operation {
            ASTOperation::AST_VAR_DECL => self.analyze_var_decl_stmt(node),
            ASTOperation::AST_FUNCTION => self.analyze_func_decl_stmt(node),
            ASTOperation::AST_RETURN => self.analyze_return_stmt(node),
            ASTOperation::AST_FUNC_CALL => self.analyze_fn_call(node),
            ASTOperation::AST_LOOP => self.analyze_node(node.left.as_mut().unwrap()),
            ASTOperation::AST_IF => self.analyze_if_stmt(node),
            ASTOperation::AST_IMPORT => Ok(LitTypeVariant::Void),
            ASTOperation::AST_RECORD_DECL => self.analyze_record_decl_stmt(node),
            ASTOperation::AST_BLOCK => self.analyze_block_stmt(node),
            ASTOperation::AST_GLUE => {
                if let Some(left) = &mut node.left {
                    self.analyze_node(left)?;
                }
                if let Some(right) = &mut node.right {
                    self.analyze_node(right)?;
                }
                Ok(LitTypeVariant::None)
            },
            ASTOperation::AST_NONE
            | ASTOperation::AST_BREAK => Ok(LitTypeVariant::None),
            _ => panic!("'{:?}' is not supported ASTOperation for 'analyze_node' yet!", node.operation)
        }
    }

    fn analyze_block_stmt(&mut self, node: &mut AST) -> SAResult {
        let block_stmt = node.expect_block_stmt_mut();
        for s in &mut block_stmt.statements {
            self.analyze_node(s)?;
        }
        Ok(LitTypeVariant::None)
    }

    fn analyze_fn_call(&mut self, func_call: &mut AST) -> SAResult {
        if let ASTKind::ExprAST(Expr::FuncCall(func_call_expr)) = &mut func_call.kind {
            self.analyze_func_call_expr(func_call_expr, &func_call.meta)
        }
        else {
            panic!()
        }
    }

    fn analyze_expr(&mut self, ast: &mut AST) -> SAResult {
        if !ast.kind.is_expr() {
            panic!("Needed an Expr--but found {:#?}", ast);
        }

        if let ASTKind::ExprAST(expr) = &mut ast.kind {
            ast.result_type = self.analyze_and_mutate_expr(expr, &ast.meta)?;
            return Ok(ast.result_type.clone());
        }

        panic!()
    }

    fn analyze_and_mutate_expr(&mut self, expr: &mut Expr, meta: &NodeMeta) -> SAResult {
        match expr {
            Expr::LitVal(litexpr) => self.analyze_lit_expr(litexpr),
            Expr::Binary(binexpr) => self.analyze_bin_expr(binexpr, meta),
            Expr::Ident(identexpr) => self.analyze_ident_expr(identexpr, meta),
            Expr::FuncCall(funccallexpr) => self.analyze_func_call_expr(funccallexpr, meta),
            Expr::RecordCreation(recexpr) => self.analyze_rec_creation_expr(recexpr, meta),
            Expr::RecordFieldAccess(recfieldexpr) => self.analyze_record_field_access_expr(recfieldexpr, meta),
            Expr::Null => Ok(LitTypeVariant::Null),
            _ => todo!()
        }
    }

    fn analyze_bin_expr(&mut self, bin_expr: &mut BinExpr, meta: &NodeMeta) -> SAResult {
        let left_type: LitTypeVariant = self.analyze_and_mutate_expr(&mut bin_expr.left, meta)?;
        let right_type: LitTypeVariant = self.analyze_and_mutate_expr(&mut bin_expr.right, meta)?;

        bin_expr.result_type = TypeChecker::check_bin_expr_type_compatability(
            left_type, 
            right_type, 
            bin_expr.operation,
            meta
        )?;
        Ok(bin_expr.result_type.clone())
    }

    fn analyze_record_field_access_expr(&mut self, field_access: &mut RecordFieldAccessExpr, meta: &NodeMeta) -> SAResult {
        let ctx_borrow = self.ctx.borrow_mut();
        if let Some(rec_sym) = ctx_borrow.scope.borrow().deep_lookup(&field_access.rec_alias) {
            if let SymbolType::Record { name } = &rec_sym.sym_type {
                if let Some(rec) = ctx_borrow.scope.borrow().lookup_record(name) {
                    if let Some(field) = rec.fields.iter().find(|&field| field.name == field_access.field_chain[0]) {
                        field_access.rel_stack_off = field.rel_stack_off;
                        field_access.result_type = field.typ.clone();
                        field_access.rec_name = rec.name.clone();
                        return Ok(field.typ.clone());
                    }
                }
            }
            else {
                return Err(Diagnostic {
                    code: Some(ErrCode::SEM2000),
                    severity: Severity::Error,
                    primary_span: meta.span,
                    secondary_spans: vec![],
                    message: format!("'{}' is not a record type", field_access.rec_name),
                    notes: vec![]
                });
            }
        }
        Err(Diagnostic {
            code: Some(ErrCode::SEM2000),
            severity: Severity::Error,
            primary_span: meta.span,
            secondary_spans: vec![],
            message: format!("type '{}' cannot be resolved", field_access.rec_name),
            notes: vec![]
        })
    }

    fn analyze_rec_creation_expr(&mut self, rec_expr: &mut RecordCreationExpr, meta: &NodeMeta) -> SAResult {
        for field in &mut rec_expr.fields {
            let ff = self.analyze_and_mutate_expr(&mut field.value, meta)?;
            if ff == LitTypeVariant::None {
                bug!("record's field expression evaluation resulted in type None")
            }
        }
        Ok(LitTypeVariant::Record {
            name: rec_expr.name.clone()
        })
    }

    fn analyze_ident_expr(&mut self, ident_expr: &mut IdentExpr, meta: &NodeMeta) -> SAResult {
        if let Some(ident) = self.ctx.borrow().scope.borrow().deep_lookup(&ident_expr.sym_name) {
            ident_expr.result_type = ident.lit_type.clone();
            Ok(ident_expr.result_type.clone())
        }
        else {
            let diag = Diagnostic {
                code: Some(ErrCode::SEM2000),
                severity: Severity::Error,
                primary_span: meta.span,
                secondary_spans: vec![],
                message: format!("undefined symbol '{}'", ident_expr.sym_name),
                notes: vec![]
            };
            Err(diag)
        }
    }

    /// Analyze the function call expression
    /// 
    /// This analysis is done to check if the arguments to this 
    /// function call are valid.
    fn analyze_func_call_expr(&mut self, func_call: &mut FuncCallExpr, meta: &NodeMeta) -> SAResult {
        let func_sym_type = if let Some(func_sym) = self.ctx.borrow().scope.borrow().deep_lookup(&func_call.symbol_name) {
            if func_sym.sym_type != SymbolType::Function {
                let diag = Diagnostic {
                    code: Some(ErrCode::TYP3000),
                    severity: Severity::Error,
                    primary_span: meta.span,
                    secondary_spans: vec![],
                    message: format!("'{}' is not callable", func_sym.name),
                    notes: vec![]
                };
                return Err(diag);
            }
            else {
                func_sym.lit_type.clone()
            }
        }
        else {
            let diag = Diagnostic {
                code: Some(ErrCode::SEM2000),
                severity: Severity::Error,
                primary_span: meta.span,
                secondary_spans: vec![],
                message: format!("undefined symbol '{}'", func_call.symbol_name),
                notes: vec![]
            };
            return Err(diag);
        };

        let ctx_borrow = self.ctx.borrow();
        let scope_borrow = ctx_borrow.scope.borrow();
        let func_detail = scope_borrow
            .lookup_fn_by_name(func_call.symbol_name.as_str())
            .unwrap();
        func_call.id = func_detail.func_id;
        let func_param_types = func_detail.param_types.clone();

        drop(scope_borrow);
        drop(ctx_borrow);

        self.check_func_call_args(
            &mut func_call.args[..], 
            &func_param_types, 
            meta
        )?;            

        func_call.result_type = func_sym_type.clone();
        Ok(func_sym_type)        
    }

    /// Check if the function arguments match the parameter types
    fn check_func_call_args(&mut self, args: &mut [(usize, Expr)], param_types: &[LitTypeVariant], meta: &NodeMeta) -> SAResult {
        if args.len() != param_types.len() {
            let diag = Diagnostic {
                code: Some(ErrCode::TYP3001),
                severity: Severity::Error,
                primary_span: meta.span,
                secondary_spans: vec![],
                message: format!("argument length mismatch: expected '{}' but found '{}'", param_types.len(), args.len()),
                notes: vec![]
            };
            return Err(diag);
        }

        // 
        for (idx, param_type) in param_types.iter().enumerate() {
            let expr_res: LitTypeVariant = self.analyze_and_mutate_expr(&mut args[idx].1, meta)?;
            let assignment_ok: bool = expr_res == *param_type || TypeChecker::is_type_coalesciable(expr_res.clone(), param_type.clone());
            if !assignment_ok {
                let diag = Diagnostic {
                    code: Some(ErrCode::TYP3002),
                    severity: Severity::Error,
                    primary_span: meta.span,
                    secondary_spans: vec![],
                    message: format!("'{}' is not compatible with '{}'", param_type, expr_res.clone()),
                    notes: vec![]
                };
                return Err(diag);
            }
        }
        Ok(LitTypeVariant::None) // placeholder; doesn't affect anything
    }

    /// There's nothing to be done here, actually. We don't care 
    /// what type of literal value we get, every literal value is 
    /// okay. The using expression might restraint from using it.
    fn analyze_lit_expr(&self, expr: &mut LitValExpr) -> SAResult {
        Ok(expr.result_type.clone())
    }

    fn analyze_record_decl_stmt(&mut self, node: &mut AST) -> SAResult {
        if !node.kind.is_stmt() {
            panic!("Needed a RecordDeclStmt--but found {node:#?}");
        }

        Ok(LitTypeVariant::Record {
            name: "empty 1".to_string()
        })
    }

    fn analyze_return_stmt(&mut self, node: &mut AST) -> SAResult {
        if !node.kind.is_stmt() {
            panic!("Needed a ReturnStmt--but found {node:#?}");
        }

        if let Some(Stmt::Return(ret_stmt)) = &mut node.kind.as_stmt_mut() {
            // 'return' statements can appear only inside the functions
            // thus, the current function cannot be None; it's an error otherwise
            let current_fn = self.ctx.borrow().scope.borrow().current_fn();
            let expected_fn_ret_type: LitTypeVariant = self.ctx
                .borrow()
                .scope
                .borrow()
                .lookup_fn(current_fn)
                .unwrap()
                .return_type
                .clone();
            
            // if return statement returns some value
            let found_fn_ret_type: LitTypeVariant = if let Some(return_expr) = &mut node.left {
                self.analyze_expr(return_expr)?
            }
            else {
                LitTypeVariant::Void
            };

            let missing_ret = !expected_fn_ret_type.is_void() && found_fn_ret_type.is_none();
            let incompatible_ret_chk1 = expected_fn_ret_type != found_fn_ret_type;
            let incompatible_ret_chk2 = !TypeChecker::is_type_coalesciable(
                        found_fn_ret_type.clone(), 
                        expected_fn_ret_type.clone()
                    );

            let incompatible_ret = incompatible_ret_chk1 && incompatible_ret_chk2;
            let ret_typ_mismatch = missing_ret || incompatible_ret;

            if ret_typ_mismatch {
                let diag = Diagnostic {
                    code: Some(ErrCode::TYP3002),
                    severity: Severity::Error,
                    primary_span: node.meta.span,
                    secondary_spans: vec![],
                    message: format!(
                        "'{}' is not compatible with '{}'", 
                        found_fn_ret_type, 
                        expected_fn_ret_type
                    ),
                    notes: vec![]
                };
                return Err(diag);
            }  
            ret_stmt.func_id = self.ctx.borrow().scope.borrow().current_fn();
            return Ok(found_fn_ret_type);
        }
        panic!("Not a return statement!");
    }

    fn analyze_func_decl_stmt(&mut self, node: &mut AST) -> SAResult {
        if !node.kind.is_stmt() {
            panic!("Needed a FuncDecl--but found {:#?}", node);
        }

        if let Some(Stmt::FuncDecl(func_decl)) = &mut node.kind.as_stmt() {
            let ctx_borrow = self.ctx.borrow_mut();
            let func_ret_type = ctx_borrow
                .scope
                .borrow()
                .root_scope()
                .lookup(&func_decl.name)
                .map(|func_sym| func_sym.lit_type.clone());

            ctx_borrow.scope.borrow_mut().update_current_func(func_decl.func_id);

            if func_ret_type.is_none() {
                let diag = Diagnostic {
                    code: Some(ErrCode::SEM2000),
                    severity: Severity::Error,
                    primary_span: node.meta.span,
                    secondary_spans: vec![],
                    message: format!("undefined symbol '{}'", func_decl.name),
                    notes: vec![]
                };
                return Err(diag);
            };

            // unwrap the return type
            let func_ret_type: LitTypeVariant = func_ret_type.unwrap();

            // switch to function's scope
            ctx_borrow.scope.borrow_mut().enter_scope(func_decl.scope_id);
            drop(ctx_borrow);

            if let Some(func_body) = &mut node.left {
                self.analyze_node(func_body)?;
            }

            // exit function's scope
            self.ctx.borrow_mut().scope.borrow_mut().exit_scope();
            // invalidate function id
            self.ctx.borrow_mut().scope.borrow_mut().update_current_func(INVALID_FUNC_ID);
            return Ok(func_ret_type);
        }
        panic!("Not a function declaration statement!");
    }

    fn analyze_var_decl_stmt(&mut self, node: &mut AST) -> SAResult {
        if let Some(Stmt::VarDecl(var_decl)) = node.kind.as_stmt_mut() {
            let var_value_type: LitTypeVariant = self.analyze_expr(node.left.as_mut().unwrap())?;
            let ctx_borrow = self.ctx.borrow_mut();
            let curr_func_id = ctx_borrow.scope.borrow().current_fn();

            if let Some(var_sym) = ctx_borrow.scope.borrow_mut().deep_lookup_mut(&var_decl.sym_name) {
                let var_type = Self::check_and_mutate_var_decl_stmt(var_sym, var_value_type.clone(), &node.meta)?;
                
                if let LitTypeVariant::Record{ name } = &var_value_type {
                    var_sym.sym_type = SymbolType::Record { name: name.clone() }
                }

                var_sym.func_id = Some(curr_func_id);
                var_sym.lit_type = var_type.clone();
                node.result_type = var_type.clone();
                return Ok(var_type);
            }
            let diag = Diagnostic {
                code: Some(ErrCode::SEM2000),
                severity: Severity::Error,
                primary_span: node.meta.span,
                secondary_spans: vec![],
                message: format!("undefined symbol '{}'", var_decl.sym_name),
                notes: vec![]
            };
            return Err(diag);
        }
        panic!("Not a var declaration statement");
    }

    /// Performs type checking for variable declaration statements.
    /// 
    /// This function infers the type of an expression assigned to a variable,
    /// updates the variable's symbol if it was declared without an explicit 
    /// type, and ensures the assigned expression's type matches the declared or 
    /// inferred type. If there's a type mismatch that cannot be reconciled, an 
    /// error is returned.
    pub fn check_and_mutate_var_decl_stmt(var_decl_sym: &mut Symbol, expr_type: LitTypeVariant, meta: &NodeMeta) -> SAResult {
        if var_decl_sym.lit_type == LitTypeVariant::None {
            match expr_type {
                // implicitly convert no-type-annotated byte-type into an integer
                LitTypeVariant::U8 => {
                    var_decl_sym.lit_type = LitTypeVariant::I32;
                    return Ok(LitTypeVariant::I32);
                },

                LitTypeVariant::RawStr => {
                    var_decl_sym.lit_type = LitTypeVariant::RawStr;
                    return Ok(LitTypeVariant::RawStr);
                },

                LitTypeVariant::PoolStr => {
                    var_decl_sym.lit_type = LitTypeVariant::PoolStr;
                    return Ok(LitTypeVariant::PoolStr);
                }
                
                _ => return Ok(expr_type)
            }
        }
        if 
            var_decl_sym.lit_type != expr_type 
            && !is_type_coalescing_possible(expr_type.clone(), var_decl_sym.lit_type.clone()) 
        {
            let diag = Diagnostic {
                code: Some(ErrCode::TYP3003),
                severity: Severity::Error,
                primary_span: meta.span,
                secondary_spans: vec![],
                message: format!("expected type `{}`, found `{}`", var_decl_sym.lit_type, expr_type),
                notes: vec![]
            };
            return Err(diag);
        }
        Ok(expr_type)
    }

    fn analyze_if_stmt(&mut self, node: &mut AST) -> SAResult {
        if !node.kind.is_stmt() {
            panic!("Needed an Expr--but found {:#?}", node);
        }

        if let ASTKind::StmtAST(Stmt::If(if_stmt)) = &node.kind {
            self.ctx.borrow_mut().scope.borrow_mut().enter_scope(if_stmt.scope_id);

            // every 'if' has an expression attached with it in its
            // left branch
            let cond_res: LitTypeVariant = self.analyze_expr(node.left.as_mut().unwrap())?;
            if cond_res != LitTypeVariant::I32 {
                let diag = Diagnostic {
                    code: Some(ErrCode::TYP3002),
                    severity: Severity::Error,
                    primary_span: node.meta.span,
                    secondary_spans: vec![],
                    message: format!("'{}' is not compatible with '{}'", cond_res, LitTypeVariant::I32),
                    notes: vec![]
                };
                return Err(diag);
            }

            if let Some(if_body) = &mut node.mid {
                self.analyze_node(if_body)?;
            }
            self.ctx.borrow_mut().scope.borrow_mut().exit_scope();
        }

        // else-block
        if let Some(right_tree) = &mut node.right {
            if let Some(else_block_tree) = &mut right_tree.left {
                if let ASTKind::StmtAST(Stmt::Scoping(scoping_stmt)) = &else_block_tree.kind {
                    self.ctx.borrow_mut().scope.borrow_mut().enter_scope(scoping_stmt.scope_id);
                }
                self.analyze_node(else_block_tree)?;
                self.ctx.borrow_mut().scope.borrow_mut().exit_scope();
            }
        }
        Ok(LitTypeVariant::None)
    }
}