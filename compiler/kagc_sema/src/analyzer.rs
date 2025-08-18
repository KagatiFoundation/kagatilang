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

use std::{cell::RefCell, rc::Rc};

use kagc_ast::*;
use kagc_ctx::CompilerCtx;
use kagc_errors::code::ErrCode;
use kagc_errors::diagnostic::Severity;
use kagc_errors::diagnostic::Diagnostic;
use kagc_symbol::Symbol;
use kagc_symbol::{function::INVALID_FUNC_ID, SymbolType};

use kagc_types::{is_type_coalescing_possible, LitTypeVariant};

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
        match node.operation {
            ASTOperation::AST_VAR_DECL => self.analyze_var_decl_stmt(node),

            ASTOperation::AST_FUNCTION => self.analyze_func_decl_stmt(node),

            ASTOperation::AST_RETURN => self.analyze_return_stmt(node),

            ASTOperation::AST_FUNC_CALL => self.analyze_fn_call(node),

            ASTOperation::AST_LOOP => self.analyze_node(node.left.as_mut().unwrap()),

            ASTOperation::AST_IF => self.analyze_if_stmt(node),

            ASTOperation::AST_IMPORT => Ok(LitTypeVariant::Void),
            
            ASTOperation::AST_RECORD_DECL => self.analyze_record_decl_stmt(node),

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
            let result_type = self.analyze_and_mutate_expr(expr, &ast.meta)?;
            ast.result_type = result_type;
            return Ok(result_type);
        }

        panic!()
    }

    fn analyze_and_mutate_expr(&mut self, expr: &mut Expr, meta: &NodeMeta) -> SAResult {
        match expr {
            Expr::LitVal(litexpr) => self.analyze_lit_expr(litexpr),
            
            Expr::Binary(binexpr) => self.analyze_bin_expr(binexpr, meta),

            Expr::Ident(identexpr) => self.analyze_ident_expr(identexpr, meta),

            Expr::FuncCall(funccallexpr) => self.analyze_func_call_expr(funccallexpr, meta),

            Expr::RecordCreation(recexpr) => self.analyze_rec_creation_expr(recexpr),

            Expr::RecordFieldAccess(recfieldexpr) => self.analyze_record_field_access_expr(recfieldexpr, meta),
            
            Expr::Null => Ok(LitTypeVariant::Null),

            _ => todo!()
        }
    }

    fn analyze_bin_expr(&mut self, bin_expr: &mut BinExpr, meta: &NodeMeta) -> SAResult {
        let left_type: LitTypeVariant = self.analyze_and_mutate_expr(&mut bin_expr.left, meta)?;
        let right_type: LitTypeVariant = self.analyze_and_mutate_expr(&mut bin_expr.right, meta)?;

        let expr_type: LitTypeVariant = TypeChecker::check_bin_expr_type_compatability(
            left_type, 
            right_type, 
            bin_expr.operation,
            meta
        )?;

        bin_expr.result_type = expr_type;
        Ok(expr_type)
    }

    fn analyze_record_field_access_expr(&mut self, field_access: &mut RecordFieldAccessExpr, meta: &NodeMeta) -> SAResult {
        let ctx_borrow = self.ctx.borrow_mut();

        if let Some(rec_sym) = ctx_borrow.scope.deep_lookup(&field_access.rec_alias) {
            if let SymbolType::Record { name: rec_name } = &rec_sym.sym_type {
                if let Some(rec) = ctx_borrow.scope.lookup_record(rec_name) {
                    if let Some(field) = rec.fields.iter().find(|&field| field.name == field_access.field_chain[0]) {
                        field_access.rel_stack_off = field.rel_stack_off;
                        field_access.result_type = LitTypeVariant::from(field.typ);
                        return Ok(LitTypeVariant::from(field.typ));
                    }
                }
            }
            else {
                panic!("not a record type");
            }
        }
        let diag = Diagnostic {
            code: Some(ErrCode::SEM2001),
            severity: Severity::Error,
            primary_span: meta.span,
            secondary_spans: vec![],
            message: format!("type '{}' cannot be resolved", field_access.rec_name),
            notes: vec![]
        };
        Err(diag)
    }

    fn analyze_rec_creation_expr(&mut self, _rec_expr: &mut RecordCreationExpr) -> SAResult {
        Ok(LitTypeVariant::Record)
    }

    fn analyze_ident_expr(&mut self, ident_expr: &mut IdentExpr, meta: &NodeMeta) -> SAResult {
        let ctx_borrow = self.ctx.borrow();
        if let Some(ident) = ctx_borrow.scope.deep_lookup(&ident_expr.sym_name) {
            ident_expr.result_type = ident.lit_type;
            Ok(ident.lit_type)
        }
        else {
            let diag = Diagnostic {
                code: Some(ErrCode::SEM2001),
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
        let ctx_borrow = self.ctx.borrow();

        let func_sym_type = if let Some(func_sym) = ctx_borrow.scope.deep_lookup(&func_call.symbol_name) {
            if func_sym.sym_type != SymbolType::Function {
                let diag = Diagnostic {
                    code: Some(ErrCode::TYP2101),
                    severity: Severity::Error,
                    primary_span: meta.span,
                    secondary_spans: vec![],
                    message: format!("'{}' is not callable", func_sym.name),
                    notes: vec![]
                };
                return Err(diag);
            }
            else {
                func_sym.lit_type
            }
        }
        else {
            let diag = Diagnostic {
                code: Some(ErrCode::SEM2001),
                severity: Severity::Error,
                primary_span: meta.span,
                secondary_spans: vec![],
                message: format!("undefined symbol '{}'", func_call.symbol_name),
                notes: vec![]
            };
            return Err(diag);
        };

        let func_detail = ctx_borrow.scope.lookup_fn_by_name(func_call.symbol_name.as_str()).unwrap();
        func_call.id = func_detail.func_id;
        let func_param_types = func_detail.param_types.clone();

        drop(ctx_borrow);

        self.check_func_call_args(&mut func_call.args[..], &func_param_types, meta)?;            

        func_call.result_type = func_sym_type;
        Ok(func_sym_type)        
    }

    /// Check if the function arguments match the parameter types
    fn check_func_call_args(&mut self, args: &mut [(usize, Expr)], param_types: &[LitTypeVariant], meta: &NodeMeta) -> SAResult {
        if args.len() != param_types.len() {
            let diag = Diagnostic {
                code: Some(ErrCode::TYP2102),
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
            let assignment_ok: bool = expr_res == *param_type || TypeChecker::is_type_coalesciable(expr_res, *param_type);
            if !assignment_ok {
                let diag = Diagnostic {
                    code: Some(ErrCode::TYP2103),
                    severity: Severity::Error,
                    primary_span: meta.span,
                    secondary_spans: vec![],
                    message: format!("'{}' is not compatible with '{}'", param_type, expr_res),
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
        Ok(expr.result_type)
    }

    fn analyze_record_decl_stmt(&mut self, node: &mut AST) -> SAResult {
        if !node.kind.is_stmt() {
            panic!("Needed a RecordDeclStmt--but found {node:#?}");
        }

        Ok(LitTypeVariant::Record)
    }

    fn analyze_return_stmt(&mut self, node: &mut AST) -> SAResult {
        if !node.kind.is_stmt() {
            panic!("Needed a ReturnStmt--but found {node:#?}");
        }

        if let Some(Stmt::Return(ret_stmt)) = &mut node.kind.as_stmt_mut() {
            let ctx_borrow = self.ctx.borrow();
            // 'return' statements can appear only inside the functions
            // thus, the current function cannot be None; it's an error otherwise
            let expected_fn_ret_type: LitTypeVariant = ctx_borrow.scope.lookup_fn(ctx_borrow.scope.current_fn()).unwrap().return_type;

            drop(ctx_borrow);
            
            // if return statement returns some value
            let found_fn_ret_type: LitTypeVariant = if let Some(return_expr) = &mut node.left {
                self.analyze_expr(return_expr)?
            }
            else {
                LitTypeVariant::Void
            };

            let ret_typ_mismatch: bool = 
                !expected_fn_ret_type.is_void() && found_fn_ret_type.is_none() ||
                ((expected_fn_ret_type != found_fn_ret_type) && !TypeChecker::is_type_coalesciable(found_fn_ret_type, expected_fn_ret_type));

            if ret_typ_mismatch {
                let diag = Diagnostic {
                    code: Some(ErrCode::TYP2103),
                    severity: Severity::Error,
                    primary_span: node.meta.span,
                    secondary_spans: vec![],
                    message: format!("'{}' is not compatible with '{}'", found_fn_ret_type, expected_fn_ret_type),
                    notes: vec![]
                };
                return Err(diag);
            }  
            ret_stmt.func_id = self.ctx.borrow().scope.current_fn();
            return Ok(found_fn_ret_type);
        }
        panic!("Not a return statement!");
    }

    fn analyze_func_decl_stmt(&mut self, node: &mut AST) -> SAResult {
        if !node.kind.is_stmt() {
            panic!("Needed a FuncDecl--but found {:#?}", node);
        }

        if let Some(Stmt::FuncDecl(func_decl)) = &mut node.kind.as_stmt() {
            let mut ctx_borrow = self.ctx.borrow_mut();
            let func_ret_type = ctx_borrow
                .scope
                .root_scope()
                .lookup(&func_decl.name)
                .map(|func_sym| func_sym.lit_type);

            ctx_borrow.scope.update_current_func(func_decl.func_id);

            if func_ret_type.is_none() {
                let diag = Diagnostic {
                    code: Some(ErrCode::SEM2001),
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
            ctx_borrow.scope.enter_scope(func_decl.scope_id);
            drop(ctx_borrow);

            if let Some(func_body) = &mut node.left {
                self.analyze_node(func_body)?;
            }

            // exit function's scope
            self.ctx.borrow_mut().scope.exit_scope();
            // invalidate function id
            self.ctx.borrow_mut().scope.update_current_func(INVALID_FUNC_ID);
            return Ok(func_ret_type);
        }
        panic!("Not a function declaration statement!");
    }

    fn analyze_var_decl_stmt(&mut self, node: &mut AST) -> SAResult {
        if let Some(Stmt::VarDecl(var_decl)) = node.kind.as_stmt_mut() {
            let var_value_type: LitTypeVariant = self.analyze_expr(node.left.as_mut().unwrap())?;
            let mut ctx_borrow = self.ctx.borrow_mut();
            let curr_func_id = ctx_borrow.scope.current_fn();

            if let Some(var_sym) = ctx_borrow.scope.deep_lookup_mut(&var_decl.sym_name) {
                let var_type = Self::check_and_mutate_var_decl_stmt(var_sym, var_value_type, &node.meta)?;
                var_sym.func_id = Some(curr_func_id);
                var_sym.lit_type = var_type;
                node.result_type = var_type;
                return Ok(var_type);
            }
            let diag = Diagnostic {
                code: Some(ErrCode::SEM2001),
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
            && !is_type_coalescing_possible(expr_type, var_decl_sym.lit_type) 
        {
            let diag = Diagnostic {
                code: Some(ErrCode::TYP2104),
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
            self.ctx.borrow_mut().scope.enter_scope(if_stmt.scope_id);

            // every 'if' has an expression attached with it in its
            // left branch
            let cond_res: LitTypeVariant = self.analyze_expr(node.left.as_mut().unwrap())?;
            if cond_res != LitTypeVariant::I32 {
                let diag = Diagnostic {
                    code: Some(ErrCode::TYP2103),
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

            if let Some(else_body) = &mut node.right {
                self.analyze_node(else_body)?;
            }

            self.ctx.borrow_mut().scope.exit_scope();
            return Ok(cond_res);
        }
        Ok(LitTypeVariant::None)
    }
}