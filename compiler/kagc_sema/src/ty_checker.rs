// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_ast::*;
use kagc_errors::code::ErrCode;
use kagc_errors::diagnostic::DiagnosticBag;
use kagc_errors::diagnostic::Severity;
use kagc_errors::diagnostic::Diagnostic;
use kagc_scope::ctx::ScopeCtx;
use kagc_scope::scope::ScopeId;
use kagc_symbol::Sym;
use kagc_symbol::SymTy;
use kagc_symbol::function::FuncId;
use kagc_symbol::function::INVALID_FUNC_ID;

use kagc_types::TyKind;
use kagc_utils::bug;

pub(crate) type TypeCheckResult<'t> = Option<TyKind<'t>>;

pub struct TypeChecker<'t, 'tcx> {
    diagnostics: &'t DiagnosticBag,
    scope: &'tcx ScopeCtx<'tcx>,
    curr_func_id: FuncId,
}

impl<'t, 'tcx> TypeChecker<'t, 'tcx> {
    pub fn new(
        scope: &'tcx ScopeCtx<'tcx>, 
        diags: &'t DiagnosticBag,
    ) -> Self {
        Self { 
            diagnostics: diags,
            scope,
            curr_func_id: FuncId(INVALID_FUNC_ID)
        }
    }

    /// Start the type checking process
    /// 
    /// This starts an type checking process for the given list of nodes. 
    /// This function panics if it encounters any form of error.
    pub fn check(&mut self, nodes: &mut [AstNode<'tcx>]) {
        for node in nodes {
            self.check_node(node);
        }
    }

    fn check_node(&mut self, node: &mut AstNode<'tcx>) -> TypeCheckResult<'tcx> {
        match node.op {
            AstOp::VarDecl      => self.check_var_decl_stmt(node),
            AstOp::Func         => self.check_func_decl_stmt(node),
            AstOp::Return       => self.check_return_stmt(node),
            AstOp::FuncCall     => self.check_function_call(node),
            AstOp::Loop         => self.check_loop_stmt(node),
            AstOp::If           => self.check_if_stmt(node),
            AstOp::Import       => Some(TyKind::Void),
            AstOp::RecDecl      => self.check_record_decl_stmt(node),
            AstOp::Block        => self.check_block_stmt(node),
            AstOp::None 
            | AstOp::Break      => Some(TyKind::None),
            _ => panic!("Operation '{:#?}' not supported!", node.op)
        }
    }

    fn check_block_stmt(&mut self, node: &mut AstNode<'tcx>) -> TypeCheckResult<'tcx> {
        let block_stmt = node.expect_block_stmt_mut();
        for s in &mut block_stmt.statements {
            self.check_node(s)?;
        }
        Some(TyKind::None)
    }

    fn check_function_call(&mut self, node: &mut AstNode<'tcx>) -> TypeCheckResult<'tcx> {
        let meta = node.meta.clone();
        let call_expr = node.expect_func_call_expr_mut();
        self.check_func_call_expr(call_expr, &meta)
    }

    fn check_expr(&mut self, node: &mut AstNode<'tcx>) -> TypeCheckResult<'tcx> {
        let meta = node.meta.clone();
        let Some(expr) = node.as_expr_mut() else {
            bug!("expected expr")
        };
        node.ty = self.check_and_mutate_expr(expr, &meta);
        node.ty
    }

    fn check_and_mutate_expr(&mut self, expr: &mut Expr<'tcx>, meta: &NodeMeta) -> TypeCheckResult<'tcx> {
        match expr {
            Expr::LitVal(litexpr) => self.check_literal_expr(litexpr),
            Expr::Binary(binexpr) => self.check_bin_expr(binexpr, meta),
            Expr::Ident(identexpr) => self.check_ident_expr(identexpr, meta),
            Expr::FuncCall(funccallexpr) => self.check_func_call_expr(funccallexpr, meta),
            Expr::RecordCreation(recexpr) => self.check_rec_creation_expr(recexpr, meta),
            Expr::RecordFieldAccess(recfieldexpr) => self.check_record_field_access_expr(recfieldexpr, meta),
            Expr::Null => Some(TyKind::Null),
            _ => todo!()
        }
    }

    fn are_types_compatible(
        &mut self, 
        lhs: TyKind<'tcx>, rhs: TyKind<'tcx>, 
        op: AstOp, meta: &NodeMeta
    ) -> Option<TyKind<'tcx>> {
        match op {
            AstOp::Add |
            AstOp::Multiply |
            AstOp::Subtract |
            AstOp::Divide => {
                match (lhs, rhs) {
                    (TyKind::I64, TyKind::I64) => Some(TyKind::I64),
                    (TyKind::U8, TyKind::U8) => Some(TyKind::I64),
                    _ => {
                        self.diagnostics.push(
                            Diagnostic {
                                code: Some(ErrCode::TYP3002),
                                severity: Severity::Error,
                                primary_span: meta.span,
                                secondary_spans: vec![],
                                message: format!("'{:#?}' is not compatible with '{:#?}'", lhs, rhs),
                                notes: vec![]
                            }
                        );
                        None
                    }
                }
            }
            _ => None
        }
    }

    pub fn is_type_coalesciable(src: TyKind, dest: TyKind) -> bool {
        match src {
            TyKind::U8 => matches!(dest, TyKind::U8 | TyKind::I64),
            TyKind::I64 => matches!(dest, TyKind::I64),
            TyKind::Str => matches!(dest, TyKind::PoolStr),
            TyKind::PoolStr => matches!(dest, TyKind::Str),
            TyKind::Record { name: rec_name } => {
                match dest {
                    TyKind::Record { name } => name == rec_name,
                    _ => false
                }
            }
            _ => false
        }
    }

    fn check_bin_expr(&mut self, bin_expr: &mut BinExpr<'tcx>, meta: &NodeMeta) -> TypeCheckResult<'tcx> {
        let left_type = self.check_and_mutate_expr(&mut bin_expr.left, meta)?;
        let right_type = self.check_and_mutate_expr(&mut bin_expr.right, meta)?;
        let result_type = self.are_types_compatible(left_type, right_type, bin_expr.operation, meta).unwrap();
        bin_expr.ty = result_type;
        Some(result_type)
    }

    fn check_record_field_access_expr(&mut self, field_access: &mut RecordFieldAccessExpr<'tcx>, meta: &NodeMeta) -> TypeCheckResult<'tcx> {
        if let Some(rec_sym) = self.scope.lookup_sym(None, field_access.rec_alias) {
            if let SymTy::Record { name } = rec_sym.sym_ty.get() {
                if let Some(rec) = self.scope.lookup_record(name) {
                    if let Some(field) = rec.fields.iter().find(|&field| field.name == field_access.field_name) {
                        field_access.rel_stack_off = field.rel_stack_off;
                        field_access.ty = field.ty;
                        field_access.rec_name = rec.name;
                        return Some(field.ty);
                    }
                }
            }
            else {
                self.diagnostics.push(
                    Diagnostic {
                        code: Some(ErrCode::SEM2000),
                        severity: Severity::Error,
                        primary_span: meta.span,
                        secondary_spans: vec![],
                        message: format!("'{}' is not a record type", field_access.rec_name),
                        notes: vec![]
                    }
                );
                return None;
            }
        }
        self.diagnostics.push(
            Diagnostic {
                code: Some(ErrCode::SEM2000),
                severity: Severity::Error,
                primary_span: meta.span,
                secondary_spans: vec![],
                message: format!("type '{}' cannot be resolved", field_access.rec_name),
                notes: vec![]
            }
        );
        None
    }

    fn check_rec_creation_expr(&mut self, rec_expr: &mut RecordCreationExpr<'tcx>, meta: &NodeMeta) -> TypeCheckResult<'tcx> {
        for field in &mut rec_expr.fields {
            let field_ty = self.check_and_mutate_expr(&mut field.value, meta)?;
            if field_ty == TyKind::None {
                bug!("record's field expression evaluation resulted in type None")
            }
        }
        Some(TyKind::Record { name: rec_expr.name })
    }

    fn check_ident_expr(&mut self, ident_expr: &mut IdentExpr<'tcx>, meta: &NodeMeta) -> TypeCheckResult<'tcx> {
        if let Some(ident) = self.scope.lookup_sym(None, ident_expr.sym_name) {
            ident_expr.ty = ident.ty.get();
            Some(ident_expr.ty)
        }
        else {
            self.scope.dump();
            self.diagnostics.push(
                Diagnostic {
                    code: Some(ErrCode::SEM2000),
                    severity: Severity::Error,
                    primary_span: meta.span,
                    secondary_spans: vec![],
                    message: format!("undefined symbol '{}'", ident_expr.sym_name),
                    notes: vec![]
                }
            );
            None
        }
    }

    fn check_func_call_expr(&mut self, func_call: &mut FuncCallExpr<'tcx>, meta: &NodeMeta) -> TypeCheckResult<'tcx> {
        let Some(func_symbol) = self.scope.lookup_sym(None, func_call.symbol_name) else {
            self.diagnostics.push(
                Diagnostic {
                    code: Some(ErrCode::SEM2000),
                    severity: Severity::Error,
                    primary_span: meta.span,
                    secondary_spans: vec![],
                    message: format!("undefined symbol '{}'", func_call.symbol_name),
                    notes: vec![]
                }
            );
            return None;
        };
        if func_symbol.sym_ty.get() != SymTy::Function {
            self.diagnostics.push(
                Diagnostic {
                    code: Some(ErrCode::TYP3000),
                    severity: Severity::Error,
                    primary_span: meta.span,
                    secondary_spans: vec![],
                    message: format!("'{}' is not callable", func_symbol.name),
                    notes: vec![]
                }
            );
            return None;
        }
        let func_details = self
            .scope
            .lookup_fn_by_name(func_call.symbol_name)
            .unwrap()
            .clone();
        func_call.id = func_details.id.get();
        let func_param_types = func_details.param_types.clone();

        let mut func_call_args: Vec<&mut Expr<'tcx>> = func_call
            .args
            .iter_mut()
            .map(|arg| &mut arg.1)
            .collect();

        self.check_func_call_args(
            &mut func_call_args, 
            &func_param_types, 
            meta
        );            

        func_call.ty = func_details.ty;
        Some(func_details.ty)        
    }

    /// Check if the function arguments match the parameter types
    fn check_func_call_args(&mut self, args: &mut [&mut Expr<'tcx>], param_types: &[TyKind], meta: &NodeMeta) {
        if args.len() != param_types.len() {
            self.diagnostics.push(
                Diagnostic {
                    code: Some(ErrCode::TYP3001),
                    severity: Severity::Error,
                    primary_span: meta.span,
                    secondary_spans: vec![],
                    message: format!(
                        "expected '{}' arguments but found '{}'", 
                        param_types.len(), 
                        args.len()
                    ),
                    notes: vec![]
                }
            );
        }

        for (arg, param_type) in args.iter_mut().zip(param_types.iter()) {
            let Some(expr_res) = self.check_and_mutate_expr(arg, meta) else {
                bug!("Argument's result type cannot be determined")
            };
            let assignment_ok = (expr_res == *param_type) 
                || TypeChecker::is_type_coalesciable(expr_res, *param_type);
            if !assignment_ok {
                self.diagnostics.push(
                    Diagnostic {
                        code: Some(ErrCode::TYP3002),
                        severity: Severity::Error,
                        primary_span: meta.span,
                        secondary_spans: vec![],
                        message: format!("'{param_type}' is not compatible with '{expr_res}'"),
                        notes: vec![]
                    }
                );
            }
        }
    }

    /// There's nothing to be done here, actually. Just return the type.
    fn check_literal_expr(&self, expr: &mut LitValExpr<'tcx>) -> TypeCheckResult<'tcx> {
        let lit_ty = expr.value.kind();
        expr.ty = lit_ty;
        Some(lit_ty)
    }

    fn check_record_decl_stmt(&mut self, node: &AstNode<'tcx>) -> TypeCheckResult<'tcx> {
        let record_decl = node.expect_record_decl_stmt();
        Some(TyKind::Record { name: record_decl.name })
    }

    fn check_return_stmt(&mut self, node: &mut AstNode<'tcx>) -> TypeCheckResult<'tcx> {
        node.expect_return_stmt(); // make sure its a return statement

        // 'return' statements can appear only inside functions.
        // Thus, the current function cannot be None. If it is None, then 
        // that's a huge bug.
        let expected_ret_ty = self
            .scope
            .lookup_fn(self.curr_func_id)
            .unwrap()
            .ty;
            
        // if return statement returns some value
        let found_ret_ty = if let Some(return_expr) = &mut node.left {
            self.check_expr(return_expr)?
        }
        else {
            TyKind::Void
        };

        let missing_ret = !expected_ret_ty.is_void() && found_ret_ty.is_none();
        let incompatible_ret_chk1 = expected_ret_ty != found_ret_ty;
        let incompatible_ret_chk2 = !found_ret_ty.is_compatible_with(&expected_ret_ty); 
        let incompatible_ret = incompatible_ret_chk1 && incompatible_ret_chk2;

        if missing_ret || incompatible_ret {
            self.diagnostics.push(
                Diagnostic {
                    code: Some(ErrCode::TYP3002),
                    severity: Severity::Error,
                    primary_span: node.meta.span,
                    secondary_spans: vec![],
                    message: format!(
                        "'{:#?}' is not compatible with '{:#?}'", 
                        found_ret_ty, 
                        expected_ret_ty
                    ),
                    notes: vec![]
                }
            );
            return None;
        }  

        let return_stmt = node.expect_return_stmt_mut();
        return_stmt.func_id = self.curr_func_id;
        Some(found_ret_ty)
    }

    fn check_func_decl_stmt(&mut self, node: &mut AstNode<'tcx>) -> TypeCheckResult<'tcx> {
        let meta_span = node.meta.span;
        let func_decl = node.expect_func_decl_stmt_mut();

        let Some(func) = self.scope.lookup_fn_by_name(func_decl.name) else {
            self.diagnostics.push(
                Diagnostic {
                    code: Some(ErrCode::SEM2000),
                    severity: Severity::Error,
                    primary_span: meta_span,
                    secondary_spans: vec![],
                    message: format!("undefined symbol '{}'", func_decl.name),
                    notes: vec![]
                }
            );
            return None;
        };

        let func_ty = func.ty;
        self.curr_func_id = func.id.get();

        if let Some(func_body) = &mut node.left {
            let Some(scope) = self.scope.lookup_node_scope(node.id) else {
                bug!("No scope attached with node id '{:#?}'!", node.id);
            };

            self.scope.enter(scope.id.get()); // enter function's scope

            let body_block = func_body.expect_block_stmt_mut();
            for stmt in &mut body_block.statements {
                self.check_node(stmt);
            }

            self.scope.pop(); // exit function's scope
        }

        self.curr_func_id = FuncId(INVALID_FUNC_ID);
        Some(func_ty)
    }

    fn check_var_decl_stmt(&mut self, node: &mut AstNode<'tcx>) -> TypeCheckResult<'tcx> {
        let var_decl = node.expect_var_decl_stmt();

        let Some(var_sym) = self.scope.lookup_sym(None, var_decl.sym_name) else {

            self.diagnostics.push(
                Diagnostic {
                    code: Some(ErrCode::SEM2000),
                    severity: Severity::Error,
                    primary_span: node.meta.span,
                    secondary_spans: vec![],
                    message: format!("undefined symbol '{}'", var_decl.sym_name),
                    notes: vec![]
                }
            );
            return None;
        };

        let var_value_type = self.check_expr(node.left.as_mut().unwrap())?;
        let var_type = self.check_and_mutate_var_decl_stmt(var_sym, var_value_type, &node.meta)?;
            
        if let TyKind::Record{ name } = &var_value_type {
            var_sym.sym_ty.replace(SymTy::Record { name });
        }

        var_sym.function_id.replace(self.curr_func_id);
        node.ty = Some(var_type);

        Some(var_type)
    }

    fn check_and_mutate_var_decl_stmt(
        &mut self, 
        var_decl_sym: &'tcx Sym<'tcx>, 
        expr_type: TyKind<'tcx>, 
        meta: &NodeMeta
    ) -> Option<TyKind<'tcx>> {
        if (var_decl_sym.ty.get() != expr_type) && expr_type.is_compatible_with(&var_decl_sym.ty.get()) {
            self.diagnostics.push(
                Diagnostic {
                    code: Some(ErrCode::TYP3003),
                    severity: Severity::Error,
                    primary_span: meta.span,
                    secondary_spans: vec![],
                    message: format!(
                        "expected type `{:#?}`, found `{:#?}`", 
                        var_decl_sym.ty, 
                        expr_type
                    ),
                    notes: vec![]
                }
            );
            return None;
        }
        match expr_type {
            TyKind::U8 | TyKind::I64 => { var_decl_sym.ty.replace(TyKind::I64); },
            TyKind::Str => { var_decl_sym.ty.replace(TyKind::Str); },
            _ => ()
        };
        Some(expr_type)
    }

    fn check_if_stmt(&mut self, node: &mut AstNode<'tcx>) -> TypeCheckResult<'tcx> {
        node.expect_if_stmt();
        self.scope.enter(ScopeId(0));

        // every 'if' has an expression attached with it in its left branch
        let Some(expr_node) = node.left.as_mut() else {
            bug!("an If node without an expression node in its left branch is invalid")
        };
        let cond_res = self.check_expr(expr_node)?;

        if cond_res != TyKind::I64 {
            self.diagnostics.push(
                Diagnostic {
                    code: Some(ErrCode::TYP3002),
                    severity: Severity::Error,
                    primary_span: node.meta.span,
                    secondary_spans: vec![],
                    message: format!("'{:#?}' is not compatible with '{:#?}'", cond_res, TyKind::I64),
                    notes: vec![]
                }
            );
            return None;
        }

        if let Some(if_body) = &mut node.mid {
            let node_id = if_body.id;
            let Some(then_scope) = self.scope.lookup_node_scope(node_id) else {
                bug!("No scope attached with node id '{:#?}'!", node_id);
            };

            self.scope.enter(then_scope.id.get());
            self.check_node(if_body)?;
            self.scope.pop();
        }

        // else-block
        if let Some(right_tree) = &mut node.right {
            if let Some(else_body) = &mut right_tree.left {
                let node_id = else_body.id;
                let Some(else_scope) = self.scope.lookup_node_scope(node_id) else {
                    bug!("No scope attached with node id '{:#?}'!", node_id);
                };

                self.scope.enter(else_scope.id.get());
                self.check_node(else_body)?;
                self.scope.pop();
            }
        }
        Some(TyKind::None)
    }

    fn check_loop_stmt(&mut self, node: &mut AstNode<'tcx>) -> TypeCheckResult<'tcx> {
        node.expect_loop_stmt();

        if let Some(loop_body) = &mut node.left {
            let node_id = loop_body.id;
            let Some(loop_scope) = self.scope.lookup_node_scope(node_id) else {
                bug!("No scope attached with node id '{:#?}'!", node_id);
            };

            self.scope.enter(loop_scope.id.get());
            self.check_node(loop_body)?;
            self.scope.pop();
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use kagc_errors::diagnostic::DiagnosticBag;
    use kagc_scope::{ctx::ScopeCtx, scope::Scope};
    use kagc_symbol::{Sym, function::Func};
    use kagc_types::record::RecordType;

    use crate::TypeChecker;

    #[test]
    fn test_sth() {
        let sym_arena = typed_arena::Arena::<Sym>::new();
        let rec_arena = typed_arena::Arena::<RecordType>::new();
        let func_arena = typed_arena::Arena::<Func>::new();
        let scope_arena = typed_arena::Arena::<Scope>::new();
        let cx = ScopeCtx::new(
            &sym_arena, 
            &func_arena,
            &rec_arena,
            &scope_arena
        );
        let d = DiagnosticBag::default();
        let mut tc = TypeChecker::new(&cx, &d);
        tc.check(&mut []);
    }
}