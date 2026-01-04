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
use kagc_symbol::function::INVALID_FUNC_ID;

use kagc_types::TyKind;
use kagc_utils::bug;

pub(crate) type TypeCheckResult<'tcx> = Option<TyKind<'tcx>>;

pub struct TypeChecker<'t, 'tcx> where 'tcx: 't {
    pub diagnostics: &'t DiagnosticBag,
    pub scope: &'tcx ScopeCtx<'tcx>,
    current_scope: ScopeId
}

impl<'t, 'tcx> TypeChecker<'t, 'tcx> where 'tcx: 't {
    pub fn new(scope: &'tcx ScopeCtx<'tcx>, diags: &'t DiagnosticBag) -> Self {
        Self { 
            diagnostics: diags,
            scope,
            current_scope: ScopeId(0)
        }
    }

    /// Start the analysis process
    /// 
    /// This starts an analysis process for the given list of nodes. 
    /// This function panics if it encounters any form of error.
    pub fn start_analysis(&mut self, nodes: &'tcx mut [AST<'tcx>]) {
        for node in nodes {
            self.analyze_node(node);
        }
    }

    fn analyze_node(&mut self, node: &'tcx mut AST<'tcx>) -> TypeCheckResult<'tcx> {
        let Some(_) = node.as_stmt() else {
            panic!("Only statement nodes are supported, but found {:#?}!", node);
        };
        match node.operation {
            ASTOperation::AST_VAR_DECL      => self.analyze_var_decl_stmt(node),
            ASTOperation::AST_FUNCTION      => self.analyze_func_decl_stmt(node),
            ASTOperation::AST_RETURN        => self.analyze_return_stmt(node),
            ASTOperation::AST_FUNC_CALL     => self.analyze_fn_call(node),
            ASTOperation::AST_LOOP          => self.analyze_node(node.left.as_mut().unwrap()),
            ASTOperation::AST_IF            => self.analyze_if_stmt(node),
            ASTOperation::AST_IMPORT        => Some(TyKind::Void),
            ASTOperation::AST_RECORD_DECL   => self.analyze_record_decl_stmt(node),
            ASTOperation::AST_BLOCK         => self.analyze_block_stmt(node),
            ASTOperation::AST_NONE 
            | ASTOperation::AST_BREAK       => Some(TyKind::None),
            _ => panic!("Operation '{:#?}' not supported!", node.operation)
        }
    }

    fn analyze_block_stmt(&mut self, node: &'tcx mut AST<'tcx>) -> TypeCheckResult<'tcx> {
        let block_stmt = node.expect_block_stmt_mut();
        for s in &mut block_stmt.statements {
            self.analyze_node(s)?;
        }
        Some(TyKind::None)
    }

    fn analyze_fn_call(&mut self, node: &mut AST<'tcx>) -> TypeCheckResult<'tcx> {
        // let call_expr = node.expect_function_call_expr_mut();
        // self.analyze_func_call_expr(call_expr, &node.meta)
        if let ASTKind::ExprAST(Expr::FuncCall(func_call_expr)) = &mut node.kind {
            self.analyze_func_call_expr(func_call_expr, &node.meta)
        }
        else {
            panic!()
        }
    }

    fn analyze_expr(&mut self, ast: &mut AST<'tcx>) -> TypeCheckResult<'tcx> {
        if !ast.kind.is_expr() {
            panic!("Needed an Expr--but found {:#?}", ast);
        }
        if let ASTKind::ExprAST(expr) = &mut ast.kind {
            ast.ty = self.analyze_and_mutate_expr(expr, &ast.meta);
            return ast.ty;
        }
        panic!()
    }

    fn analyze_and_mutate_expr(&mut self, expr: &mut Expr<'tcx>, meta: &NodeMeta) -> TypeCheckResult<'tcx> {
        match expr {
            Expr::LitVal(litexpr) => self.analyze_lit_expr(litexpr),
            Expr::Binary(binexpr) => self.analyze_bin_expr(binexpr, meta),
            Expr::Ident(identexpr) => self.analyze_ident_expr(identexpr, meta),
            Expr::FuncCall(funccallexpr) => self.analyze_func_call_expr(funccallexpr, meta),
            Expr::RecordCreation(recexpr) => self.analyze_rec_creation_expr(recexpr, meta),
            Expr::RecordFieldAccess(recfieldexpr) => self.analyze_record_field_access_expr(recfieldexpr, meta),
            Expr::Null => Some(TyKind::Null),
            _ => todo!()
        }
    }

    fn are_types_compatible(
        &mut self, 
        lhs: TyKind<'tcx>, rhs: TyKind<'tcx>, 
        op: ASTOperation, meta: &NodeMeta
    ) -> Option<TyKind<'tcx>> {
        match op {
            ASTOperation::AST_ADD |
            ASTOperation::AST_MULTIPLY |
            ASTOperation::AST_SUBTRACT |
            ASTOperation::AST_DIVIDE => {
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

    fn analyze_bin_expr(&mut self, bin_expr: &mut BinExpr<'tcx>, meta: &NodeMeta) -> TypeCheckResult<'tcx> {
        let left_type = self.analyze_and_mutate_expr(&mut bin_expr.left, meta)?;
        let right_type = self.analyze_and_mutate_expr(&mut bin_expr.right, meta)?;
        let result_type = self.are_types_compatible(left_type, right_type, bin_expr.operation, meta).unwrap();
        bin_expr.ty = result_type;
        Some(result_type)
    }

    fn analyze_record_field_access_expr(&mut self, field_access: &mut RecordFieldAccessExpr<'tcx>, meta: &NodeMeta) -> TypeCheckResult<'tcx> {
        if let Some(rec_sym) = self.scope.deep_lookup(Some(self.current_scope), field_access.rec_alias) {
            if let SymTy::Record { name } = rec_sym.sym_ty.get() {
                if let Some(rec) = self.scope.lookup_record(name) {
                    if let Some(field) = rec.fields.iter().find(|&field| field.name == field_access.field_chain[0]) {
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

    fn analyze_rec_creation_expr(&mut self, rec_expr: &mut RecordCreationExpr<'tcx>, meta: &NodeMeta) -> TypeCheckResult<'tcx> {
        for field in &mut rec_expr.fields {
            let ff = self.analyze_and_mutate_expr(&mut field.value, meta)?;
            if ff == TyKind::None {
                bug!("record's field expression evaluation resulted in type None")
            }
        }
        Some(TyKind::Record {
            name: rec_expr.name
        })
    }

    fn analyze_ident_expr(&mut self, ident_expr: &mut IdentExpr<'tcx>, meta: &NodeMeta) -> TypeCheckResult<'tcx> {
        if let Some(ident) = self.scope.deep_lookup(Some(self.current_scope), ident_expr.sym_name) {
            ident_expr.ty = ident.ty.get();
            Some(ident_expr.ty)
        }
        else {
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

    /// Analyze the function call expression
    /// 
    /// This analysis is done to check if the arguments to this 
    /// function call are valid.
    fn analyze_func_call_expr(&mut self, func_call: &mut FuncCallExpr<'tcx>, meta: &NodeMeta) -> TypeCheckResult<'tcx> {
        let Some(func_symbol) = self.scope.deep_lookup(Some(self.current_scope), func_call.symbol_name) else {
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
        let func_detail = self
            .scope
            .lookup_fn_by_name(func_call.symbol_name)
            .unwrap()
            .clone();
        func_call.id = func_detail.id.get();
        let func_param_types = func_detail.param_types.clone();

        self.check_func_call_args(
            &mut func_call.args[..], 
            &func_param_types, 
            meta
        )?;            

        func_call.ty = TyKind::Void;
        Some(TyKind::Void)        
    }

    /// Check if the function arguments match the parameter types
    fn check_func_call_args(&mut self, args: &mut [(usize, Expr<'tcx>)], param_types: &[TyKind], meta: &NodeMeta) -> TypeCheckResult<'tcx> {
        if args.len() != param_types.len() {
            let diag = Diagnostic {
                code: Some(ErrCode::TYP3001),
                severity: Severity::Error,
                primary_span: meta.span,
                secondary_spans: vec![],
                message: format!("argument length mismatch: expected '{}' but found '{}'", param_types.len(), args.len()),
                notes: vec![]
            };
            self.diagnostics.push(diag);
            return None;
        }

        for (idx, param_type) in param_types.iter().enumerate() {
            let expr_res = self.analyze_and_mutate_expr(&mut args[idx].1, meta)?;
            let assignment_ok: bool = true; // expr_res == *param_type || TypeChecker::is_type_coalesciable(expr_res.clone(), param_type.clone());
            if !assignment_ok {
                let diag = Diagnostic {
                    code: Some(ErrCode::TYP3002),
                    severity: Severity::Error,
                    primary_span: meta.span,
                    secondary_spans: vec![],
                    message: format!("'{:#?}' is not compatible with '{:#?}'", param_type, expr_res),
                    notes: vec![]
                };
                self.diagnostics.push(diag);
                return None;
            }
        }
        Some(TyKind::None) // placeholder; doesn't affect anything
    }

    /// There's nothing to be done here, actually. We don't care 
    /// what type of literal value we get, every literal value is 
    /// okay. The using expression might restraint from using it.
    fn analyze_lit_expr(&self, expr: &mut LitValExpr<'tcx>) -> TypeCheckResult<'tcx> {
        Some(expr.ty)
    }

    fn analyze_record_decl_stmt(&mut self, node: &mut AST<'tcx>) -> TypeCheckResult<'tcx> {
        if !node.kind.is_stmt() {
            panic!("Needed a RecordDeclStmt--but found {node:#?}");
        }

        Some(TyKind::Record {
            name: "empty 1"
        })
    }

    fn analyze_return_stmt(&mut self, node: &'tcx mut AST<'tcx>) -> TypeCheckResult<'tcx> {
        if !node.kind.is_stmt() {
            panic!("Needed a ReturnStmt--but found {node:#?}");
        }

        if let Some(Stmt::Return(ret_stmt)) = &mut node.kind.as_stmt_mut() {
            // 'return' statements can appear only inside the functions
            // thus, the current function cannot be None; it's an error otherwise
            let current_fn = self.scope.current_fn();
            let expected_fn_ret_type = self
                .scope
                .lookup_fn(current_fn)
                .unwrap()
                .ty;
            
            // if return statement returns some value
            let found_fn_ret_type = if let Some(return_expr) = &mut node.left {
                self.analyze_expr(return_expr)?
            }
            else {
                TyKind::Void
            };

            let missing_ret = !expected_fn_ret_type.is_void() && found_fn_ret_type.is_none();
            let incompatible_ret_chk1 = expected_fn_ret_type != found_fn_ret_type;
            let incompatible_ret_chk2 = true; 
            // !TypeChecker::is_type_coalesciable(
            //     found_fn_ret_type.clone(), 
            //     expected_fn_ret_type.clone()
            // );

            let incompatible_ret = incompatible_ret_chk1 && incompatible_ret_chk2;
            let ret_typ_mismatch = missing_ret || incompatible_ret;

            if ret_typ_mismatch {
                let diag = Diagnostic {
                    code: Some(ErrCode::TYP3002),
                    severity: Severity::Error,
                    primary_span: node.meta.span,
                    secondary_spans: vec![],
                    message: format!(
                        "'{:#?}' is not compatible with '{:#?}'", 
                        found_fn_ret_type, 
                        expected_fn_ret_type
                    ),
                    notes: vec![]
                };
                self.diagnostics.push(diag);
                return None;
            }  
            ret_stmt.func_id = self.scope.current_fn();
            return Some(found_fn_ret_type);
        }
        panic!("Not a return statement!");
    }

    fn analyze_func_decl_stmt(&mut self, node: &'tcx mut AST<'tcx>) -> TypeCheckResult<'tcx> {
        if !node.kind.is_stmt() {
            panic!("Needed a FuncDecl--but found {:#?}", node);
        }

        if let Some(Stmt::FuncDecl(func_decl)) = &mut node.kind.as_stmt() {
            let func_ret_type = self
                .scope
                .root_scope()
                .get_sym(func_decl.name)
                .map(|func_sym| func_sym.ty.clone());

            self.scope.update_current_func(func_decl.func_id);

            if func_ret_type.is_none() {
                let diag = Diagnostic {
                    code: Some(ErrCode::SEM2000),
                    severity: Severity::Error,
                    primary_span: node.meta.span,
                    secondary_spans: vec![],
                    message: format!("undefined symbol '{}'", func_decl.name),
                    notes: vec![]
                };
                self.diagnostics.push(diag);
                return None;
            };

            // unwrap the return type
            let func_ret_type: TyKind = TyKind::Void;

            // switch to function's scope
            self.scope.enter_scope(ScopeId(func_decl.scope_id));

            if let Some(func_body) = &mut node.left {
                self.analyze_node(func_body)?;
            }

            // exit function's scope
            self.scope.exit_scope();
            // invalidate function id
            self.scope.update_current_func(INVALID_FUNC_ID);
            return Some(func_ret_type);
        }
        panic!("Not a function declaration statement!");
    }

    fn analyze_var_decl_stmt(&mut self, node: &'tcx mut AST<'tcx>) -> TypeCheckResult<'tcx> {
        if let Some(Stmt::VarDecl(var_decl)) = node.kind.as_stmt_mut() {
            let var_value_type: TyKind = self.analyze_expr(node.left.as_mut().unwrap())?;
            let curr_func_id = self.scope.current_fn();

            if let Some(var_sym) = self.scope.deep_lookup(Some(self.current_scope), var_decl.sym_name) {
                let var_type = self.check_and_mutate_var_decl_stmt(var_sym, var_value_type, &node.meta)?;
                
                if let TyKind::Record{ name } = &var_value_type {
                    var_sym.sym_ty.replace(SymTy::Record { name });
                }

                // var_sym.func_id = Some(curr_func_id);
                // var_sym.lit_type = var_type.clone();
                node.ty = Some(var_type);
                return Some(var_type);
            }
            let diag = Diagnostic {
                code: Some(ErrCode::SEM2000),
                severity: Severity::Error,
                primary_span: node.meta.span,
                secondary_spans: vec![],
                message: format!("undefined symbol '{}'", var_decl.sym_name),
                notes: vec![]
            };
            self.diagnostics.push(diag);
            return None;
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
    pub fn check_and_mutate_var_decl_stmt(&mut self, var_decl_sym: &Sym<'tcx>, expr_type: TyKind<'tcx>, meta: &NodeMeta) -> Option<TyKind<'tcx>> {
        if var_decl_sym.ty.get() != TyKind::None {
            match expr_type {
                // implicitly convert no-type-annotated byte-type into an integer
                TyKind::U8 => {
                    // var_decl_sym.lit_type = TyKind::I64;
                    return Some(TyKind::I64);
                },

                // TyKind::Str => {
                    // var_decl_sym.lit_type = TyKind::RawStr;
                    // return Some(TyKind::Str);
                // },

                TyKind::Str => {
                    // var_decl_sym.lit_type = TyKind::PoolStr;
                    return Some(TyKind::PoolStr);
                }
                
                _ => return Some(expr_type)
            }
        }
        if false
            // var_decl_sym.lit_type != expr_type 
            // && !is_type_coalescing_possible(expr_type.clone(), var_decl_sym.lit_type.clone()) 
        {
            let diag = Diagnostic {
                code: Some(ErrCode::TYP3003),
                severity: Severity::Error,
                primary_span: meta.span,
                secondary_spans: vec![],
                message: format!("expected type `{:#?}`, found `{:#?}`", var_decl_sym.ty, expr_type),
                notes: vec![]
            };
            self.diagnostics.push(diag);
            return None;
        }
        Some(expr_type)
    }

    fn analyze_if_stmt(&mut self, node: &'tcx mut AST<'tcx>) -> TypeCheckResult<'tcx> {
        let if_stmt = node.expect_if_stmt();
        self.scope.enter_scope(ScopeId(if_stmt.scope_id));

        // every 'if' has an expression attached with it in its
        // left branch
        let Some(expr_node) = node.left.as_mut() else {
            bug!("an If node without an expression node in its left branch is invalid")
        };
        let cond_res = self.analyze_expr(expr_node)?;
        if cond_res != TyKind::I64 {
            let diag = Diagnostic {
                code: Some(ErrCode::TYP3002),
                severity: Severity::Error,
                primary_span: node.meta.span,
                secondary_spans: vec![],
                message: format!("'{:#?}' is not compatible with '{:#?}'", cond_res, TyKind::I64),
                notes: vec![]
            };
            self.diagnostics.push(diag);
            return None;
        }

        if let Some(if_body) = &mut node.mid {
            self.analyze_node(if_body)?;
        }
        self.scope.exit_scope();

        // else-block
        if let Some(right_tree) = &mut node.right {
            if let Some(else_block_tree) = &mut right_tree.left {
                if let ASTKind::StmtAST(Stmt::Scoping(scoping_stmt)) = &else_block_tree.kind {
                    self.scope.enter_scope(ScopeId(scoping_stmt.scope_id));
                }
                self.analyze_node(else_block_tree)?;
                self.scope.exit_scope();
            }
        }
        Some(TyKind::None)
    }
}