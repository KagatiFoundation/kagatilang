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
use kagc_symbol::SymbolType;
use kagc_token::Token;
use kagc_types::LitTypeVariant;

use crate::{
    errors::*, 
    type_checker::TypeChecker, 
    typedefs::SAResult
};

pub struct SemanticAnalyzer {
    pub ctx: Rc<RefCell<CompilerCtx>>,
}

impl SemanticAnalyzer {
    pub fn new(ctx: Rc<RefCell<CompilerCtx>>) -> Self {
        Self {
            ctx
        }
    }

    /// Start the analysis process
    /// 
    /// This starts an analysis process for the given list of nodes. 
    /// This function panics if it encounters any form of error.
    pub fn start_analysis(&mut self, nodes: &mut Vec<AST>) {
        for node in nodes {
            let result: SAResult = self.analyze_node(node);
            if let Err(analysis_err) = result {
                analysis_err.dump();
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
            }

            _ => panic!("'{:?}' is not supported ASTOperation for 'analyze_node' yet!", node.operation)
        }
    }

    fn analyze_fn_call(&mut self, func_call: &mut AST) -> SAResult {
        if let ASTKind::ExprAST(Expr::FuncCall(func_call_expr)) = &mut func_call.kind {
            self.analyze_func_call_expr(func_call_expr)
        }
        else if let ASTKind::StmtAST(Stmt::FuncCall(func_call_stmt)) = &mut func_call.kind {
            self.analyze_func_call_stmt(func_call_stmt)
        }
        else {
            panic!()
        }
    }

    fn analyze_func_call_stmt(&mut self, func_call: &mut FuncCallStmt) -> SAResult {
        let ctx_borrow = self.ctx.borrow();

        if let Some(live_scope) = ctx_borrow.live_scope() {
            if let Some(func_sym) = live_scope.lookup(&func_call.symbol_name) {
                if func_sym.sym_type != SymbolType::Function {
                    return Err(
                        SAError::TypeError(
                            SATypeError::NonCallable { 
                                sym_name: func_sym.name.clone() 
                            }
                        )
                    );
                }

                func_call.result_type = func_sym.lit_type;
                return Ok(func_sym.lit_type);
            }
        }
        Err(
            SAError::UndefinedSymbol { 
                sym_name: func_call.symbol_name.clone(), 
                token: Token::none() 
            }
        )
    }

    fn analyze_expr(&mut self, ast: &mut AST) -> SAResult {
        if !ast.kind.is_expr() {
            panic!("Needed an Expr--but found {:#?}", ast);
        }

        if let ASTKind::ExprAST(expr) = &mut ast.kind {
            return self.analyze_and_mutate_expr(expr);
        }

        panic!()
    }

    fn analyze_and_mutate_expr(&mut self, expr: &mut Expr) -> SAResult {
        match expr {
            Expr::LitVal(litexpr) => self.analyze_lit_expr(litexpr),
            
            Expr::Binary(binexpr) => self.analyze_bin_expr(binexpr),

            Expr::Ident(identexpr) => {
                self.analyze_ident_expr(identexpr)
            },

            Expr::FuncCall(funccallexpr) => self.analyze_func_call_expr(funccallexpr),
            
            Expr::Null => Ok(LitTypeVariant::Null),

            _ => todo!()
        }
    }

    fn analyze_bin_expr(&mut self, bin_expr: &mut BinExpr) -> SAResult {
        let left_type: LitTypeVariant = self.analyze_and_mutate_expr(&mut bin_expr.left)?;
        let right_type: LitTypeVariant = self.analyze_and_mutate_expr(&mut bin_expr.right)?;

        let expr_type: LitTypeVariant = TypeChecker::check_bin_expr_type_compatability(
            left_type, 
            right_type, 
            bin_expr.operation
        )?;

        Ok(expr_type)
    }

    fn analyze_ident_expr(&mut self, ident_expr: &mut IdentExpr) -> SAResult {
        let ctx_borrow = self.ctx.borrow();

        if let Some(ident) = ctx_borrow.deep_lookup(&ident_expr.sym_name) {
            Ok(ident.lit_type)
        }
        else {
            Err(
                SAError::UndefinedSymbol { 
                    sym_name: ident_expr.sym_name.clone(), 
                    token: Token::none() 
                }
            )
        }
    }

    /// Analyze the function call expression
    /// 
    /// This analysis is done to check if the arguments to this 
    /// function call are valid.
    fn analyze_func_call_expr(&mut self, func_call: &mut FuncCallExpr) -> SAResult {
        let ctx_borrow = self.ctx.borrow();

        let func_sym_type = if let Some(func_sym) = ctx_borrow.deep_lookup(&func_call.symbol_name) {
            if func_sym.sym_type != SymbolType::Function {
                return Err(
                    SAError::TypeError(
                        SATypeError::NonCallable { 
                            sym_name: func_sym.name.clone() 
                        }
                    )
                );
            }
            else {
                func_sym.lit_type
            }
        }
        else {
            return Err(
                SAError::UndefinedSymbol { 
                    sym_name: func_call.symbol_name.clone(), 
                    token: Token::none() 
                }
            );
        };

        let func_param_types = ctx_borrow
            .func_table
            .get(&func_call.symbol_name)
            .unwrap()
            .param_types
            .clone();

        drop(ctx_borrow);

        self.check_func_call_args(&mut func_call.args[..], &func_param_types)?;            

        func_call.result_type = func_sym_type;
        Ok(func_sym_type)        
    }

    /// Check if the function arguments match the parameter types
    fn check_func_call_args(
        &mut self,
        args: &mut [(usize, Expr)],
        param_types: &[LitTypeVariant],
    ) -> SAResult {
        if args.len() != param_types.len() {
            return Err(
                SAError::ArgLengthMismatch { 
                    expected: args.len(), 
                    found: param_types.len() 
                }
            );
        }

        for (idx, param_type) in param_types.iter().enumerate() {
            let expr_res: LitTypeVariant = self.analyze_and_mutate_expr(&mut args[idx].1)?;
            let assignment_ok: bool = expr_res == *param_type || TypeChecker::is_type_coalesciable(expr_res, *param_type);

            if !assignment_ok {
                return Err(
                    SAError::TypeError(
                        SATypeError::TypeMismatch { 
                            expected: *param_type, 
                            found: expr_res 
                        }
                    )
                );
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

        if let Some(Stmt::Return(ret_stmt)) = &mut node.kind.as_stmt() {
            let ctx_borrow = self.ctx.borrow();
            // println!("{:#?}", ctx_borrow.lookup_fn(ret_stmt.func_id));
            
            // 'return' statements can appear only inside the functions
            // thus, the current function cannot be None; it's an error otherwise
            let expected_fn_ret_type: LitTypeVariant = ctx_borrow.lookup_fn(ret_stmt.func_id).unwrap().return_type;

            drop(ctx_borrow);
            
            // if return statement returns some value
            let found_fn_ret_type: LitTypeVariant = if let Some(return_expr) = &mut node.left {
                self.analyze_expr(return_expr)?
            }
            else {
                LitTypeVariant::Void
            };

            if expected_fn_ret_type.is_void() 
                && (found_fn_ret_type.is_none() || !found_fn_ret_type.is_void()) {
                return Err(
                    SAError::TypeError(
                        SATypeError::ReturnType(
                            SAReturnTypeError::ExpectedNoReturnValue { 
                                found: found_fn_ret_type
                            }
                        )
                    )
                );
            }

            let ret_typ_mismatch: bool = 
                !expected_fn_ret_type.is_void() && found_fn_ret_type.is_none() ||
                ((expected_fn_ret_type != found_fn_ret_type) && !TypeChecker::is_type_coalesciable(found_fn_ret_type, expected_fn_ret_type));

            if ret_typ_mismatch {
                return Err(
                    SAError::TypeError(
                        SATypeError::ReturnType(
                            SAReturnTypeError::TypeMismatch { 
                                expected: expected_fn_ret_type, 
                                found: found_fn_ret_type 
                            }
                        )
                    )
                );
            }  
            return Ok(found_fn_ret_type);
        }
        panic!("Not a return statement!");
    }

    fn analyze_func_decl_stmt(&mut self, node: &mut AST) -> SAResult {
        if !node.kind.is_stmt() {
            panic!("Needed a FuncDecl--but found {:#?}", node);
        }

        if let Some(Stmt::FuncDecl(func_decl)) = &mut node.kind.as_stmt() {
            // switch to function's scope
            let mut ctx_borrow = self.ctx.borrow_mut();

            let func_ret_type = ctx_borrow.root_scope().lookup(&func_decl.name).map(|func_sym| func_sym.lit_type);

            if func_ret_type.is_none() {
                return Err(
                    SAError::UndefinedSymbol { 
                        sym_name: func_decl.name.clone(), 
                        token: Token::none() 
                    }
                );
            };

            // unwrap the return type
            let func_ret_type: LitTypeVariant = func_ret_type.unwrap();

            ctx_borrow.enter_scope(func_decl.scope_id);
            
            // drop context borrow
            drop(ctx_borrow);

            if let Some(func_body) = &mut node.left {
                self.analyze_node(func_body)?;
            }

            // exit function's scope
            self.ctx.borrow_mut().exit_scope();

            return Ok(func_ret_type);
        }
        panic!("Not a function declaration statement!");
    }

    fn analyze_var_decl_stmt(&mut self, node: &mut AST) -> SAResult {
        if let Some(Stmt::VarDecl(ref var_decl)) = node.kind.as_stmt() {
            let var_value_type: LitTypeVariant = self.analyze_expr(node.left.as_mut().unwrap())?;

            let mut ctx_borrow = self.ctx.borrow_mut();

            if let Some(var_sym) = ctx_borrow.deep_lookup_mut(&var_decl.sym_name) {
                return TypeChecker::type_check_var_decl_stmt(var_sym, var_value_type);
            }
            return Err(
                SAError::UndefinedSymbol { 
                    sym_name: var_decl.sym_name.clone(), 
                    token: Token::none() 
                }
            );
        }

        panic!("Not a var declaration statement");
    }

    fn analyze_if_stmt(&mut self, node: &mut AST) -> SAResult {
        if !node.kind.is_stmt() {
            panic!("Needed an Expr--but found {:#?}", node);
        }

        if let ASTKind::StmtAST(Stmt::If(if_stmt)) = &node.kind {
            self.ctx.borrow_mut().enter_scope(if_stmt.scope_id);

            // every 'if' has an expression attached with it in its
            // left branch
            let cond_res: LitTypeVariant = self.analyze_expr(node.left.as_mut().unwrap())?;
            if cond_res != LitTypeVariant::I32 {
                return Err(
                    SAError::TypeError(
                        SATypeError::TypeMismatch { 
                            expected: LitTypeVariant::I32, 
                            found: cond_res
                        }
                    )
                );
            }

            if let Some(if_body) = &mut node.mid {
                self.analyze_node(if_body)?;
            }

            if let Some(else_body) = &mut node.right {
                self.analyze_node(else_body)?;
            }

            self.ctx.borrow_mut().exit_scope();
            return Ok(cond_res);
        }
        Ok(LitTypeVariant::None)
    }
}