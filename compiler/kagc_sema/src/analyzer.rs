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
use kagc_token::Token;
use kagc_types::LitTypeVariant;

use crate::{
    errors::*, 
    type_checker::TypeChecker, 
    typedefs::SAResult
};

#[allow(non_camel_case_types)]
enum _InternalSAErrType {
    __ISET_UndefinedSymbol__ {
        name: String // name of the symbol
    },
    __ISET__NonCallable {
        name: String
    }
}

pub struct SemanticAnalyzer<'sa> {
    pub ctx: Rc<RefCell<CompilerCtx<'sa>>>,
}

impl<'sa> SemanticAnalyzer<'sa> {
    pub fn new(ctx: Rc<RefCell<CompilerCtx<'sa>>>) -> Self {
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

        if let Some(func_sym_pos) = ctx_borrow.sym_table.find_symbol(&func_call.symbol_name) {
            if let Some(func_sym) = ctx_borrow.sym_table.get_symbol(func_sym_pos) {

                if !TypeChecker::is_callable(func_sym) {
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

            Expr::Ident(identexpr) => self.analyze_ident_expr(identexpr),

            Expr::FuncCall(funccallexpr) => self.analyze_func_call_expr(funccallexpr),

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

        if let Ok(sym) = ctx_borrow.find_sym(&ident_expr.sym_name) {
            ident_expr.result_type = sym.lit_type;
        }

        Err(
            SAError::UndefinedSymbol { 
                sym_name: ident_expr.sym_name.clone(), 
                token: Token::none() 
            }
        )
    }

    /// Analyze the function call expression
    /// 
    /// This analysis is done to check if the arguments to this 
    /// function call are valid.
    fn analyze_func_call_expr(&self, func_call: &mut FuncCallExpr) -> SAResult {
        let ctx_borrow = self.ctx.borrow();

        if let Some(func_sym_pos) = ctx_borrow.sym_table.find_symbol(&func_call.symbol_name) {
            if let Some(func_sym) = ctx_borrow.sym_table.get_symbol(func_sym_pos) {

                if !TypeChecker::is_callable(func_sym) {
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

    /// There's nothing to be done here, actually. We don't care 
    /// what type of literal value we get, every literal value is 
    /// okay. The using expression might restraint from using it.
    fn analyze_lit_expr(&self, expr: &mut LitValExpr) -> SAResult {
        Ok(expr.result_type)
    }

    fn analyze_return_stmt(&mut self, node: &mut AST) -> SAResult {
        if !node.kind.is_stmt() {
            panic!("Needed an ReturnStmt--but found {:#?}", node);
        }

        if let Some(Stmt::Return(_)) = &mut node.kind.as_stmt() {
            let ctx_borrow = self.ctx.borrow();
            
            // 'return' statements can appear only inside the functions
            // thus, the current function cannot be None; it's an error otherwise
            let expected_fn_ret_type: LitTypeVariant = ctx_borrow.get_curr_func().unwrap().return_type;

            drop(ctx_borrow);
            
            // if return statement returns some value
            let found_fn_ret_type: LitTypeVariant = if let Some(return_expr) = &mut node.left {
                self.analyze_expr(return_expr)?
            }
            else {
                LitTypeVariant::None
            };

            if expected_fn_ret_type.is_void() && found_fn_ret_type != LitTypeVariant::None {
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

            let return_type_mismatch: bool = 
                !expected_fn_ret_type.is_void() && found_fn_ret_type.is_none() ||
                ((expected_fn_ret_type != found_fn_ret_type) && !TypeChecker::is_type_coalesciable(found_fn_ret_type, expected_fn_ret_type));

            if return_type_mismatch {
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

        panic!();
    }

    fn analyze_func_decl_stmt(&mut self, node: &mut AST) -> SAResult {
        let func_id: usize = match &node.kind {
            ASTKind::StmtAST(stmt_ast) => {
                match stmt_ast {
                    Stmt::FuncDecl(func_decl_stmt) => {
                        func_decl_stmt.func_id
                    },

                    _ => panic!("not a function declaration statement")
                }
            },

            _ => panic!("not a statement")
        };

        let mut ctx_borrow = self.ctx.borrow_mut();

        let func_name: String = ctx_borrow.sym_table.get_symbol(func_id).unwrap().name.clone();

        if let Some(_finfo) = ctx_borrow.func_table.get(&func_name) {
            ctx_borrow.switch_to_func_scope(func_id);
        } 
        else {
            panic!("Function '{}' not found", func_name);
        }

        drop(ctx_borrow);

        if let Some(func_body) = &mut node.left {
            return self.analyze_node(func_body);
        }

        self.ctx.borrow_mut().switch_to_global_scope();

        Ok(LitTypeVariant::None)
    }

    fn analyze_var_decl_stmt(&mut self, node: &mut AST) -> SAResult {
        if let Some(Stmt::VarDecl(ref var_decl)) = node.kind.as_stmt() {
            let var_value_type: LitTypeVariant = self.analyze_expr(node.left.as_mut().unwrap())?;

            let mut ctx_borrow = self.ctx.borrow_mut();

            if let Ok(var_sym) = ctx_borrow.find_sym_mut(&var_decl.sym_name) {
                return TypeChecker::type_check_var_decl_stmt(var_sym, var_value_type);
            }
            else {
                return Err(
                    SAError::UndefinedSymbol { 
                        sym_name: var_decl.sym_name.clone(), 
                        token: Token::none() 
                    }
                );
            }
        }

        panic!("Not a var declaration statement");
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use kagc_ast::{ASTKind, ASTOperation, BinExpr, Expr, LitValExpr, Stmt, VarDeclStmt, AST};
    use kagc_ctx::CompilerCtx;
    use kagc_symbol::{FunctionInfoTable, StorageClass, Symbol, SymbolType, Symtable};
    use kagc_types::{LitType, LitTypeVariant};

    use crate::{errors::{SAError, SATypeError}, typedefs::SAResult};

    use super::SemanticAnalyzer;

    fn create_symt(syms: Vec<Symbol>) -> Symtable<Symbol> {
        let mut symt = Symtable::<Symbol>::new();
        for sym in syms {
            symt.add_symbol(sym);
        }
        symt
    }

    fn create_funt() -> FunctionInfoTable {
        FunctionInfoTable::new()
    }

    fn create_ctx<'ctx>(symt: &'ctx mut Symtable<Symbol>, func_table: &'ctx mut FunctionInfoTable) -> CompilerCtx<'ctx> {
        CompilerCtx::new(symt, func_table)
    }

    fn create_i32_var_decl_ast(pos: usize, name: String) -> AST {
        AST::new(
            ASTKind::StmtAST(Stmt::VarDecl(
                VarDeclStmt {
                    class: StorageClass::LOCAL,
                    symtbl_pos: pos,
                    sym_name: name
                }
            )), 
            ASTOperation::AST_VAR_DECL, 
            Some(AST::create_leaf(
                ASTKind::ExprAST(
                    Expr::LitVal(LitValExpr { 
                        value: LitType::I32(123), 
                        result_type: LitTypeVariant::I32 
                    })
                ), 
                ASTOperation::AST_INTLIT, 
                LitTypeVariant::I32, 
                None, 
                None
            )
            ), 
            None, 
            LitTypeVariant::I32
        )
    }

    fn create_i32_var_decl_ast_with_bin_expr(pos: usize, name: String) -> AST {
        AST::new(
            ASTKind::StmtAST(Stmt::VarDecl(
                VarDeclStmt {
                    class: StorageClass::LOCAL,
                    symtbl_pos: pos,
                    sym_name: name
                }
            )), 
            ASTOperation::AST_VAR_DECL, 
            Some(AST::create_leaf(
                ASTKind::ExprAST(
                    Expr::Binary(BinExpr {
                        operation: ASTOperation::AST_ADD,
                        left: Box::new(
                            Expr::LitVal(LitValExpr { 
                                value: LitType::I32(123), 
                                result_type: LitTypeVariant::I32 
                            })
                        ),
                        right: Box::new(
                            Expr::LitVal(
                                LitValExpr {
                                    value: LitType::Str("bichara".to_string(), 7),
                                    result_type: LitTypeVariant::Str
                                }
                            )
                        ),
                        result_type: LitTypeVariant::I32
                    })
                    
                ), 
                ASTOperation::AST_INTLIT, 
                LitTypeVariant::I32, 
                None, 
                None
            )
            ), 
            None, 
            LitTypeVariant::I32
        )
    }

    fn create_i32_symbol(name: String) -> Symbol {
        Symbol::new(name, LitTypeVariant::I32, SymbolType::Variable, StorageClass::LOCAL)
    }

    fn create_symbol_without_explicit_type(name: String) -> Symbol {
        Symbol::new(name, LitTypeVariant::None, SymbolType::Variable, StorageClass::LOCAL)
    }

    #[test]
    fn test_no_type_var_decl_stmt_analysis() {
        let symt: &mut Symtable<Symbol> = &mut create_symt(
            vec![
                create_symbol_without_explicit_type("number".to_string()),
            ]
        );
        let funct: &mut FunctionInfoTable = &mut create_funt();
        let ctx: Rc<RefCell<CompilerCtx<'_>>> = Rc::new(RefCell::new(create_ctx(symt, funct)));

        let mut a_analyzer: SemanticAnalyzer<'_> = SemanticAnalyzer::new(Rc::clone(&ctx));

        let mut no_type_var_ast: AST = create_i32_var_decl_ast(0, "number".to_string());
        let ar2: SAResult = a_analyzer.analyze_var_decl_stmt(&mut no_type_var_ast);
        assert!(ar2.is_ok());

        // type has to be updated of non-type symbol
        assert_eq!(symt.get_symbol(0).unwrap().lit_type, LitTypeVariant::I32);
    }

    #[test]
    fn test_bin_expr_var_decl_stmt_analysis() {
        let symt: &mut Symtable<Symbol> = &mut create_symt(
            vec![
                create_symbol_without_explicit_type("number".to_string()),
            ]
        );
        let funct: &mut FunctionInfoTable = &mut create_funt();
        let ctx: Rc<RefCell<CompilerCtx<'_>>> = Rc::new(RefCell::new(create_ctx(symt, funct)));

        let mut a_analyzer: SemanticAnalyzer<'_> = SemanticAnalyzer::new(Rc::clone(&ctx));

        let mut no_type_var_ast: AST = create_i32_var_decl_ast_with_bin_expr(0, "number".to_string());
        let ar2: SAResult = a_analyzer.analyze_var_decl_stmt(&mut no_type_var_ast);
        assert!(ar2.is_err());
        matches!(
            ar2, 
            Err(
                SAError::TypeError(
                    SATypeError::IncompatibleTypes { 
                        a: LitTypeVariant::I32, 
                        b: LitTypeVariant::Str, 
                        operation: ASTOperation::AST_ADD
                    }
                )
            )
        );

        // type has to be updated of non-type symbol
        // assert_eq!(symt.get_symbol(0).unwrap().lit_type, LitTypeVariant::I32);
    }

    #[test]
    fn test_var_decl_stmt_analysis() {
        let symt: &mut Symtable<Symbol> = &mut create_symt(
            vec![
                create_i32_symbol("number".to_string()),
            ]
        );
        let funct: &mut FunctionInfoTable = &mut create_funt();
        let ctx: Rc<RefCell<CompilerCtx<'_>>> = Rc::new(RefCell::new(create_ctx(symt, funct)));

        let mut var_ast: AST = create_i32_var_decl_ast(0, "number".to_string());

        let mut a_analyzer: SemanticAnalyzer<'_> = SemanticAnalyzer::new(Rc::clone(&ctx));

        let analysis_res: SAResult = a_analyzer.analyze_var_decl_stmt(&mut var_ast);
        assert!(analysis_res.is_ok());
    }
}