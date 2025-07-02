use std::{
    cell::RefCell, 
    rc::Rc
};
use kagc_ast::*;
use kagc_ctx::CompilerCtx;
use kagc_symbol::{function::*, *};
use kagc_token::Token;
use kagc_types::LitTypeVariant;

use crate::errors::*;

pub struct Resolver {
    pub ctx: Rc<RefCell<CompilerCtx>>,
    pub local_offset: usize
}

/// Ok(usize) = Symbol's symbol table position
pub type ResolverResult = Result<usize, SAError>;

impl Resolver {
    pub fn new(ctx: Rc<RefCell<CompilerCtx>>) -> Self {
        Self {
            ctx,
            local_offset: 0
        }
    }

    pub fn resolve(&mut self, nodes: &mut Vec<AST>) {
        for node in nodes {
            if let Err(e) = self.declare_symbol(node) {
                e.dump();
            }
        }
    }

    fn declare_symbol(&mut self, node: &mut AST) -> ResolverResult  {
        match node.operation {
            ASTOperation::AST_FUNCTION => self.declare_func(node),

            // ASTOperation::AST_VAR_DECL => self.declare_let_binding(node),

            _ => Ok(0)
        }
    }

    fn declare_func(&mut self, node: &mut AST) -> ResolverResult {
        if !node.kind.is_stmt() {
            panic!("Needed a FuncDecl--but found {:#?}", node);
        }

        if let Some(Stmt::FuncDecl(func_decl)) = &mut node.kind.as_stmt_mut() {
            // switch to function's scope
            let mut ctx_borrow = self.ctx.borrow_mut();
            let _ = ctx_borrow.enter_scope(func_decl.scope_id);

            let function_id: Option<usize> = {
                let sym = Symbol::new(
                    func_decl.name.clone(),
                    LitTypeVariant::from(func_decl.return_type),
                    SymbolType::Function,
                    func_decl.storage_class,
                );
                let insert_pos: Option<usize> = ctx_borrow.root_scope_mut().declare(sym);
                insert_pos
            };

            // ^^^^ scope.declare returns None if the symbol cannot be added indicating re-definition of the symbol
            if function_id.is_none() {
                return Err(
                    SAError::SymbolAlreadyDefined { 
                        sym_name: func_decl.name.clone(), 
                        token: Token::none() 
                    }
                );
            };

            let function_id = function_id.unwrap();

            // making sure the subsequent users of this FuncDeclStmt AST know 
            // what the function ID is
            func_decl.func_id = function_id;

            let func_info: FunctionInfo = FunctionInfo::new(
                func_decl.name.clone(),
                function_id,
                func_decl.stack_off as i32,
                LitTypeVariant::from(func_decl.return_type),
                func_decl.storage_class,
                func_decl.locals.clone(),
                func_decl.func_param_types.clone()
            );

            // create a new FunctionInfo
            ctx_borrow.func_table.add(func_info);
            // drop context borrow
            drop(ctx_borrow);

            if let Some(func_body) = &mut node.left {
                let _ = self.declare_symbol(func_body)?;
            }

            // exit function's scope
            self.ctx.borrow_mut().exit_scope();
            return Ok(function_id);
        }
        panic!("Not a function declaration statement!");
    }

    fn declare_let_binding(&mut self, node: &AST) -> ResolverResult {
        let mut ctx_borrow = self.ctx.borrow_mut();
        if let ASTKind::StmtAST(Stmt::VarDecl(stmt)) = &node.kind {
            let sym = Symbol::create(
                stmt.sym_name.clone(),
                LitTypeVariant::from(stmt.type_id),
                stmt.symbol_type.clone(),
                0,
                stmt.class,
                stmt.local_offset.try_into().unwrap(),
                None,
                stmt.func_id
            );
            if let Some(id) = ctx_borrow.declare(sym) {
                return Ok(id);
            }
            else {
                panic!("Cannot create symbol!");
            }
        }
        panic!("Cannot create symbol!");
    }
}