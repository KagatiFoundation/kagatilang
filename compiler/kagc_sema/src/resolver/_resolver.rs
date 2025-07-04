use std::{
    cell::RefCell, 
    rc::Rc
};
use kagc_ast::*;
use kagc_ctx::CompilerCtx;
use kagc_symbol::{function::*, *};
use kagc_token::Token;
use kagc_types::{
    record::{
        RecordFieldType, 
        RecordType
    }, 
    LitTypeVariant
};

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

            ASTOperation::AST_VAR_DECL => self.declare_let_binding(node),
            
            ASTOperation::AST_RECORD_DECL => self.declare_record(node),

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

    fn declare_let_binding(&mut self, node: &mut AST) -> ResolverResult {
        let mut ctx_borrow = self.ctx.borrow_mut();
        let value_node = node.left.as_ref().unwrap().clone();

        let stmt = match &node.kind {
            ASTKind::StmtAST(Stmt::VarDecl(stmt)) => stmt,
            _ => panic!("Invalid node"),
        };

        // If it's a record type, validate fields
        if let SymbolType::Record { name: rec_name } = &stmt.symbol_type {
            let rec = ctx_borrow.lookup_record(rec_name).ok_or_else(|| {
                SAError::UndefinedRecord {
                    record_name: rec_name.clone(),
                }
            })?;

            if let ASTKind::ExprAST(Expr::RecordCreation(rec_create)) = &value_node.kind {
                for field in &rec_create.fields {
                    let found = rec.fields.iter().find(|ac_field| ac_field.name == field.name);
                    if found.is_none() {
                        return Err(SAError::UnknownRecordField {
                            field_name: field.name.clone(),
                            record_name: rec_name.clone(),
                        });
                    }
                }
            }
        }

        let sym = Symbol::create(
            stmt.sym_name.clone(),
            LitTypeVariant::from(stmt.value_type),
            stmt.symbol_type.clone(),
            0,
            stmt.class,
            stmt.local_offset.try_into().unwrap(),
            None,
            stmt.func_id,
        );

        let id = ctx_borrow.declare(sym).ok_or({
            SAError::Internal(SAInternalError::SymbolDeclarationFailed)
        })?;

        Ok(id)
    }

    fn declare_record(&mut self, node: &mut AST) -> ResolverResult {
        if let ASTKind::StmtAST(Stmt::Record(stmt)) = &node.kind {
            let record_entry = RecordType {
                name: stmt.name.clone(),
                size: 0,
                __alignment: 0,
                fields: stmt.fields.iter().enumerate().map(|(idx, field)| {
                    RecordFieldType {
                        name: field.name.clone(),
                        typ: field.typ,
                        rel_stack_off: idx
                    }
                }).collect::<Vec<RecordFieldType>>()
            };
            self.ctx.borrow_mut().create_record(record_entry);
            return Ok(0);
        }
        panic!("Cannot create record!!!")
    }
}