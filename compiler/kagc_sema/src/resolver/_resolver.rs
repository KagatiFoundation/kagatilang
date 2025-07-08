use std::{
    cell::RefCell, 
    collections::BTreeMap, 
    rc::Rc
};
use kagc_ast::*;
use kagc_const::pool::{
    KagcConst, 
    PoolIdx, 
    RecordConst
};
use kagc_ctx::CompilerCtx;
use kagc_symbol::{
    function::*, 
    registery::Registry, 
    *
};
use kagc_token::Token;
use kagc_types::{
    record::{
        RecordFieldType, 
        RecordType
    }, 
    LitType, 
    LitTypeVariant
};

use crate::errors::*;

pub struct Resolver {
    pub ctx: Rc<RefCell<CompilerCtx>>,
    pub local_offset: usize,
    curr_func_id: Option<usize>
}

/// Ok(usize) = Symbol's symbol table position
pub type ResolverResult = Result<usize, SAError>;

impl Resolver {
    pub fn new(ctx: Rc<RefCell<CompilerCtx>>) -> Self {
        Self {
            ctx,
            local_offset: 0,
            curr_func_id: None
        }
    }

    pub fn resolve(&mut self, nodes: &mut Vec<AST>) {
        let mut errors = vec![];
        for node in nodes {
            if let Err(e) = self.declare_symbol(node) {
                errors.push(e);
            }
        }

        errors.iter().for_each(|err| err.dump());
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
            self.curr_func_id = Some(function_id);

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
            ctx_borrow.func_table.declare(func_info);
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
        let stmt = match &mut node.kind {
            ASTKind::StmtAST(Stmt::VarDecl(stmt)) => stmt,
            _ => panic!("Invalid node"),
        };

        // If it's a record type, validate fields
        if let SymbolType::Record { name: rec_name } = &stmt.symbol_type {
            let value_node = node.left.as_ref().unwrap().clone();
            _ = self.validate_record_let_binding(rec_name, &value_node)?;
        }

        if let Some(left) = &mut node.left {
            let _ = self.validate_and_process_expr(left, &stmt.sym_name)?;
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

        let id = self.ctx.borrow_mut().declare(sym).ok_or({
            SAError::SymbolAlreadyDefined { sym_name: stmt.sym_name.clone(), token: Token::none() }
        })?;

        Ok(id)
    }

    fn validate_and_process_expr(&mut self, ast: &mut AST, symbol_name: &str) -> ResolverResult {
        if !ast.kind.is_expr() {
            panic!("Needed an Expr--but found {:#?}", ast);
        }

        if let ASTKind::ExprAST(expr) = &mut ast.kind {
            return self.resolve_literal_constant(expr, false, symbol_name);
        }

        panic!()
    }

    fn resolve_literal_constant(&mut self, expr: &mut Expr, parent_is_record: bool, symbol_name: &str) -> ResolverResult {
        if let Expr::LitVal(lit_expr) = expr {
            match lit_expr.result_type {
                LitTypeVariant::RawStr => {
                    let raw_value = if let LitType::RawStr(ref s) = lit_expr.value {
                        s.clone()
                    } else {
                        panic!("Expected RawStr variant but found something else");
                    };

                    let pool_idx = self.ctx.borrow_mut().const_pool.insert(
                        KagcConst::Str(raw_value), 
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
                        LitType::I32(value) => {
                            lit_expr.result_type = LitTypeVariant::I32;
                            value
                        }
                        LitType::U8(value) => {
                            lit_expr.result_type = LitTypeVariant::U8;
                            value as i32
                        }
                        _ => panic!("Expected RawStr variant but found something else")
                    };

                    let pool_idx = self.ctx.borrow_mut().const_pool.insert(
                        KagcConst::Int(raw_value as i64), 
                        self.curr_func_id
                    );

                    return Ok(pool_idx);
                },
                _ => return Ok(0)
            }
        }
        else if let Expr::RecordCreation(rec_create) = expr {
            let record_const = self.build_record_const(rec_create, symbol_name)?;
            
            if parent_is_record {
                // keep the indices of this record const in its parent
            }

            if !parent_is_record {
                let record_idx = self.ctx.borrow_mut().const_pool.insert(
                    KagcConst::Record(record_const),
                    self.curr_func_id,
                );
                return Ok(record_idx);
            } else {
                return Ok(0xFFFFFFFF);
            }
        }
        panic!()
    }

    fn build_record_const(&mut self, rec_create: &mut RecordCreationExpr, symbol_name: &str) -> Result<RecordConst, SAError> {
        let mut indices = vec![];
        for field in &mut rec_create.fields {
            let pool_idx = self.resolve_literal_constant(&mut field.value, true, symbol_name)?;
            indices.push((field.name.clone(), pool_idx));
        }

        Ok(RecordConst {
            type_name: rec_create.name.clone(),
            alias: symbol_name.to_string(),
            fields: indices.into_iter().collect::<BTreeMap<String, PoolIdx>>(),
        })
    }

    fn validate_record_let_binding(&mut self, rec_name: &str, value_node: &AST) -> ResolverResult {
        let ctx_borrow = self.ctx.borrow_mut();
        let rec = ctx_borrow.lookup_record(rec_name).ok_or_else(|| {
            SAError::RecordError(
                SARecordError::UndefinedRecord {
                    record_name: rec_name.to_string(),
                }
            )
        })?;

        if let ASTKind::ExprAST(Expr::RecordCreation(rec_create)) = &value_node.kind {
            for field in &rec_create.fields {
                let found = rec.fields.iter().find(|ac_field| ac_field.name == field.name);
                if found.is_none() {
                    return Err(SAError::RecordError(
                        SARecordError::UnknownRecordField {
                            field_name: field.name.clone(),
                            record_name: rec_name.to_string(),
                        }
                    ));
                }
            }

            for field in &rec.fields {
                let found = rec_create.fields.iter().find(|ac_field| ac_field.name == field.name);
                if found.is_none() {
                    return Err(SAError::RecordError(
                        SARecordError::MissingRecordField {
                            field_name: field.name.clone(),
                            record_name: rec_name.to_string(),
                        }
                    ));
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
                __alignment: 0,
                fields: stmt.fields.iter().enumerate().map(|(idx, field)| {
                    RecordFieldType {
                        name: field.name.clone(),
                        typ: field.typ,
                        rel_stack_off: idx
                    }
                }).collect::<Vec<RecordFieldType>>()
            };
            if self.ctx.borrow_mut().create_record(record_entry).is_none() {
                return Err(
                    SAError::RecordError(
                        SARecordError::DuplicateRecord { 
                            record_name: stmt.name.clone() 
                        }
                    )
                );
            }
            return Ok(0);
        }
        panic!("Cannot create record!!!")
    }
}