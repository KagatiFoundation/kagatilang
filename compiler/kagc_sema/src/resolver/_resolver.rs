use std::{
    cell::RefCell, 
    rc::Rc
};
use indexmap::IndexMap;
use kagc_ast::*;
use kagc_const::pool::{
    KagcConst, OrderedMap, RecordConst
};
use kagc_ctx::CompilerCtx;
use kagc_symbol::{
    function::*, 
    *
};
use kagc_token::Token;
use kagc_types::{
    builtins::obj::KObjType, record::{
        RecordFieldType, 
        RecordType
    }, LitType, LitTypeVariant
};
use kagc_utils::bug;

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
                    return self.resolve_literal_constant(func_call, false, "");
                }
                Ok(0xFFFFFFFF)
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
            self.ctx.borrow_mut().scope.enter_scope(if_stmt.scope_id);
            self.ctx.borrow_mut().scope.exit_scope();
        }
        else {
            bug!("Provided Stmt {:#?} is not of type IfStmt! Aborting...", node);
        }

        if let Some(mid_tree) = &mut node.mid {
            self.declare_symbol(mid_tree)?; // mid tree is the 'if' block
            self.ctx.borrow_mut().scope.exit_scope(); // exit if-block's scope
        }

        if let Some(right_tree) = &mut node.right {
            // right tree is the 'else' block
            if let Some(else_block_tree) = &mut right_tree.left {
                if let ASTKind::StmtAST(Stmt::Scoping(scoping_stmt)) = &else_block_tree.kind {
                    self.ctx.borrow_mut().scope.enter_scope(scoping_stmt.scope_id);
                }
                self.declare_symbol(else_block_tree)?;
                self.ctx.borrow_mut().scope.exit_scope();
            }
        }
        Ok(0)
    }

    fn declare_func(&mut self, node: &mut AST) -> ResolverResult {
        if !node.kind.is_stmt() {
            bug!("Needed a FuncDecl--but found {:#?}", node);
        }

        if let Some(Stmt::FuncDecl(func_decl)) = &mut node.kind.as_stmt_mut() {
            // switch to function's scope
            let mut ctx_borrow = self.ctx.borrow_mut();
            let _ = ctx_borrow.scope.enter_scope(func_decl.scope_id);

            let function_id: Option<usize> = {
                let sym = Symbol::new(
                    func_decl.name.clone(),
                    func_decl.return_type.clone(),
                    SymbolType::Function,
                    func_decl.storage_class,
                );
                let insert_pos: Option<usize> = ctx_borrow
                    .scope
                    .root_scope_mut()
                    .declare(sym);
                insert_pos
            };

            // ^^^^ scope.declare() returns None if the symbol cannot be added indicating re-definition of the symbol
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
                func_decl.return_type.clone(),
                func_decl.storage_class,
                func_decl.locals.clone(),
                func_decl.func_param_types.clone()
            );

            // create a new FunctionInfo
            ctx_borrow.scope.declare_fn(func_info);

            // drop context borrow
            drop(ctx_borrow);

            // loop through function's body to find new symbols
            if let Some(func_body) = &mut node.left {
                let _ = self.declare_symbol(func_body)?;
            }

            // exit the function's scope
            self.ctx.borrow_mut().scope.exit_scope();
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

        stmt.func_id = self.ctx.borrow_mut().scope.current_fn();

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

        let id = self.ctx
            .borrow_mut()
            .scope
            .declare(sym)
            .ok_or({
                SAError::SymbolAlreadyDefined { 
                    sym_name: stmt.sym_name.clone(), 
                    token: Token::none() 
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
            let record_const = self.build_record_const(rec_create, symbol_name)?;
            
            if parent_is_record {
                // keep the indices of this record const in its parent
            }

            if !parent_is_record {
                let record_idx = self.ctx.borrow_mut().const_pool.insert(
                    KagcConst::Record(record_const),
                    KObjType::KRec,
                    self.curr_func_id,
                );
                rec_create.pool_idx = record_idx;
                return Ok(record_idx);
            } else {
                return Ok(0xFFFFFFFF);
            }
        }
        else if let Expr::FuncCall(func_call) = expr {
            for arg in &mut func_call.args {
                let _ = self.resolve_literal_constant(&mut arg.1, false, "")?;
            }
            return Ok(0xFFFFFFFF);
        }
        else if let Expr::Binary(bin) = expr {
            let _ = self.resolve_literal_constant(&mut bin.left, parent_is_record, symbol_name)?;
            let _ = self.resolve_literal_constant(&mut bin.right, parent_is_record, symbol_name)?;
            return Ok(0xFFFFFFFF);
        }
        Ok(0xFFFFFFFF)
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
            fields: OrderedMap(IndexMap::from_iter(indices)),
            alignment: 3 // on Mac M1 silicon chip
        })
    }

    fn validate_record_let_binding(&mut self, rec_name: &str, value_node: &AST) -> ResolverResult {
        let ctx_borrow = self.ctx.borrow_mut();
        let rec = ctx_borrow.scope.lookup_record(rec_name).ok_or_else(|| {
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
                __alignment: stmt.alignment,
                fields: stmt.fields.iter().enumerate().map(|(idx, field)| {
                    RecordFieldType {
                        name: field.name.clone(),
                        typ: field.typ.clone(),
                        rel_stack_off: idx
                    }
                }).collect::<Vec<RecordFieldType>>()
            };
            if self.ctx.borrow_mut().scope.create_record(record_entry).is_none() {
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
        bug!("Cannot create record!!!")
    }
}