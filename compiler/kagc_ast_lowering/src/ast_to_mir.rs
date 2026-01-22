// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use core::panic;
use std::vec;

use kagc_ast::*;
use kagc_symbol::*;
use kagc_backend::reg::*;
use kagc_types::*;
use kagc_const::pool::{ConstPool, PoolIdx};
use kagc_scope::ctx::ScopeCtx;
use kagc_scope::scope::ScopeId;

use kagc_mir::value::{IRValue, IRValueId};
use kagc_mir::block::{BlockId, Terminator, INVALID_BLOCK_ID};
use kagc_mir::instruction::{IRAddress, IRCondition, IRInstruction, StackSlotId};
use kagc_mir::mir_builder::MirBuilder;
use kagc_mir::function::FunctionParam;
use kagc_mir::types::IRType;
use kagc_mir::builtin::BuiltinFn;
use kagc_errors::diagnostic::Diagnostic;
use kagc_symbol::function::{Func, FuncId};
use kagc_utils::bug;

use crate::fn_ctx::FunctionContext;
use crate::loop_ctx::LoopContext;

/// Expression lowering result
type ExprLoweringResult = Result<IRValueId, Diagnostic>;

/// Statement lowering result
type StmtLoweringResult = Result<BlockId, Diagnostic>;

/// `AstToMirLowerer` is responsible for transforming the Abstract 
/// Syntax Tree (AST) into the Mid-Level Intermediate Representation (MIR).
pub struct AstToMirLowerer<'a, 'tcx> {
    /// A reference-counted, mutable reference to the global 
    /// compiler context. Holds shared state like symbol tables, 
    /// type info, and other data needed during IR lowering.
    scope: &'tcx ScopeCtx<'tcx>,
    const_pool: &'a ConstPool,

    /// Public MIR builder used to generate MIR instructions, manage
    /// basic blocks, and keep track of the current insertion point.
    pub ir_builder: MirBuilder,

    /// Current function that is being parsed
    current_function: Option<Func<'tcx>>,
}

impl<'a, 'tcx> AstToMirLowerer<'a, 'tcx> {
    pub fn new(scope: &'tcx ScopeCtx<'tcx>, const_pool: &'a ConstPool) -> Self {
        Self {
            scope,
            const_pool,
            current_function: None,
            ir_builder: MirBuilder::default()
        }
    }

    pub fn lower(&mut self, nodes: &mut [AstNode]) -> StmtLoweringResult {
        for node in nodes {
            self.lower_node(node, &mut FunctionContext::new())?;
        }
        Ok(self.ir_builder.current_block_id_unchecked())
    }

    pub fn lower_node(&mut self, node: &mut AstNode, fn_ctx: &mut FunctionContext) -> StmtLoweringResult {
        if node.op == AstOp::Func {
            return self.lower_function(node);
        }
        else if node.op == AstOp::FuncCall {
            return self.lower_function_call(node, fn_ctx);
        }
        else if node.op == AstOp::VarDecl {
            return self.lower_variable_declaration(node, fn_ctx);
        }
        else if node.op == AstOp::Return {
            return self.lower_return(node, fn_ctx);
        }
        else if node.op == AstOp::Import {
            return self.lower_import();
        }
        else if node.op == AstOp::RecDecl {
            return self.lower_record_declaration(node);
        }
        else if node.op == AstOp::Loop {
            return self.lower_infinite_loop(node, fn_ctx);
        }
        else if node.op == AstOp::If {
            return self.lower_if_else_tree(node, fn_ctx);
        }
        else if node.op == AstOp::Else {
            return self.lower_else_block(node, fn_ctx);
        }
        else if node.op == AstOp::Block {
            return self.lower_block(node, fn_ctx);
        }
        todo!("{node_type:#?}", node_type = node.op);
    }

    fn lower_block(&mut self, ast: &mut AstNode, fn_ctx: &mut FunctionContext) -> StmtLoweringResult {
        let node_id = ast.id;
        let block_stmt = ast.expect_block_stmt_mut();

        let Some(block_scope) = self.scope.lookup_node_scope(node_id) else {
            bug!("AstNode {:#?}'s scope not found", node_id);
        };

        self.scope.enter(block_scope.id.get());

        let mut statements = block_stmt
            .statements
            .iter_mut()
            .collect::<Vec<&mut AstNode>>();
        
        let current_block_id = self.lower_linear_sequence(&mut statements, fn_ctx)?;

        self.scope.pop();
        Ok(current_block_id)
    }

    fn lower_function(&mut self, ast: &mut AstNode) -> StmtLoweringResult {
        let func_decl = ast.expect_func_decl_stmt(); // make sure its a function declaration statement
        
        let Some(func_scope) = self.scope.lookup_node_scope(ast.id) else {
            bug!("Function {name}'s scope not found", name = func_decl.name);
        };

        let Some(func) = self.scope.lookup_fn_by_name(func_decl.name) else {
            bug!("Function '{name}' not found", name = func_decl.name);
        };

        self.current_function = Some(func.clone());
        let storage_class = func.storage_class;

        self.scope.enter(func_scope.id.get()); // enter the function's scope
        let mut fn_ctx = FunctionContext::new();

        let func_ir_params = self
            .scope
            .collect_params(func_scope.id.get())
            .iter()
            .map(|&sym| {
                let param_stack_slot = fn_ctx.alloc_local_slot();
                fn_ctx.var_offsets.insert(sym.name.to_string(), param_stack_slot.0);
                self.ir_builder.create_function_parameter(
                    IRType::from(sym.ty.get()), 
                    param_stack_slot
                )
            }).collect::<Vec<FunctionParam>>();

        let func_anchor = self.ir_builder.create_function(
            func.name.to_string(),
            func_ir_params,
            IRType::from(ast.ty.unwrap_or_else(|| bug!("fix this man"))),
            storage_class
        );

        if storage_class == StorageClass::EXTERN {
            // create the function but skip body lowering as there is 
            // no body associated with 'extern' functions
            return Ok(INVALID_BLOCK_ID);
        }

        let return_label = func_anchor.exit_block; // single point of return
        fn_ctx.set_return_label(return_label);

        let Some(func_body) = &mut ast.left else {
            bug!("no function body found");
        };

        let func_body_block = func_body.expect_block_stmt_mut();
        let mut statements = func_body_block
            .statements
            .iter_mut()
            .collect::<Vec<&mut AstNode>>();
        let current_block_id = self.lower_linear_sequence(&mut statements, &mut fn_ctx)?;

        if !self.ir_builder.has_terminator(current_block_id) {
            // return from the function
            self.ir_builder.set_terminator(
                current_block_id, 
                Terminator::Return { 
                    value: None, 
                    target: fn_ctx.get_return_label().expect("No return label! Aborting...") 
                }
            );
        }

        self.current_function = None;
        self.scope.pop(); // exit function's scope
        Ok(current_block_id)
    }

    fn lower_function_call(&mut self, node: &mut AstNode, fn_ctx: &mut FunctionContext) -> StmtLoweringResult {
        if let NodeKind::ExprAST(Expr::FuncCall(func_call)) = &mut node.kind {
            let _ = self.lower_function_call_expr(func_call, fn_ctx)?;
            return Ok(self.ir_builder.current_block_id_unchecked());
        }
        panic!()
    }

    fn lower_variable_declaration(&mut self, var_ast: &mut AstNode, fn_ctx: &mut FunctionContext) -> StmtLoweringResult {
        let var_decl = var_ast.kind.as_stmt().unwrap_or_else(|| panic!("Requires a VarDeclStmt"));
        if let Stmt::VarDecl(var_decl) = var_decl {
            if var_ast.left.is_none() {
                bug!("Variable is not assigned a value!");
            }

            fn_ctx.change_parent_ast_kind(AstOp::VarDecl);
            let var_decl_value = self.lower_expression_ast(var_ast.left.as_mut().unwrap(), fn_ctx)?;

            if !var_ast.ty.unwrap().is_gc_alloced() { // unsafe unwrap call
                let var_stack_off = fn_ctx.alloc_local_slot();
                self.ir_builder.inst(
                    IRInstruction::Store { 
                        src: var_decl_value, 
                        address: IRAddress::StackSlot(var_stack_off) 
                    }
                );
                fn_ctx.var_offsets.insert(var_decl.sym_name.to_string(), var_stack_off.0);
            }
            else {
                let var_stack_off = fn_ctx.alloc_local_slot() - StackSlotId(1);
                // Subtract 1 from the slot ID because a heap-allocated variable occupies
                // one stack slot for the object’s base pointer
                fn_ctx.var_offsets.insert(var_decl.sym_name.to_string(), var_stack_off.0);
            }
            return Ok(self.ir_builder.current_block_id_unchecked());
        }
        bug!("required VarDeclStmt--but found {var_ast:#?}");
    }

    fn lower_expression_ast(&mut self, ast: &mut AstNode, fn_ctx: &mut FunctionContext) -> ExprLoweringResult {
        if !ast.kind.is_expr() {
            bug!("needed an Expr--but found {ast:#?}");
        }
        let expr = ast
            .kind
            .as_expr_mut()
            .unwrap_or_else(|| bug!("cannot lower an expression"));
        self.lower_expression(expr, fn_ctx)
    }

    fn lower_expression(&mut self, expr: &mut Expr, fn_ctx: &mut FunctionContext) -> ExprLoweringResult {
        match expr {
            Expr::LitVal(lit_expr) => self.lower_literal_value_expr(lit_expr, fn_ctx),
            Expr::Ident(ident_expr) => self.lower_identifier_expr(ident_expr, fn_ctx),
            Expr::Binary(bin_expr) => self.lower_binary_expr(bin_expr, fn_ctx),
            Expr::FuncCall(func_call_expr) => self.lower_function_call_expr(func_call_expr, fn_ctx),
            Expr::RecordFieldAccess(rec_field_access) => self.lower_record_field_access_expr(rec_field_access, fn_ctx),
            Expr::RecordCreation(rec_create_expr) => self.lower_record_creation_expr(rec_create_expr, fn_ctx),
            _ => unimplemented!()
        }
    }

    fn lower_record_creation_expr(&mut self, rec_create_expr: &mut RecordCreationExpr, fn_ctx: &mut FunctionContext) -> ExprLoweringResult {
        let mut field_stack_slot_ids = Vec::with_capacity(rec_create_expr.fields.len());
        for field in &mut rec_create_expr.fields {
            let field_slot = self.lower_record_field_creation_expr(field, fn_ctx)?;
            field_stack_slot_ids.push(field_slot);
        }

        let rec_stack_slot = self.load_record_from_const_pool(rec_create_expr.pool_idx, fn_ctx)?;
        for (index, field_slot_id) in field_stack_slot_ids.iter().enumerate() {
            let rec_data_off_value_id = self.ir_builder.create_load(IRAddress::StackSlot(rec_stack_slot));
            let field_slot_load_value_id = self.ir_builder.create_load(IRAddress::StackSlot(*field_slot_id));
            let index_value_id = self.ir_builder.create_move(IRValue::Constant(index as i64));
            self.ir_builder.inst(
                IRInstruction::CallBuiltin { 
                    builtin: BuiltinFn::AssignRef, 
                    args: vec![rec_data_off_value_id, field_slot_load_value_id, index_value_id], 
                    result: None 
                }
            );
        }
        Ok(IRValueId(0xFFFFFFFF))
    }

    fn lower_record_field_creation_expr(&mut self, expr: &mut RecordFieldAssignExpr, fn_ctx: &mut FunctionContext) -> Result<StackSlotId, Diagnostic> {
        match expr.value.result_type() {
            TyKind::I64 => {
                let call_result_value_id = self.ir_builder.occupy_value_id();
                let eval_value_id = self.lower_expression(&mut expr.value, fn_ctx)?;
                self.ir_builder.inst(
                    IRInstruction::CallBuiltin { 
                        builtin: BuiltinFn::AllocInt, 
                        args: vec![eval_value_id], 
                        result: Some(call_result_value_id)
                    }
                );
                let stack_slot = fn_ctx.alloc_local_slot();
                self.ir_builder.inst(
                    IRInstruction::Store { 
                        src: eval_value_id, 
                        address: IRAddress::StackSlot(stack_slot) 
                    }
                );
                Ok(stack_slot)
            },
            TyKind::PoolStr => {
                let lit_val_expr = expr.value.as_litval().unwrap_or_else(|| {
                    bug!("type was PoolStr but the expression itself is not of type LitValExpr")
                });
                self.load_str_from_const_pool(lit_val_expr, fn_ctx)
            },
            TyKind::Str => {
                let eval_value_id = self.lower_expression(&mut expr.value, fn_ctx)?;
                let stack_slot = fn_ctx.alloc_local_slot();
                self.ir_builder.inst(
                    IRInstruction::Store { 
                        src: eval_value_id, 
                        address: IRAddress::StackSlot(stack_slot) 
                    }
                );
                Ok(stack_slot)
            }
            _ => unimplemented!("cannot assign {typ:#?} to a record's field", typ = expr.value.result_type())
        }
    }

    fn lower_record_field_access_expr(&mut self, access: &mut RecordFieldAccessExpr, fn_ctx: &mut FunctionContext) -> ExprLoweringResult {
        let rec_stack_off = *fn_ctx
            .var_offsets
            .get(access.rec_alias)
            .unwrap_or_else(|| bug!("record's stack offset not found"));

        let base_pointer_value = self.ir_builder.create_load(
            IRAddress::StackSlot(
                StackSlotId(rec_stack_off)
            )
        );
        let data_pointer_value = self.ir_builder.create_load(
            IRAddress::BaseSlot(
                base_pointer_value, 
                StackSlotId(unsafe { runtime::GC_OFFSET_CHILDREN as usize })
            )
        );
        Ok(self.ir_builder.create_load(
            IRAddress::BaseSlot(
                data_pointer_value, 
                StackSlotId(access.rel_stack_off)
            )
        ))
    }

    // returns the id of value and the value's size
    fn load_str_from_const_pool(&mut self, expr: &LitValExpr, fn_ctx: &mut FunctionContext) -> Result<StackSlotId, Diagnostic> {
        match expr.value {
            Literal::PoolStr(pool_idx) => {
                let const_value = self.ir_builder.occupy_value_id();
                let const_size = self
                    .const_pool
                    .size(pool_idx)
                    .unwrap_or_else(|| bug!("cannot find const entry"));
                self.ir_builder.inst(
                    IRInstruction::LoadConst { 
                        label_id: pool_idx,
                        result: const_value
                    }
                );

                let const_size_value = self.ir_builder.create_move(IRValue::Constant(const_size as i64));
                let call_result_value = self.ir_builder.occupy_value_id();
                self.ir_builder.inst(
                    IRInstruction::CallBuiltin { 
                        builtin: BuiltinFn::AllocStr, 
                        args: vec![const_value, const_size_value],
                        result: Some(call_result_value)
                    }
                );
                let stack_slot = fn_ctx.alloc_local_slot();
                self.ir_builder.inst(
                    IRInstruction::Store { 
                        src: call_result_value, 
                        address: IRAddress::StackSlot(stack_slot) 
                    }
                );
                Ok(stack_slot)
            },
            _ => bug!("expected a PoolStr but found '{:#?}'", expr)
        }
    }

    fn load_record_from_const_pool(&mut self, pool_idx: PoolIdx, fn_ctx: &mut FunctionContext) -> Result<StackSlotId, Diagnostic> {
        let const_value_id = self.ir_builder.occupy_value_id();
        let const_size = self.const_pool.size(pool_idx).unwrap_or_else(|| bug!("cannot find const entry"));
        self.ir_builder.inst(
            IRInstruction::LoadConst { 
                label_id: pool_idx, 
                result: const_value_id
            }
        );
        let const_value_size_id = self.ir_builder.create_move(IRValue::Constant(const_size as i64));
        let call_result_value_id = self.ir_builder.occupy_value_id();
        self.ir_builder.inst(
            IRInstruction::CallBuiltin { 
                builtin: BuiltinFn::AllocRec, 
                args: vec![const_value_id, const_value_size_id],
                result: Some(call_result_value_id)
            }
        );
        let stack_slot = fn_ctx.alloc_local_slot();
        self.ir_builder.inst(
            IRInstruction::Store { 
                src: call_result_value_id, 
                address: IRAddress::StackSlot(stack_slot) 
            }
        );
        Ok(stack_slot)
    }

    fn lower_function_call_expr(&mut self, func_call_expr: &mut FuncCallExpr, fn_ctx: &mut FunctionContext) -> ExprLoweringResult {
        let mut func_call_args = Vec::with_capacity(func_call_expr.args.len());
        for (_, arg_expr) in &mut func_call_expr.args {
            let arg_value_id = self.lower_expression(arg_expr, fn_ctx)?;
            func_call_args.push(arg_value_id);
        }

        if !matches!(func_call_expr.ty, TyKind::None | TyKind::Void) {
            let call_result_value = self.ir_builder.occupy_value_id();
            self.ir_builder.inst(
                IRInstruction::Call { 
                    func: func_call_expr.symbol_name.to_string(), 
                    args: func_call_args,
                    result: Some(call_result_value)
                }
            );
            Ok(call_result_value)
        }
        else {
            self.ir_builder.inst(
                IRInstruction::Call { 
                    func: func_call_expr.symbol_name.to_string(), 
                    args: func_call_args,
                    result: None
                }
            );
            Ok(IRValueId(0xFFFFFFFF))
        }
    }

    fn lower_literal_value_expr(&mut self, lit_expr: &LitValExpr, fn_ctx: &mut FunctionContext) -> ExprLoweringResult {
        if let Literal::PoolStr(_) = &lit_expr.value {
            let str_stack_slot = self.load_str_from_const_pool(lit_expr, fn_ctx)?;
            return Ok(self.ir_builder.create_load(
                IRAddress::StackSlot(str_stack_slot)
            ));
        }
        match lit_expr.ty {
            TyKind::I64 => {
                let const_value = *lit_expr.value.unwrap_i32().expect("No i64 value!");
                Ok(self.ir_builder.create_move(IRValue::Constant(const_value as i64)))
            },
            TyKind::U8 => {
                let const_value = *lit_expr.value.unwrap_u8().expect("No u8 value!") as i64;
                Ok(self.ir_builder.create_move(IRValue::Constant(const_value)))
            },
            _ => unimplemented!("{lit_expr:#?}")
        }
    }

    fn lower_identifier_expr(&mut self, ident_expr: &IdentExpr, fn_ctx: &mut FunctionContext) -> ExprLoweringResult {
        let sym = self.scope.lookup_sym(None, ident_expr.sym_name).unwrap(); // unsafe unwrap call
        let sym_off = *fn_ctx
            .var_offsets
            .get(ident_expr.sym_name)
            .unwrap_or_else(|| bug!("undefined symbol '{name}'", name = ident_expr.sym_name));

        let reg_sz: RegSize = sym.ty.get().to_reg_size();
        assert_ne!(reg_sz, 0);
        let load_value_id = self.ir_builder.create_load(IRAddress::StackSlot(StackSlotId(sym_off)));
        Ok(load_value_id)
    }

    fn lower_binary_expr(&mut self, bin_expr: &mut BinExpr, fn_ctx: &mut FunctionContext) -> ExprLoweringResult {
        let lhs_value_id = self.lower_expression(&mut bin_expr.left, fn_ctx)?;
        let rhs_value_id = self.lower_expression(&mut bin_expr.right, fn_ctx)?;
        match bin_expr.operation {
            AstOp::Add       => Ok(self.ir_builder.create_add(IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            AstOp::Subtract  => Ok(self.ir_builder.create_subtract(IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            AstOp::Multiply  => Ok(self.ir_builder.create_divide(IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            AstOp::Divide    => Ok(self.ir_builder.create_multiply(IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            AstOp::EqEq      => Ok(self.ir_builder.create_conditional_jump(IRCondition::EqEq, IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            AstOp::NEq       => Ok(self.ir_builder.create_conditional_jump(IRCondition::NEq, IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            AstOp::LtEq      => Ok(self.ir_builder.create_conditional_jump(IRCondition::LTEq, IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            AstOp::GtEq      => Ok(self.ir_builder.create_conditional_jump(IRCondition::GTEq, IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            AstOp::LThan     => Ok(self.ir_builder.create_conditional_jump(IRCondition::LThan, IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            AstOp::GThan     => Ok(self.ir_builder.create_conditional_jump(IRCondition::GThan, IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            _ => unimplemented!()
        }
    }

    fn lower_return(&mut self, ret_stmt: &mut AstNode, fn_ctx: &mut FunctionContext) -> StmtLoweringResult {
        if let Some(Stmt::Return(_)) = &ret_stmt.kind.as_stmt() {
            if let Some(curr_fn) = &self.current_function {
                let curr_block = self.ir_builder.current_block_id_unchecked();
                let func_exit_block = fn_ctx
                    .get_return_label()
                    .expect("Function's return block is not set! Aborting...");

                if !curr_fn.ty.is_void() {
                    let return_value_id = self.lower_expression_ast(
                        ret_stmt.left.as_mut().unwrap(), 
                        fn_ctx
                    )?;
                    self.ir_builder.set_terminator(
                        curr_block, 
                        Terminator::Return {
                            target: func_exit_block,
                            value: Some(return_value_id)
                        }
                    );
                }
                else {
                    self.ir_builder.set_terminator(
                        curr_block, 
                        Terminator::Return {
                            target: func_exit_block,
                            value: None
                        }
                    );
                }
                return Ok(curr_block);
            }
            else {
                bug!("'return' outside a function");
            }
        }
        bug!("expected ReturnStmt but found {ret_stmt:#?}");
    }

    fn lower_infinite_loop(&mut self, ast: &mut AstNode, fn_ctx: &mut FunctionContext) -> StmtLoweringResult {
        let prev_block_id = self.ir_builder.current_block_id_unchecked();
        let loop_body_id = self.ir_builder.create_block("loop_body");
        let loop_tail_id = self.ir_builder.create_label();
        fn_ctx.enter_loop(LoopContext { head_block: loop_body_id, exit_block: loop_tail_id });

        self.ir_builder.set_terminator(prev_block_id, Terminator::Jump(loop_body_id));
        self.ir_builder.link_blocks(prev_block_id, loop_body_id);

        let mut linearized_body = ast.left.as_mut().unwrap().linearize_mut();
        let current_block_id = self.lower_linear_sequence(&mut linearized_body, fn_ctx)?;

        if !self.ir_builder.has_terminator(current_block_id) {
            // only if the current block is still "open"
            self.ir_builder.set_terminator(current_block_id, Terminator::Jump(loop_body_id));
            self.ir_builder.link_blocks(current_block_id, loop_body_id);
        }

        fn_ctx.exit_loop();
        Ok(current_block_id)
    }

    fn lower_if_else_tree(&mut self, ast: &mut AstNode, fn_ctx: &mut FunctionContext) -> StmtLoweringResult {
        if let NodeKind::StmtAST(Stmt::If) = &ast.kind {
            self.scope.enter(ScopeId(0));
        }
        let prev_block_id = self.ir_builder.current_block_id_unchecked();

        // Evaluate the condition and store the resilt in a register.
        // Every if-else must have the `left` branch set in the main if-else AST tree.
        let conditional_block = self.ir_builder.create_block("if-header");
        self.ir_builder.set_terminator(prev_block_id, Terminator::Jump(conditional_block));
        self.ir_builder.link_blocks(prev_block_id, conditional_block);
        
        // lower the condition expression in the entry block
        let if_stmt_cond_value = self.lower_expression_ast(ast.left.as_mut().unwrap(), fn_ctx)?;

        // then-branch
        let then_block = self.ir_builder.create_block("then");
        // else-branch 
        let else_block = self.ir_builder.create_block("else");
        self.ir_builder.link_blocks_multiple(conditional_block, vec![then_block, else_block]);

        // join branch
        let merge_block = self.ir_builder.create_block("merge");
        self.ir_builder.link_blocks(then_block, merge_block);
        self.ir_builder.link_blocks(else_block, merge_block);

        self.ir_builder.set_terminator(
            conditional_block, 
            Terminator::CondJump { 
                cond: if_stmt_cond_value, 
                then_block, 
                else_block
            }
        );

        // lower the if-block
        self.ir_builder.switch_to_block(then_block);
        if let Some(mid_tree) = &mut ast.mid {
            let current_block_id = self.lower_linear_sequence(&mut mid_tree.linearize_mut(), fn_ctx)?;
            if !self.ir_builder.has_terminator(current_block_id) {
                self.ir_builder.set_terminator(current_block_id, Terminator::Jump(merge_block));
                self.ir_builder.link_blocks(current_block_id, merge_block);
            }
        }
        // exit if-scope
        self.scope.pop();
        
        if let Some(right_tree) = &mut ast.right {
            // dump 'else' block's code
            self.ir_builder.switch_to_block(else_block);
            // else-block
            let current_block_id = self.lower_linear_sequence(&mut right_tree.linearize_mut(), fn_ctx)?;
            if !self.ir_builder.has_terminator(current_block_id) {
                self.ir_builder.set_terminator(current_block_id, Terminator::Jump(merge_block));
                self.ir_builder.link_blocks(current_block_id, merge_block);
            }
        }

        // switch to the merge block afte emitting if-branch's code
        self.ir_builder.switch_to_block(merge_block);
        Ok(merge_block)
    }

    pub fn lower_linear_sequence(&mut self, stmts: &mut [&mut AstNode], fn_ctx: &mut FunctionContext) -> StmtLoweringResult {
        let mut current = self.ir_builder.current_block_id_unchecked();
        let stmts_len = stmts.len();
        for (idx, stmt) in stmts.iter_mut().enumerate() {
            let lowered = self.lower_node(stmt, fn_ctx)?;
            if idx < stmts_len - 1 {
                current = self.ir_builder.ensure_continuation_block(lowered);
            }
        }
        Ok(current)
    }

    fn lower_else_block(&mut self, ast: &mut AstNode, fn_ctx: &mut FunctionContext) -> StmtLoweringResult {
        if let NodeKind::StmtAST(Stmt::Scoping) = &ast.kind {
            self.scope.enter(ScopeId(0)); // TODO
        }
        else {
            bug!("provided AST tree is not of type 'AST_ELSE'");
        }

        // Current IRBasicBlock is for this 'else' block.
        // `lower_if_else_tree` creates a new block for the 'else' block.
        // We can freely utilize that created block to add new instructions.
        let mut last_block_id = self.ir_builder.current_block_id_unchecked();
        
        // Reference to Parser's `parser_if_stmt` function to learn why the 'left' 
        // branch is used for the 'else' block.
        let else_ast_linearized = ast.left.as_mut().unwrap().linearize_mut();
        for body_ast in else_ast_linearized {
            let body_block_id = self.lower_node(body_ast, fn_ctx)?;
            if last_block_id != body_block_id {
                let new_body_block = self.ir_builder.create_block("else-body-block");
                last_block_id = new_body_block;
                self.ir_builder.link_blocks(body_block_id, new_body_block);
            }
        }

        self.scope.pop();
        Ok(last_block_id)
    }

    fn lower_import(&mut self) -> StmtLoweringResult {
        Ok(INVALID_BLOCK_ID) // import statements aren't supported inside blocks
    }

    fn lower_record_declaration(&mut self, _node: &mut AstNode) -> StmtLoweringResult {
        Ok(INVALID_BLOCK_ID) // record declaration statements aren't supported inside functi blocks
    }

    fn get_func_name(&mut self, index: usize) -> Option<String> {
        self.scope.lookup_fn(FuncId(index)).map(|func| func.name.to_string())
    }
}