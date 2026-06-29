// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use core::panic;
use std::vec;

use kagc_ast::*;
use kagc_symbol::*;
use kagc_types::*;
use kagc_mir::instruction::IrLocation;
use kagc_types::builtins::obj::KObjType;
use kagc_const::pool::{ConstPool, KagcConst};
use kagc_scope::ScopeCtx;
use kagc_scope::ScopeId;

use kagc_mir::value::{IrValue, IrValueId};
use kagc_mir::block::{BlockId, Terminator, INVALID_BLOCK_ID};
use kagc_mir::instruction::{IrCondition, IrInstruction};
use kagc_mir::mir_builder::IrBuilder;
use kagc_mir::function::FunctionParam;
use kagc_mir::types::IrType;
use kagc_mir::builtin::BuiltinFn;
use kagc_errors::diagnostic::Diagnostic;
use kagc_symbol::function::Func;
use kagc_utils::bug;

use crate::fn_ctx::FunctionContext;
use crate::loop_ctx::LoopContext;

type ExprLoweringResult = Result<IrValueId, Diagnostic>;
type StmtLoweringResult = Result<BlockId, Diagnostic>;

pub struct AstToMirLowerer<'a, 'tcx> {
    scope: &'tcx ScopeCtx<'tcx>,
    const_pool: &'a mut ConstPool,
    pub ir_builder: IrBuilder,
    current_function: Option<Func<'tcx>>,
}

impl<'a, 'tcx> AstToMirLowerer<'a, 'tcx> {
    pub fn new(scope: &'tcx ScopeCtx<'tcx>, const_pool: &'a mut ConstPool) -> Self {
        Self {
            scope,
            const_pool,
            current_function: None,
            ir_builder: IrBuilder::default()
        }
    }

    pub fn lower(&mut self, nodes: &mut [AstNode]) -> StmtLoweringResult {
        let mut fn_ctx = FunctionContext::new();
        for node in nodes {
            self.lower_node(node, &mut fn_ctx)?;
        }
        Ok(BlockId(1))
    }

    pub fn lower_node(&mut self, node: &mut AstNode, fn_ctx: &mut FunctionContext) -> StmtLoweringResult {
        match node.op {
            AstOp::Func    => self.lower_function(node),
            AstOp::FuncCall => self.lower_function_call(node, fn_ctx),
            AstOp::VarDecl  => self.lower_variable_declaration(node, fn_ctx),
            AstOp::Return   => self.lower_return(node, fn_ctx),
            AstOp::Import   => self.lower_import(),
            AstOp::RecDecl  => self.lower_record_declaration(node),
            AstOp::Loop     => self.lower_infinite_loop(node, fn_ctx),
            AstOp::If       => self.lower_if_else_tree(node, fn_ctx),
            AstOp::Else     => self.lower_else_block(node, fn_ctx),
            AstOp::Block    => self.lower_block(node, fn_ctx),
            _ => todo!("{node_type:#?}", node_type = node.op),
        }
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
        let func_decl = ast.expect_func_decl_stmt(); 
        
        let Some(func_scope) = self.scope.lookup_node_scope(ast.id) else {
            bug!("Function {name}'s scope not found", name = func_decl.name);
        };

        let Some(func) = self.scope.lookup_fn_by_name(func_decl.name) else {
            bug!("Function '{name}' not found", name = func_decl.name);
        };

        self.current_function = Some(func.clone());
        let storage_class = func.storage_class;

        self.scope.enter(func_scope.id.get()); 
        let mut fn_ctx = FunctionContext::new();

        let func_ir_params = self
            .scope
            .collect_params(func_scope.id.get())
            .iter()
            .map(|&sym| {
                let param = self.ir_builder.create_function_parameter(
                    IrType::from(sym.ty.get()), 
                );

				let param_id = param.id;
				let variable_id = fn_ctx.next_variable_id();

				self.ir_builder.inst(
					IrInstruction::Store { src: param_id, location: IrLocation::Variable(variable_id) }
				);

				param
            }).collect::<Vec<FunctionParam>>();

        let func_anchor = self.ir_builder.create_function(
            func.name.to_string(),
            func_ir_params,
            IrType::from(ast.ty.unwrap_or_else(|| bug!("Function return type must be defined"))),
            storage_class
        );

        if storage_class == StorageClass::EXTERN {
            return Ok(INVALID_BLOCK_ID);
        }

        let return_label = func_anchor.exit_block; 
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
            self.ir_builder.set_terminator(
                current_block_id, 
                Terminator::Return { 
                    value: None, 
                    target: return_label 
                }
            );
        }

        self.current_function = None;
        self.scope.pop(); 
        Ok(current_block_id)
    }

    fn lower_function_call(&mut self, node: &mut AstNode, fn_ctx: &mut FunctionContext) -> StmtLoweringResult {
        if let NodeKind::ExprAST(Expr::FuncCall(func_call)) = &mut node.kind {
            let _ = self.lower_function_call_expr(func_call, fn_ctx)?;
            return Ok(self.ir_builder.current_block_id_unchecked());
        }
        panic!("Invalid function call assignment matching state")
    }

    fn lower_variable_declaration(&mut self, var_ast: &mut AstNode, fn_ctx: &mut FunctionContext) -> StmtLoweringResult {
        if var_ast.left.is_none() { 
            bug!("Variable is not assigned a value!");
        }

        let assigned_expr = var_ast.left.as_mut().unwrap(); 
        let assigned_expr_value_id = self.lower_expression_ast(assigned_expr, fn_ctx)?;

        let var_decl = var_ast.expect_var_decl_stmt_mut();
        
        self.ir_builder.inst(
            IrInstruction::Store { 
                src: assigned_expr_value_id, 
                location: IrLocation::Variable(fn_ctx.next_variable_id())
            }
        );

		fn_ctx.map_var(var_decl.sym_name.to_string());

        Ok(self.ir_builder.current_block_id_unchecked())
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
            // Expr::RecordFieldAccess(rec_field_access) => self.lower_record_field_access_expr(rec_field_access, fn_ctx),
            // Expr::RecordCreation(rec_create_expr) => self.lower_record_creation_expr(rec_create_expr, fn_ctx),
            _ => unimplemented!()
        }
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
                IrInstruction::Call { 
                    func: func_call_expr.symbol_name.to_string(), 
                    args: func_call_args,
                    result: Some(call_result_value)
                }
            );
            Ok(call_result_value)
        }
        else {
            self.ir_builder.inst(
                IrInstruction::Call { 
                    func: func_call_expr.symbol_name.to_string(), 
                    args: func_call_args,
                    result: None
                }
            );
            Ok(IrValueId(0xFFFFFFFF))
        }
    }

    fn lower_literal_value_expr(&mut self, lit_expr: &LitValExpr, _fn_ctx: &mut FunctionContext) -> ExprLoweringResult {
        if let Literal::RawStr(str_value) = &lit_expr.value {
            let const_value = self.ir_builder.occupy_value_id();
            let pool_index = self.const_pool.insert(KagcConst::Str(str_value.to_string()), KObjType::KStr, None);
            let const_size = self.const_pool.size(pool_index).unwrap_or_else(|| bug!("cannot find const entry"));

            self.ir_builder.inst(
                IrInstruction::LoadConst { 
                    label_id: pool_index,
                    result: const_value
                }
            );

            let const_size_value = self.ir_builder.create_move(IrValue::Constant(const_size as i64));
            let call_result_value = self.ir_builder.occupy_value_id();

            self.ir_builder.inst(
                IrInstruction::CallBuiltin { 
                    builtin: BuiltinFn::AllocStr, 
                    args: vec![const_value, const_size_value],
                    result: Some(call_result_value)
                }
            );
            return Ok(call_result_value);
        }
        match lit_expr.ty {
            TyKind::I64 => {
                let const_value = *lit_expr.value.unwrap_i64().expect("No i64 value!");
                Ok(self.ir_builder.create_move(IrValue::Constant(const_value)))
            },
            TyKind::U8 => {
                let const_value = *lit_expr.value.unwrap_u8().expect("No u8 value!") as i64;
                Ok(self.ir_builder.create_move(IrValue::Constant(const_value)))
            },
            _ => unimplemented!("{lit_expr:#?}")
        }
    }

    fn lower_identifier_expr(&mut self, ident_expr: &IdentExpr, fn_ctx: &mut FunctionContext) -> ExprLoweringResult {
        let sym = self.scope.lookup_sym(None, ident_expr.sym_name).unwrap(); 

		let var_id = fn_ctx.get_mapped_var_unchecked(sym.name.to_string());

        let load_value_id = self.ir_builder.create_load(IrLocation::Variable(var_id));
        Ok(load_value_id)
    }

    fn lower_binary_expr(&mut self, bin_expr: &mut BinExpr, fn_ctx: &mut FunctionContext) -> ExprLoweringResult {
        let lhs_value_id = self.lower_expression(&mut bin_expr.left, fn_ctx)?;
        let rhs_value_id = self.lower_expression(&mut bin_expr.right, fn_ctx)?;
        match bin_expr.operation {
            // FIX: Repaired the inverted Multiply/Divide copy-paste error
            AstOp::Add       => Ok(self.ir_builder.create_add(IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
            AstOp::Subtract  => Ok(self.ir_builder.create_subtract(IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
            AstOp::Multiply  => Ok(self.ir_builder.create_multiply(IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
            AstOp::Divide    => Ok(self.ir_builder.create_divide(IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
            AstOp::EqEq      => Ok(self.ir_builder.create_conditional_jump(IrCondition::EqEq, IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
            AstOp::NEq       => Ok(self.ir_builder.create_conditional_jump(IrCondition::NEq, IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
            AstOp::LtEq      => Ok(self.ir_builder.create_conditional_jump(IrCondition::LTEq, IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
            AstOp::GtEq      => Ok(self.ir_builder.create_conditional_jump(IrCondition::GTEq, IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
            AstOp::LThan     => Ok(self.ir_builder.create_conditional_jump(IrCondition::LThan, IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
            AstOp::GThan     => Ok(self.ir_builder.create_conditional_jump(IrCondition::GThan, IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
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
		let loop_head_id = self.ir_builder.create_block("loop_head");
        let loop_body_id = self.ir_builder.create_block("loop_body");
        let loop_tail_id = self.ir_builder.create_block("loop_exit"); // Utilizing create_block hierarchy mapping

        fn_ctx.enter_loop(LoopContext { head_block: loop_body_id, exit_block: loop_tail_id });

        self.ir_builder.set_terminator(prev_block_id, Terminator::Jump(loop_head_id));
        self.ir_builder.link_blocks(prev_block_id, loop_head_id);

        self.ir_builder.switch_to_block(loop_head_id);
        self.ir_builder.set_terminator(loop_head_id, Terminator::Jump(loop_body_id));
        self.ir_builder.link_blocks(loop_head_id, loop_body_id);

        self.ir_builder.switch_to_block(loop_body_id);

        let mut linearized_body = ast.left.as_mut().unwrap().linearize_mut();
        let active_tail_block = self.lower_linear_sequence(&mut linearized_body, fn_ctx)?;

        if !self.ir_builder.has_terminator(active_tail_block) {
            self.ir_builder.set_terminator(active_tail_block, Terminator::Jump(loop_head_id));
            self.ir_builder.link_blocks(active_tail_block, loop_head_id);
        }

        fn_ctx.exit_loop();
        
        self.ir_builder.switch_to_block(loop_tail_id);
        Ok(loop_tail_id)
    }

    fn lower_if_else_tree(&mut self, ast: &mut AstNode, fn_ctx: &mut FunctionContext) -> StmtLoweringResult {
        if let NodeKind::StmtAST(Stmt::If) = &ast.kind {
            self.scope.enter(ScopeId(0));
        }
        let prev_block_id = self.ir_builder.current_block_id_unchecked();

        let conditional_block = self.ir_builder.create_block("if-header");
        self.ir_builder.set_terminator(prev_block_id, Terminator::Jump(conditional_block));
        self.ir_builder.link_blocks(prev_block_id, conditional_block);
        
        self.ir_builder.switch_to_block(conditional_block);
        let if_stmt_cond_value = self.lower_expression_ast(ast.left.as_mut().unwrap(), fn_ctx)?;

        let then_block = self.ir_builder.create_block("then");
        let else_block = self.ir_builder.create_block("else");
        let merge_block = self.ir_builder.create_block("merge");

        self.ir_builder.link_blocks_multiple(conditional_block, vec![then_block, else_block]);

        self.ir_builder.set_terminator(
            conditional_block, 
            Terminator::CondJump { 
                jump_value_id: if_stmt_cond_value, 
                then_block, 
                else_block,
				cond: IrCondition::EqEq
            }
        );

        self.ir_builder.switch_to_block(then_block);
        if let Some(mid_tree) = &mut ast.mid {
            let then_tail = self.lower_linear_sequence(&mut mid_tree.linearize_mut(), fn_ctx)?;
            if !self.ir_builder.has_terminator(then_tail) {
                self.ir_builder.set_terminator(then_tail, Terminator::Jump(merge_block));
                self.ir_builder.link_blocks(then_tail, merge_block);
            }
        } else if !self.ir_builder.has_terminator(then_block) {
            self.ir_builder.set_terminator(then_block, Terminator::Jump(merge_block));
            self.ir_builder.link_blocks(then_block, merge_block);
        }
        self.scope.pop();
        
        self.ir_builder.switch_to_block(else_block);
        if let Some(right_tree) = &mut ast.right {
            let else_tail = self.lower_linear_sequence(&mut right_tree.linearize_mut(), fn_ctx)?;
            if !self.ir_builder.has_terminator(else_tail) {
                self.ir_builder.set_terminator(else_tail, Terminator::Jump(merge_block));
                self.ir_builder.link_blocks(else_tail, merge_block);
            }
        } else if !self.ir_builder.has_terminator(else_block) {
            self.ir_builder.set_terminator(else_block, Terminator::Jump(merge_block));
            self.ir_builder.link_blocks(else_block, merge_block);
        }

        self.ir_builder.switch_to_block(merge_block);
        Ok(merge_block)
    }

    pub fn lower_linear_sequence(&mut self, stmts: &mut [&mut AstNode], fn_ctx: &mut FunctionContext) -> StmtLoweringResult {
        let mut current = self.ir_builder.current_block_id_unchecked();
        let stmts_len = stmts.len();
        for (idx, stmt) in stmts.iter_mut().enumerate() {
            if self.ir_builder.has_terminator(current) {
                break;
            }
            
            current = self.lower_node(stmt, fn_ctx)?;
            
            if idx < stmts_len - 1 {
                current = self.ir_builder.ensure_continuation_block(current);
            }
        }
        Ok(current)
    }

    fn lower_else_block(&mut self, ast: &mut AstNode, fn_ctx: &mut FunctionContext) -> StmtLoweringResult {
        if let NodeKind::StmtAST(Stmt::Scoping) = &ast.kind {
            self.scope.enter(ScopeId(0)); 
        } else {
            bug!("provided AST tree is not of type 'AST_ELSE'");
        }

        let mut last_block_id = self.ir_builder.current_block_id_unchecked();
        let else_ast_linearized = ast.left.as_mut().unwrap().linearize_mut();
        
        for body_ast in else_ast_linearized {
            if self.ir_builder.has_terminator(last_block_id) {
                break;
            }
            let body_block_id = self.lower_node(body_ast, fn_ctx)?;
            if last_block_id != body_block_id {
                let new_body_block = self.ir_builder.create_block("else-body-block");
                self.ir_builder.link_blocks(body_block_id, new_body_block);
                self.ir_builder.switch_to_block(new_body_block);
                last_block_id = new_body_block;
            }
        }

        self.scope.pop();
        Ok(last_block_id)
    }

    fn lower_import(&mut self) -> StmtLoweringResult {
        Ok(self.ir_builder.current_block_id_unchecked()) 
    }

    fn lower_record_declaration(&mut self, _node: &mut AstNode) -> StmtLoweringResult {
        Ok(self.ir_builder.current_block_id_unchecked()) 
    }
}