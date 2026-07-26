// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use core::panic;
use std::vec;

use kagc_ast::*;
use kagc_symbol::*;
use kagc_types::*;
use kagc_comp_unit::CompUnit;
use kagc_mir::loop_ctx::IrLoopContext;
use kagc_types::builtins::obj::KObjType;
use kagc_const::pool::{ConstPool, KagcConst};
use kagc_scope::ScopeCtx;

use kagc_mir::value::{IrValue, IrValueId};
use kagc_mir::block::{BlockId, Terminator, INVALID_BLOCK_ID};
use kagc_mir::instruction::{IrCondition, IrInstruction};
use kagc_mir::mir_builder::IrBuilder;
use kagc_mir::types::IrType;
use kagc_mir::builtin::BuiltinFn;
use kagc_mir::instruction::IrLocation;
use kagc_mir::function::IrFunctionContext;

use kagc_errors::diagnostic::Diagnostic;
use kagc_symbol::function::Func;
use kagc_utils::bug;

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

	pub fn lower_comp_unit(&mut self, unit: &mut CompUnit<'_>) {
		for ast in &mut unit.asts {
			match ast.op {
				AstOp::Func => {
					_ = self.lower_function(ast);
				},
				AstOp::Import
				| AstOp::RecDecl => {},
            	_ => todo!("{node_type:#?}", node_type = ast.op),
			}
		}
	}

    fn lower_node(&mut self, node: &mut AstNode, fn_ctx: &mut IrFunctionContext) -> StmtLoweringResult {
        match node.op {
            AstOp::Func    => self.lower_function(node),
            AstOp::FuncCall => self.lower_function_call(node, fn_ctx),
            AstOp::VarDecl  => self.lower_variable_declaration(node, fn_ctx),
            AstOp::Return   => self.lower_return(node, fn_ctx),
            AstOp::Loop     => self.lower_infinite_loop(node, fn_ctx),
            AstOp::If       => self.lower_if_else_tree(node, fn_ctx),
            AstOp::Block    => self.lower_block(node, fn_ctx),
			AstOp::Break	=> self.lower_break(node, fn_ctx),
			AstOp::Continue	=> self.lower_continue(node, fn_ctx),
			AstOp::Assign	=> self.lower_assignment(node, fn_ctx),
            _ => todo!("{node_type:#?} not supported right now", node_type = node.op),
        }
    }

    fn lower_block(&mut self, ast: &mut AstNode, fn_ctx: &mut IrFunctionContext) -> StmtLoweringResult {
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

        let func_ir_params = self
            .scope
            .collect_params(func_scope.id.get())
            .iter()
            .map(|&sym| sym.ty.get())
			.collect::<Vec<TyKind<'_>>>();

        let mut func_context = self.ir_builder.create_function(
            func.name.to_string(),
            func_ir_params,
            IrType::from(ast.ty.unwrap_or_else(|| bug!("Function return type must be defined"))),
            storage_class
        );

        if storage_class == StorageClass::EXTERN {
            return Ok(INVALID_BLOCK_ID);
        }

        let return_label = func_context.anchor.exit_block; 
        func_context.set_return_label(return_label);

        let Some(func_body) = &mut ast.left else {
            bug!("no function body found");
        };

        let func_body_block = func_body.expect_block_stmt_mut();
        let mut statements = func_body_block
            .statements
            .iter_mut()
            .collect::<Vec<&mut AstNode>>();
            
        let current_block_id = self.lower_linear_sequence(&mut statements, &mut func_context)?;

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

    fn lower_function_call(&mut self, node: &mut AstNode, fn_ctx: &mut IrFunctionContext) -> StmtLoweringResult {
        if let NodeKind::ExprAST(Expr::FuncCall(func_call)) = &mut node.kind {
            let _ = self.lower_function_call_expr(func_call, fn_ctx)?;
            return Ok(self.ir_builder.current_block_id_unchecked());
        }
        panic!("Invalid function call assignment matching state")
    }

    fn lower_variable_declaration(&mut self, var_ast: &mut AstNode, fn_ctx: &mut IrFunctionContext) -> StmtLoweringResult {
        if var_ast.left.is_none() { 
            bug!("Variable is not assigned a value!");
        }

        let assigned_expr = var_ast.left.as_mut().unwrap(); 
        let assigned_expr_value_id = self.lower_expression_ast(assigned_expr, fn_ctx)?;

        let var_decl = var_ast.expect_var_decl_stmt_mut();

		let var_id = fn_ctx.map_var(var_decl.sym_name.to_string());
        
        self.ir_builder.inst(
            IrInstruction::Store { 
                src: assigned_expr_value_id, 
                location: IrLocation::Variable(var_id)
            }
        );

        Ok(self.ir_builder.current_block_id_unchecked())
    }

	fn lower_assignment(&mut self, ast: &mut AstNode, fn_ctx: &mut IrFunctionContext) -> StmtLoweringResult {
        let assigned_expr = ast.left.as_mut().unwrap(); 
		let assigned_expr_value_id = self.lower_expression_ast(assigned_expr, fn_ctx)?;

		let assignment = ast.expect_assignment_stmt_mut();

		let var_id = fn_ctx.get_mapped_var_unchecked(assignment.sym_name.to_string());

        self.ir_builder.inst(
            IrInstruction::Store { 
                src: assigned_expr_value_id, 
                location: IrLocation::Variable(var_id)
            }
        );

		Ok(self.current_block_unchecked())
	}

    fn lower_expression_ast(&mut self, ast: &mut AstNode, fn_ctx: &mut IrFunctionContext) -> ExprLoweringResult {
		let _ = ast.expect_expr();

        let expr = ast
            .kind
            .as_expr_mut()
            .unwrap_or_else(|| bug!("cannot lower an expression"));
        self.lower_expression(expr, fn_ctx)
    }

    fn lower_expression(&mut self, expr: &mut Expr, fn_ctx: &mut IrFunctionContext) -> ExprLoweringResult {
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

    fn lower_function_call_expr(&mut self, func_call_expr: &mut FuncCallExpr, fn_ctx: &mut IrFunctionContext) -> ExprLoweringResult {
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

    fn lower_literal_value_expr(&mut self, lit_expr: &LitValExpr, _fn_ctx: &mut IrFunctionContext) -> ExprLoweringResult {
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

    fn lower_identifier_expr(&mut self, ident_expr: &IdentExpr, fn_ctx: &mut IrFunctionContext) -> ExprLoweringResult {
        let sym = self
			.scope
			.lookup_sym(None, ident_expr.sym_name)
			.unwrap_or_else(|| panic!("symbol '{}' not found", ident_expr.sym_name)); 

		let var_id = fn_ctx.get_mapped_var_unchecked(sym.name.to_string());

        let load_value_id = self.ir_builder.create_load(IrLocation::Variable(var_id));
        Ok(load_value_id)
    }

    fn lower_binary_expr(&mut self, bin_expr: &mut BinExpr, fn_ctx: &mut IrFunctionContext) -> ExprLoweringResult {
        let lhs_value_id = self.lower_expression(&mut bin_expr.left, fn_ctx)?;
        let rhs_value_id = self.lower_expression(&mut bin_expr.right, fn_ctx)?;
        match bin_expr.operation {
            AstOp::Add       => Ok(self.ir_builder.create_add(IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
            AstOp::Subtract  => Ok(self.ir_builder.create_subtract(IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
            AstOp::Multiply  => Ok(self.ir_builder.create_multiply(IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
            AstOp::Divide    => Ok(self.ir_builder.create_divide(IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
			AstOp::EqEq 	 => Ok(self.ir_builder.create_conditional_eqeq(IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
            AstOp::NEq       => Ok(self.ir_builder.create_conditional_neq(IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
            AstOp::LThan     => Ok(self.ir_builder.create_conditional_lthan(IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
            AstOp::GThan     => Ok(self.ir_builder.create_conditional_gthan(IrValue::Register(lhs_value_id), IrValue::Register(rhs_value_id))),
            _ => unimplemented!()
        }
    }

    fn lower_return(&mut self, ret_stmt: &mut AstNode, fn_ctx: &mut IrFunctionContext) -> StmtLoweringResult {
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

    fn lower_infinite_loop(&mut self, ast: &mut AstNode, fn_ctx: &mut IrFunctionContext) -> StmtLoweringResult {
        let prev_block_id = self.current_block_unchecked();
		let loop_head_id = self.ir_builder.create_block("loop_head");
        let loop_body_id = self.ir_builder.create_block("loop_body");
        let loop_tail_id = self.ir_builder.create_block("loop_exit");

        fn_ctx.enter_loop(IrLoopContext { head_block: loop_head_id, exit_block: loop_tail_id });

		if !self.ir_builder.has_terminator(prev_block_id) {
        	self.ir_builder.set_terminator(prev_block_id, Terminator::Fallthrough(loop_head_id));
		}
		 
        self.ir_builder.link_blocks(prev_block_id, loop_head_id);

		if let Some(loop_condition_node) = &mut ast.right {
        	self.ir_builder.switch_to_block(loop_head_id);

			// if there is a condition for the loop to run, first lower it and insert
			// an 'if' statement like 'conditional' jump terminator for the loop header block
			let jump_value_id = self.lower_expression_ast(loop_condition_node, fn_ctx)?;

			self.ir_builder.set_terminator(
				loop_head_id,
				Terminator::CondJump {
					jump_value_id,
					cond: IrCondition::EqEq,
					then_block: loop_body_id,
					else_block: loop_tail_id
				}
			);
		}
		else {
			// if no condition for the loop is present, simply fallthrough to the loop body
        	self.ir_builder.set_terminator(loop_head_id, Terminator::Fallthrough(loop_body_id));
		}
  
		self.ir_builder.link_blocks_multiple(loop_head_id, vec![loop_body_id, loop_tail_id]);

        self.ir_builder.switch_to_block(loop_body_id);

        if let Some(left_tree) = &mut ast.left {
        	let Some(loop_scope) = self.scope.lookup_node_scope(left_tree.id) else {
            	bug!("scope not found");
        	};

			self.scope.enter(loop_scope.id.get());

        	let mut linearized_body = ast.left.as_mut().unwrap().linearize_mut();
        	let active_tail_block = self.lower_linear_sequence(&mut linearized_body, fn_ctx)?;

			self.ir_builder.link_blocks(active_tail_block, loop_head_id);

        	if !self.ir_builder.has_terminator(active_tail_block) {
            	self.ir_builder.set_terminator(active_tail_block, Terminator::Jump(loop_head_id));
        	}

			self.scope.pop();
		}
        
        fn_ctx.exit_loop(); // exit the current loop

        self.ir_builder.switch_to_block(loop_tail_id);
        Ok(loop_tail_id)
    }

    fn lower_if_else_tree(&mut self, ast: &mut AstNode, fn_ctx: &mut IrFunctionContext) -> StmtLoweringResult {
        let prev_block_id = self.ir_builder.current_block_id_unchecked();
        let conditional_block = self.ir_builder.create_block("if-header");

        self.ir_builder.set_terminator(prev_block_id, Terminator::Fallthrough(conditional_block));
        self.ir_builder.link_blocks(prev_block_id, conditional_block);
        
        self.ir_builder.switch_to_block(conditional_block);
        let if_stmt_cond_value = self.lower_expression_ast(ast.left.as_mut().unwrap(), fn_ctx)?;

        let merge_block = self.ir_builder.reserve_block("if-else-merge");

        let then_block = if let Some(mid_tree) = &mut ast.mid {
			let then_block = self.ir_builder.create_block("then");
			self.ir_builder.link_blocks(conditional_block, then_block);
        	self.ir_builder.switch_to_block(then_block);

        	let Some(then_scope) = self.scope.lookup_node_scope(mid_tree.id) else {
            	bug!("then scope not found");
        	};

			self.scope.enter(then_scope.id.get());

            let then_tail = self.lower_linear_sequence(&mut mid_tree.linearize_mut(), fn_ctx)?;

            if !self.ir_builder.has_terminator(then_tail) {
				if ast.right.is_some() { // if there is an 'else' statement, skip (or jump over) it
                	self.ir_builder.set_terminator(then_tail, Terminator::Jump(merge_block));
				}
				else { // otherwise it is safe to fallthrough to the 'merge' block
                	self.ir_builder.set_terminator(then_tail, Terminator::Fallthrough(merge_block));
				}
            }
        	
            self.ir_builder.link_blocks(then_tail, merge_block);
			self.scope.pop(); // come out of 'then' scope
			then_block
        }
		else {
			bug!("mid-tree cannot be None for an 'if' AST node")
		};

        let jump_block = if let Some(right_tree) = &mut ast.right {
        	let else_block = self.ir_builder.create_block("else");
			
			self.ir_builder.link_blocks(conditional_block, else_block);
        	self.ir_builder.switch_to_block(else_block);

        	let Some(else_scope) = self.scope.lookup_node_scope(right_tree.id) else {
            	bug!("else scope not found");
        	};

			self.scope.enter(else_scope.id.get());

            let else_tail = self.lower_linear_sequence(&mut right_tree.linearize_mut(), fn_ctx)?;

            if !self.ir_builder.has_terminator(else_tail) {
                self.ir_builder.set_terminator(else_tail, Terminator::Fallthrough(merge_block));
            }

            self.ir_builder.link_blocks(else_tail, merge_block);
        	self.scope.pop(); // come out of 'else' scope
			else_block
        }
		else { 
			self.ir_builder.link_blocks(conditional_block, merge_block);
			merge_block
		};

        self.ir_builder.set_terminator(
            conditional_block, 
            Terminator::CondJump { 
                jump_value_id: if_stmt_cond_value, 
                then_block, 
                else_block: jump_block,
				cond: IrCondition::EqEq
            }
        );

		self.ir_builder.commit_and_switch_block(merge_block);
        Ok(merge_block)
    }

	fn lower_break(&mut self, _ast: &mut AstNode, fn_ctx: &mut IrFunctionContext) -> StmtLoweringResult {
		let Some(current_loop) = fn_ctx.current_loop() else {
			bug!("No current loop context is present");
		};

		self.ir_builder.create_jump(current_loop.exit_block);
		Ok(self.ir_builder.current_block_id_unchecked())
	}

	fn lower_continue(&mut self, _ast: &mut AstNode, fn_ctx: &mut IrFunctionContext) -> StmtLoweringResult {
		let Some(current_loop) = fn_ctx.current_loop() else {
			bug!("No current loop context is present");
		};

		self.ir_builder.create_jump(current_loop.head_block);
		Ok(self.ir_builder.current_block_id_unchecked())
	}

    fn lower_linear_sequence(&mut self, stmts: &mut [&mut AstNode], fn_ctx: &mut IrFunctionContext) -> StmtLoweringResult {
        let mut current = self.ir_builder.current_block_id_unchecked();

        for stmt in stmts.iter_mut() {
            current = self.lower_node(stmt, fn_ctx)?;
        }
  
        Ok(current)
    }

	fn current_block_unchecked(&self) -> BlockId {
		self.ir_builder.current_block_id_unchecked()
	}
}