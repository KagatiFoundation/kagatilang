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

pub mod aarch64;
pub mod errors;
pub mod typedefs;
pub mod fn_ctx;

use std::cell::RefCell;
use std::rc::Rc;

use fn_ctx::FnCtx;
use kagc_ast::*;
use kagc_ctx::CompilerCtx;
use kagc_ir::gc::GCOBJECT_BUFFER_IDX;
use kagc_ir::ir_instr::*;
use kagc_ir::ir_operands::*;
use kagc_ir::LabelId;
use kagc_target::reg::REG_SIZE_8;
use kagc_types::builtins::obj::KObjType;
use kagc_types::*;
use typedefs::*;

pub trait IRGen {
    fn gen_ir(&mut self, nodes: &mut [AST]) -> Vec<IR> {
        let mut output: Vec<IR> = vec![];
        for node in nodes {
            let node_ir = self.gen_ir_from_node(
                node, 
                &mut FnCtx::new(0), 
                ASTOperation::AST_NONE
            );
            if let Ok(irs) = node_ir {
                output.extend(irs);
            }
            else if let Err(e) = node_ir {
                self.ctx().borrow_mut().diagnostics.push(e);
            }
        }
        output
    }

    fn gen_ir_from_node(
        &mut self, 
        node: &mut AST, 
        fn_ctx: &mut FnCtx, 
        parent_ast_kind: ASTOperation
    ) -> CGRes {
        if node.operation == ASTOperation::AST_GLUE {
            let mut output: Vec<IR> = vec![];
            if let Some(left) = node.left.as_mut() {
                let left_irs: Vec<IR> = self.gen_ir_from_node(left, fn_ctx, parent_ast_kind)?;
                output.extend(left_irs);
            }
            if let Some(right) = node.right.as_mut() {
                let right_irs: Vec<IR> = self.gen_ir_from_node(right, fn_ctx, parent_ast_kind)?;
                output.extend(right_irs);
            }
            Ok(output)
        }
        else if node.operation == ASTOperation::AST_FUNCTION {
            return self.gen_ir_fn(node);
        }
        else if node.operation == ASTOperation::AST_VAR_DECL {
            return self.gen_ir_var_decl(node, fn_ctx);
        }
        else if node.operation == ASTOperation::AST_FUNC_CALL {
            return self.gen_ir_fn_call(node, fn_ctx, None);
        }
        else if node.operation == ASTOperation::AST_LOOP {
            return self.gen_ir_loop(node, fn_ctx);
        }
        else if node.operation == ASTOperation::AST_IF {
            return self.gen_ir_if(node, fn_ctx);
        }
        else if node.operation == ASTOperation::AST_IMPORT {
            return self.lower_import_to_ir();
        }
        else if node.operation == ASTOperation::AST_RECORD_DECL {
            return self.lower_rec_decl_to_ir(node);
        }
        else if node.operation == ASTOperation::AST_RETURN {
            if parent_ast_kind != ASTOperation::AST_FUNCTION {
                fn_ctx.early_return = true;
            }
            return self.gen_ir_return(node, fn_ctx);
        }
        else if node.operation == ASTOperation::AST_NONE {
            return Ok(vec![]);
        }
        else {
            panic!("{:#?} not supported right now!", node);
        }
    }

    fn gen_ir_fn_call(&mut self, node: &mut AST, fn_ctx: &mut FnCtx, dest: Option<IROperand>) -> CGRes {
        if let ASTKind::ExprAST(Expr::FuncCall(func_call)) = &mut node.kind {
            let mut result: Vec<IR> = vec![];
            self.gen_ir_fn_call_expr(func_call, fn_ctx, dest).iter().for_each(|instrs| {
                instrs.iter().for_each(|instr| {
                    result.push(IR::Instr(instr.clone()));
                });
            });
            return Ok(result);
        }
        panic!()
    }

    /// Generate IR nodes from an AST expression node.
    fn gen_ir_expr(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx, dest: Option<IROperand>) -> CGExprEvalRes {
        if !ast.kind.is_expr() {
            panic!("Needed an Expr--but found {ast:#?}");
        }
        
        let expr = ast
            .kind
            .as_expr_mut()
            .unwrap_or_else(|| panic!("Cannot unwrap an expression for some reason. Aborting..."));

        self.__gen_expr(expr, fn_ctx, dest)
    }

    fn __gen_expr(&mut self, expr: &mut Expr, fn_ctx: &mut FnCtx, dest: Option<IROperand>) -> CGExprEvalRes {
        match expr {
            Expr::LitVal(litexpr) => self.gen_lit_ir_expr(litexpr, fn_ctx, dest),
            Expr::Binary(binexpr) => self.gen_bin_ir_expr(binexpr, fn_ctx, dest),
            Expr::Ident(identexpr) => self.gen_ident_ir_expr(identexpr, fn_ctx, dest),
            Expr::FuncCall(funccallexpr) => self.gen_ir_fn_call_expr(funccallexpr, fn_ctx, dest),
            Expr::RecordCreation(ref mut recexpr) => self.lower_rec_creation_to_ir(recexpr, fn_ctx, dest),
            Expr::RecordFieldAccess(recfieldexpr) => self.lower_rec_field_access_to_ir(recfieldexpr, fn_ctx, dest),
            Expr::Null => self.lower_null_const_to_ir(fn_ctx),
            _ => todo!()
        }
    }

    fn lower_rec_creation_to_ir(&mut self, rec_creation: &mut RecordCreationExpr, fn_ctx: &mut FnCtx, dest: Option<IROperand>) -> CGExprEvalRes {
        let mut child_offsets = vec![];
        let mut output = vec![];
        if let Some(entry) = self.ctx().borrow().const_pool.get(rec_creation.pool_idx) {
            match &entry.value {
                kagc_const::pool::KagcConst::Record(record_const) => {
                    for field in record_const.fields.iter() {
                        let load = self.gen_ir_load_global_var(*field.1, fn_ctx)?;
                        let count = load.len();
                        let child_off = fn_ctx.stack_offset - 1;
                        child_offsets.push(child_off);
                        output.extend_from_slice(&load[0..count - 1]);
                    }
                },
                _ => todo!(),
            }
        }

        // Stack offset for the record variable in the current function frame.
        //
        // After allocating a record, `rec_base` stores the stack slot where the
        // record pointer will be saved. This allows the variable name (`rec_alias`)
        // to be mapped to a stable location, so that future instructions can
        // load the record pointer from the stack when accessing or assigning fields.
        let rec_base_off = fn_ctx.stack_offset;
        let alloc_rec = IRInstr::MemAlloc { 
            size: rec_creation.fields.len() * 8,
            ob_type: KObjType::KRec,
            dest: IROperand::CallValue { 
                position: 0, 
                size: REG_SIZE_8
            }
        };

        let store_canon_pointer = IRInstr::Store { 
            src: alloc_rec.dest().unwrap(), 
            addr: IRAddr::StackOff(fn_ctx.next_stack_off()) 
        };

        let load_data_pointer = IRInstr::Load { 
            dest: IROperand::Temp { 
                id: fn_ctx.next_temp(), 
                size: REG_SIZE_8
            }, 
            addr: IRAddr::BaseOff(
                alloc_rec.dest().unwrap(), 
                GCOBJECT_BUFFER_IDX as i32
            )
        };

        let data_pointer_dest = load_data_pointer.dest().unwrap().clone();

        output.push(alloc_rec);
        output.push(store_canon_pointer);
        output.push(load_data_pointer);

        for (idx, c_off) in child_offsets.iter().enumerate() {
            let load_child_mem = IRInstr::Load { 
                dest: IROperand::Temp { 
                    id: fn_ctx.next_temp(), 
                    size: REG_SIZE_8
                }, 
                addr: IRAddr::StackOff(*c_off) 
            };
            let store_child_mem = IRInstr::Store { 
                src: load_child_mem.dest().unwrap(), 
                addr: IRAddr::BaseOff(
                    data_pointer_dest.clone(),
                    idx as i32
                )
            };
            output.push(load_child_mem);
            output.push(store_child_mem);
        }

        let dest = dest.unwrap_or(IROperand::Temp {
            id: fn_ctx.next_temp(),
            size: REG_SIZE_8
        });

        output.push(
            IRInstr::Load { 
                dest, 
                addr: IRAddr::StackOff(rec_base_off)
            }
        );

        // record's base offset
        fn_ctx.var_offsets.insert(rec_creation.rec_alias.clone(), rec_base_off);
        Ok(output)
    }

    fn lower_null_const_to_ir(&mut self, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        Ok(vec![
            IRInstr::mov_into_temp(
                fn_ctx.next_temp(), 
                IROperand::Const(IRImmVal::Null),
                8
            )
        ])
    }

    fn gen_lit_ir_expr(&mut self, lit_expr: &LitValExpr, fn_ctx: &mut FnCtx, dest: Option<IROperand>) -> CGExprEvalRes {
        if let LitType::PoolStr(pool_idx) = &lit_expr.value  {
            return self.gen_ir_load_str(*pool_idx, fn_ctx);
        }

        let (ir_lit, reg_size) = match lit_expr.result_type {
            LitTypeVariant::I32 => (IRImmVal::Int32(*lit_expr.value.unwrap_i32().expect("No i32 value!")), 8_usize),
            LitTypeVariant::U8 => (IRImmVal::U8(*lit_expr.value.unwrap_u8().expect("No u8 value!")), 8_usize),
            _ => todo!(),
        };

        let dest = dest.unwrap_or(IROperand::Temp {
            id: fn_ctx.next_temp(),
            size: reg_size,
        });

        Ok(vec![
            IRInstr::Mov { 
                dest, 
                src: IROperand::Const(ir_lit)
            }
        ])
    }

    fn gen_ir_load_str(&mut self, idx: usize, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        self.gen_ir_load_global_var(idx, fn_ctx)
    }

    fn gen_bin_ir_expr(&mut self, bin_expr: &mut BinExpr, fn_ctx: &mut FnCtx, dest: Option<IROperand>) -> CGExprEvalRes {
        let mut irs: Vec<IRInstr> = vec![];

        let eval_left_expr: Vec<IRInstr> = self.__gen_expr(&mut bin_expr.left, fn_ctx, None)?;
        let eval_right_expr: Vec<IRInstr> = self.__gen_expr(&mut bin_expr.right, fn_ctx, None)?;

        let left_dest = eval_left_expr
            .last()
            .unwrap_or_else(|| panic!("No left destination found! Abort!"));
        let right_dest = eval_right_expr
            .last()
            .unwrap_or_else(|| panic!("No left destination found! Abort!"));

        fn dest_extd_temp(fn_ctx: &mut FnCtx, result_type: LitTypeVariant) -> IROperand {
            let reg_sz = result_type.to_reg_size();
            IROperand::Temp { id: fn_ctx.next_temp(), size: reg_sz }
        }

        let dest = dest.unwrap_or(dest_extd_temp(fn_ctx, bin_expr.result_type.clone()));

        let perform_bin_opr: IRInstr = match bin_expr.operation {
            ASTOperation::AST_ADD => {
                self.lower_add_to_ir(
                    dest,
                    left_dest.dest().unwrap(), 
                    right_dest.dest().unwrap()
                )
            },
            ASTOperation::AST_SUBTRACT => {
                self.lower_sub_to_ir(
                    dest,
                    left_dest.dest().unwrap(), 
                    right_dest.dest().unwrap()
                )
            },
            ASTOperation::AST_MULTIPLY => {
                self.lower_mul_to_ir(
                    dest,
                    left_dest.dest().unwrap(), 
                    right_dest.dest().unwrap()
                )
            },
            ASTOperation::AST_DIVIDE => {
                self.lower_div_to_ir(
                    dest,
                    left_dest.dest().unwrap(), 
                    right_dest.dest().unwrap()
                )
            },

            ASTOperation::AST_GTHAN | ASTOperation::AST_LTHAN   | 
            ASTOperation::AST_LTEQ  | ASTOperation::AST_GTEQ    | 
            ASTOperation::AST_NEQ   | ASTOperation::AST_EQEQ => 
            {
                let parent_ast_kind: ASTOperation = fn_ctx.parent_ast_kind;
                let is_cond_dependent = (parent_ast_kind == ASTOperation::AST_IF) || (parent_ast_kind == ASTOperation::AST_WHILE);
                if is_cond_dependent {
                    self.gen_ir_cmp_and_jump(
                        left_dest.dest().unwrap(), 
                        right_dest.dest().unwrap(), 
                        fn_ctx.force_label_use, 
                        IRCondOp::from(bin_expr.operation)
                    )
                } 
                else {
                    todo!()
                } 
            },

            _ => todo!()
        };

        irs.extend(eval_left_expr);
        irs.extend(eval_right_expr);
        irs.push(perform_bin_opr);
        Ok(irs)
    }

    fn gen_ir_cmp_and_jump(
        &mut self, 
        op1: IROperand, 
        op2: IROperand, 
        label_id: LabelId, 
        operation: IRCondOp
    ) -> IRInstr {
        IRInstr::CondJump {
            label_id,
            op1,
            op2,
            operation
        }
    }

    fn gen_ir_fn_call_expr(
        &mut self, 
        func_call_expr: &mut FuncCallExpr, 
        fn_ctx: &mut FnCtx, 
        dest: Option<IROperand>
    ) -> CGExprEvalRes;

    fn lower_add_to_ir(&mut self, dest: IROperand, op1: IROperand, op2: IROperand) -> IRInstr {
       IRInstr::Add { dest, op1, op2 }
    }

    fn lower_sub_to_ir(&mut self, dest: IROperand, op1: IROperand, op2: IROperand) -> IRInstr {
       IRInstr::Sub { dest, op1, op2 }
    }

    fn lower_mul_to_ir(&mut self, dest: IROperand, op1: IROperand, op2: IROperand) -> IRInstr {
       IRInstr::Mul { dest, op1, op2 }
    }

    fn lower_div_to_ir(&mut self, dest: IROperand, op1: IROperand, op2: IROperand) -> IRInstr {
       IRInstr::Div { dest, op1, op2 }
    }

    fn lower_rec_field_access_to_ir(
        &mut self, access: &mut RecordFieldAccessExpr, 
        fn_ctx: &mut FnCtx, 
        dest: Option<IROperand>
    ) -> CGExprEvalRes;

    fn gen_ir_fn(&mut self, ast: &mut AST) -> CGRes;

    fn gen_ir_var_decl(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> CGRes;

    fn gen_ir_return(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> CGRes;

    fn gen_ir_loop(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> CGRes;

    fn gen_ir_if(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> CGRes;

    fn gen_ir_label(&self, label_id: LabelId) -> CGRes;

    fn gen_ir_jump(&self, label_id: LabelId) -> CGRes;

    fn lower_import_to_ir(&self) -> CGRes;

    fn lower_rec_decl_to_ir(&mut self, node: &mut AST) -> CGRes;

    fn gen_ident_ir_expr(&mut self, ident_expr: &IdentExpr, fn_ctx: &mut FnCtx, dest: Option<IROperand>) -> CGExprEvalRes;

    fn gen_ir_load_global_var(&mut self, idx: usize, fn_ctx: &mut FnCtx) -> CGExprEvalRes;

    fn ctx(&self) -> Rc<RefCell<CompilerCtx>>;
}