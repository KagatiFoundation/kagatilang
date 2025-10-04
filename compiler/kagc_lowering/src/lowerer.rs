// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use core::panic;
use std::cell::RefCell;
use std::rc::Rc;
use std::vec;

use kagc_ast::*;
use kagc_mir::block::Terminator;
use kagc_mir::builder::IRBuilder;
use kagc_mir::function::FunctionParam;
use kagc_mir::instruction::IRAddress;
use kagc_mir::instruction::IRCondition;
use kagc_mir::instruction::IRInstruction;
use kagc_mir::ir_instr::*;
use kagc_mir::ir_operands::*;
use kagc_mir::types::IRType;
use kagc_mir::value::IRValue;
use kagc_mir::value::IRValueId;
use kagc_mir::GCOBJECT_BUFFER_IDX;
use kagc_symbol::*;
use kagc_backend::reg::*;
use kagc_types::*;

use kagc_const::pool::KagcConst;
use kagc_ctx::CompilerCtx;
use kagc_errors::code::ErrCode;
use kagc_errors::diagnostic::Diagnostic;
use kagc_errors::diagnostic::Severity;
use kagc_mir::LabelId;
use kagc_symbol::function::FunctionInfo;
use kagc_types::builtins::obj::KObjType;

use crate::fn_ctx::FnCtx;
use crate::typedefs::CGExprEvalRes;
use crate::typedefs::CGRes;

type ExprLoweringResult = Result<IRValueId, Diagnostic>;

type StmtLoweringResult = Result<(), Diagnostic>;

pub struct IRLowerer {
    ctx: Rc<RefCell<CompilerCtx>>,

    // label ID tracker
    label_id: LabelId,

    /// Current function that is being parsed
    current_function: Option<FunctionInfo>,
    
    early_return_label_id: Option<usize>,

    pub ir_builder: IRBuilder
}

impl IRLowerer {
    pub fn new(ctx: Rc<RefCell<CompilerCtx>>) -> Self {
        Self {
            ctx,
            label_id: 0,
            early_return_label_id: None,
            current_function: None,
            ir_builder: IRBuilder::default()
        }
    }

    pub fn gen_ir(&mut self, nodes: &mut [AST]) -> Vec<IR> {
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
                self.ctx.borrow_mut().diagnostics.push(e);
            }
        }
        output
    }

    pub fn lower_irs(&mut self, nodes: &mut [AST]) {
        for node in nodes {
            self.lower_ir_node(node, &mut FnCtx::new(0));
        }
    }

    pub fn lower_ir_node(&mut self, node: &mut AST, fn_ctx: &mut FnCtx) -> StmtLoweringResult {
        if node.operation == ASTOperation::AST_GLUE {
            if let Some(left_tree) = node.left.as_mut() {
                return self.lower_ir_node(left_tree, fn_ctx);
            }
            if let Some(right_tree) = node.right.as_mut() {
                return self.lower_ir_node(right_tree, fn_ctx);
            }
        }
        else if node.operation == ASTOperation::AST_FUNCTION {
            return self.lower_function(node);
        }
        else if node.operation == ASTOperation::AST_VAR_DECL {
            return self.lower_variable_declaration(node, fn_ctx);
        }
        else if node.operation == ASTOperation::AST_RETURN {
            return self.lower_return(node, fn_ctx);
        }
        else if node.operation == ASTOperation::AST_IMPORT {
            return self.lower_import();
        }
        else if node.operation == ASTOperation::AST_RECORD_DECL {
            return self.lower_record_declaration(node);
        }
        else if node.operation == ASTOperation::AST_LOOP {
            return self.lower_infinite_loop(node, fn_ctx);
        }
        else if node.operation == ASTOperation::AST_IF {
            return self.lower_if_else(node, fn_ctx);
        }
        todo!("{node:#?}");
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

        let mut record_entry = None;
        if let Some(rec_entry) = self.ctx.borrow().const_pool.get(rec_creation.pool_idx) {
            record_entry = Some(rec_entry.clone());
        }

        if let Some(entry) = record_entry {
            match entry.value {
                KagcConst::Record(record_const) => {
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
                size: REG_SIZE_8,
                temp: fn_ctx.next_temp()
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

    fn lower_load_string(&mut self, idx: usize, fn_ctx: &mut FnCtx) -> ExprLoweringResult {
        self.lower_load_heap_allocated_value(idx, fn_ctx)
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

    fn gen_ir_fn(&mut self, ast: &mut AST) -> CGRes {
        let (func_id, func_scope): (usize, usize) = if let Some(Stmt::FuncDecl(func_decl)) = &ast.kind.as_stmt() {
            self.ctx.borrow_mut().scope.enter_scope(func_decl.scope_id);
            (func_decl.func_id, func_decl.scope_id)
        } else {
            panic!("Expected FuncStmt but found {:?}", ast);
        };

        let func_name: String = self.get_func_name(func_id).expect("Function name error!");
        if let Some(finfo) = self.ctx.borrow().scope.lookup_fn_by_name(func_name.as_str()) {
            self.current_function = Some(finfo.clone());
        }

        let func_info: &FunctionInfo = self.current_function.as_ref().unwrap();
        if func_info.storage_class == StorageClass::EXTERN {
            return Ok(
                vec![IR::Func(
                    IRFunc { 
                        name: func_name, 
                        params: vec![], 
                        body: vec![],
                        class: func_info.storage_class,
                        is_leaf: true,
                        scope_id: self.ctx.borrow().scope.live_scope_id(),
                        id: func_id
                    }
                )]
            );
        }

        let store_class: StorageClass = func_info.storage_class;
        let mut virtual_reg: usize = 0;

        let mut fn_body: Vec<IR> = vec![];

        // Setting up function's context
        let mut fn_ctx: FnCtx = FnCtx::new(self.label_id);

        // Collect function parameters and map each of them to a 
        // parameter register based on the Aarch64 ABI
        let params: Vec<IROperand> = self.ctx
            .borrow()
            .scope
            .collect_params(func_scope)
            .iter()
            .map(|&sym| {
                let reg_sz = sym.lit_type.to_reg_size();
                assert_ne!(reg_sz, 0);

                let store_param = self.gen_store_param_ir(
                    sym, 
                    virtual_reg, 
                    &mut fn_ctx
                );
                fn_body.extend(store_param.ok().unwrap());

                let reg: IROperand = IROperand::Param { 
                    position: virtual_reg, 
                    size: reg_sz 
                };
                virtual_reg += 1;
                reg
            }).collect();

        let linearized_body = ast.left.as_mut().unwrap().linearize_mut();
        for body_ast  in linearized_body {
            let body_ir: Vec<IR> = self.gen_ir_from_node(body_ast, &mut fn_ctx, ASTOperation::AST_FUNCTION)?;
            fn_body.extend(body_ir);
        }

        if let Some(early_ret) = self.early_return_label_id {
            fn_body.push(IR::Label(IRLabel(early_ret)));
            self.early_return_label_id = None;
        }

        // get out of function
        self.current_function = None;

        // exit function's scope
        let func_scope: usize = self.ctx.borrow_mut().scope.exit_scope();
        
        self.label_id = fn_ctx.next_label;
        let calls_fns = ast.contains_operation(ASTOperation::AST_FUNC_CALL);
        let allocs_mem = contains_ops!(ast, ASTOperation::AST_RECORD_CREATE, ASTOperation::AST_STRLIT);

        Ok(
            vec![IR::Func(
                IRFunc { 
                    name: func_name, 
                    params, 
                    body: fn_body,
                    class: store_class,
                    is_leaf: !calls_fns && !allocs_mem,
                    scope_id: func_scope,
                    id: func_id 
                }
            )]
        )
    }

    fn lower_function(&mut self, ast: &mut AST) -> StmtLoweringResult {
        let (func_id, func_scope): (usize, usize) = if let Some(Stmt::FuncDecl(func_decl)) = &ast.kind.as_stmt() {
            self.ctx.borrow_mut().scope.enter_scope(func_decl.scope_id);
            (func_decl.func_id, func_decl.scope_id)
        } else {
            panic!("Expected FuncStmt but found {:?}", ast);
        };

        let func_name: String = self.get_func_name(func_id).expect("Function name error!");
        if let Some(finfo) = self.ctx.borrow().scope.lookup_fn_by_name(func_name.as_str()) {
            self.current_function = Some(finfo.clone());
        }

        let mut fn_ctx: FnCtx = FnCtx::new(self.label_id);

        let func_ir_params = self.ctx
            .borrow()
            .scope
            .collect_params(func_scope)
            .iter()
            .map(|&sym| {
                FunctionParam {
                    id: fn_ctx.next_value_id(),
                    ty: IRType::from(sym.lit_type.clone())
                }
            }).collect::<Vec<FunctionParam>>();

        let (_, _) = self.ir_builder.create_function(
            func_ir_params,
            IRType::from(ast.result_type.clone())
        );

        let linearized_body = ast.left.as_mut().unwrap().linearize_mut();
        for body_ast in linearized_body {
            self.lower_ir_node(body_ast, &mut fn_ctx)?;
        }

        self.current_function = None;
        // exit function's scope
        self.ctx.borrow_mut().scope.exit_scope();
        self.label_id = fn_ctx.next_label;
        Ok(())
    }

    fn gen_store_param_ir(
        &self, 
        sym: &Symbol, 
        arg_pos: usize, 
        fn_ctx: &mut FnCtx
    ) -> CGRes {
        let mut output = vec![];
        let reg_sz = sym.lit_type.to_reg_size();
        assert_ne!(reg_sz, 0);

        let store_arg_value = IROperand::Param { 
            position: arg_pos, 
            size: reg_sz 
        };

        let param_off = fn_ctx.next_stack_off();
        let store_param_into_stack = IRInstr::Store { 
            src: store_arg_value.clone(),
            addr: IRAddr::StackOff(param_off)
        };
        output.push(IR::Instr(store_param_into_stack));

        if sym.lit_type.is_gc_alloced() {
            let load_data_off = IRInstr::Load { 
                dest: IROperand::Temp { 
                    id: fn_ctx.next_temp(), 
                    size: REG_SIZE_8 
                }, 
                addr: IRAddr::BaseOff(
                    store_arg_value, 
                    GCOBJECT_BUFFER_IDX as i32
                ) 
            };
            let data_off = fn_ctx.next_stack_off();
            let store_data_off_into_stack = IRInstr::Store { 
                src: load_data_off.dest().unwrap(), 
                addr: IRAddr::StackOff(data_off)
            };
            output.push(IR::Instr(load_data_off));
            output.push(IR::Instr(store_data_off_into_stack));
        }
        fn_ctx.var_offsets.insert(sym.name.clone(), param_off);
        Ok(output)
    }

    fn gen_ir_var_decl(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> CGRes {
        let var_decl = ast.kind.as_stmt().unwrap_or_else(|| panic!("Requires a VarDeclStmt"));
        if let Stmt::VarDecl(var_decl) = var_decl {
            if ast.left.is_none() {
                panic!("Variable is not assigned a value!");
            }

            // set parent ast kind as var decl
            fn_ctx.change_parent_ast_kind(ASTOperation::AST_VAR_DECL);

            let var_decl_val: Vec<IRInstr> = self.gen_ir_expr(ast.left.as_mut().unwrap(), fn_ctx, None)?;
            let last_instr = var_decl_val.last().unwrap().clone();

            if last_instr.dest().is_none() {
                return Err(
                    Diagnostic {
                        code: Some(ErrCode::TYP2103),
                        severity: Severity::Error,
                        primary_span: ast.left.as_ref().unwrap().meta.span,
                        secondary_spans: vec![],
                        message: "'void' type cannot be assigned to a variable".to_string(),
                        notes: vec![]
                    }
                );
            }

            fn_ctx.reset_parent_ast_kind();

            let mut result = var_decl_val.into_iter().map(|instr| {
                IR::Instr(instr)
            }).collect::<Vec<IR>>();

            let var_stack_off = fn_ctx.next_stack_off();
            let store_var_ir = IR::Instr(
                IRInstr::Store { 
                    src: last_instr.dest().unwrap(), 
                    addr: IRAddr::StackOff(var_stack_off)
                }
            );
            result.push(store_var_ir);

            if ast.result_type.is_gc_alloced() {
                let load_gc_alloced_obj_data = IRInstr::Load { 
                    dest: IROperand::Temp { 
                        id: fn_ctx.next_temp(),
                        size: REG_SIZE_8 
                    }, 
                    addr: IRAddr::BaseOff(
                        last_instr.dest().unwrap(), 
                        GCOBJECT_BUFFER_IDX as i32
                    )
                };

                let store_gc_alloc_data_ptr = IRInstr::Store { 
                    src: load_gc_alloced_obj_data.dest().unwrap(), 
                    addr: IRAddr::StackOff(fn_ctx.next_stack_off())
                }; 
                result.push(IR::Instr(load_gc_alloced_obj_data));
                result.push(IR::Instr(store_gc_alloc_data_ptr));
            }

            // remember the variable's stack offset
            fn_ctx.var_offsets.insert(var_decl.sym_name.clone(), var_stack_off);
            return Ok(result);
        }
        // FIND A BETTER SOLUTION THAN JUST PANICKING
        panic!()
    }

    fn lower_variable_declaration(
        &mut self, 
        var_ast: &mut AST, 
        fn_ctx: &mut FnCtx
    ) -> StmtLoweringResult {
        let var_decl = var_ast.kind.as_stmt().unwrap_or_else(|| panic!("Requires a VarDeclStmt"));
        if let Stmt::VarDecl(var_decl) = var_decl {
            if var_ast.left.is_none() {
                panic!("Variable is not assigned a value!");
            }

            fn_ctx.change_parent_ast_kind(ASTOperation::AST_VAR_DECL);
            let var_decl_value = self.lower_expression_ast(var_ast.left.as_mut().unwrap(), fn_ctx)?;

            let var_stack_off = fn_ctx.next_stack_off();
            self.ir_builder.inst(
                IRInstruction::Store { 
                    src: IRValue::Var(var_decl_value), 
                    address: IRAddress::StackOffset(var_stack_off) 
                }
            );

            if var_ast.result_type.is_gc_alloced() {
                let data_ptr_value_id = fn_ctx.next_value_id();
                self.ir_builder.inst(
                    IRInstruction::Load { 
                        src: IRAddress::BaseOffset(var_decl_value, GCOBJECT_BUFFER_IDX), 
                        result: data_ptr_value_id
                    }
                );
                self.ir_builder.inst(
                    IRInstruction::Store { 
                        src: IRValue::Var(data_ptr_value_id), 
                        address: IRAddress::StackOffset(fn_ctx.next_stack_off())
                    }
                );
            }

            fn_ctx.var_offsets.insert(var_decl.sym_name.clone(), var_stack_off);
            Ok(())
        }
        else {
            panic!()
        }
    }

    fn gen_ident_ir_expr(&mut self, ident_expr: &IdentExpr, fn_ctx: &mut FnCtx, dest: Option<IROperand>) -> CGExprEvalRes {
        let sym: Symbol = self.get_symbol_local_or_global(&ident_expr.sym_name).unwrap();
        let sym_off = *fn_ctx
            .var_offsets
            .get(&ident_expr.sym_name)
            .unwrap_or_else(|| panic!("Undefined symbol bug. This mustn't be happening. Aborting..."));

        let reg_sz: RegSize = sym.lit_type.to_reg_size();
        assert_ne!(reg_sz, 0);

        let dest = dest.unwrap_or(IROperand::Temp {
            id: fn_ctx.next_temp(),
            size: reg_sz,
        });

        Ok(vec![
            IRInstr::Load {
                dest,
                addr: IRAddr::StackOff(sym_off)
            }
        ])
    }

    fn gen_ir_fn_call_expr(
        &mut self, 
        func_call_expr: &mut FuncCallExpr, 
        fn_ctx: &mut FnCtx, 
        dest: Option<IROperand>
    ) -> CGExprEvalRes {
        let mut func_call_instrs = vec![];
        let mut actual_params = vec![];

        let num_args = func_call_expr.args.len();
        let mut last_instrs = vec![];

        for (rev_idx, (_, param_expr)) in func_call_expr.args.iter_mut().rev().enumerate() {
            let rev_param_position = num_args - 1 - rev_idx;
            // let current_tmp = fn_ctx.temp_counter;
            // let param_temp = fn_ctx.next_temp();
            let param_eval_instrs = self.__gen_expr(
                param_expr, 
                fn_ctx, 
                None
                // Some(IROperand::CallArg {
                    // position: rev_param_position,
                    // size: REG_SIZE_8,
                    // temp: param_temp
                // })
            )?;

            let last_instr = param_eval_instrs.last().unwrap().clone();
            last_instrs.push((last_instr, param_expr.result_type()));

            // let new_temp = fn_ctx.temp_counter;
            // let tmp_reg_loc = current_tmp + (new_temp - current_tmp) - 1;

            let value_size = param_expr.result_type().to_reg_size();
            assert_ne!(value_size, 0);

            func_call_instrs.extend(param_eval_instrs);
            // actual_params.push(
            //     (
            //         rev_param_position, 
            //         IROperand::CallArg { 
            //             temp: tmp_reg_loc, 
            //             size: value_size,
            //             position: rev_param_position
            //         }
            //     )
            // );
        }

        for (param_position , last_instr) in last_instrs.iter().rev().enumerate() {
            func_call_instrs.push(
                IRInstr::Mov {
                    dest: IROperand::CallArg { 
                        temp: fn_ctx.next_temp(), 
                        position: param_position, 
                        size: last_instr.1.to_reg_size() 
                    }, 
                    src: last_instr.0.dest().unwrap()
                }
            );
            let param = func_call_instrs.last().unwrap();
            actual_params.push((param_position, param.dest().unwrap().clone()));
        }

        func_call_instrs.push(IRInstr::Call {
            fn_name: func_call_expr.symbol_name.clone(),
            params: actual_params,
            dest: Some(IROperand::CallValue { 
                temp: fn_ctx.next_temp(), 
                size: REG_SIZE_8 
            })
        });
        Ok(func_call_instrs)
    }

    fn lower_expression_ast(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> ExprLoweringResult {
        if !ast.kind.is_expr() {
            panic!("Needed an Expr--but found {ast:#?}");
        }
        let expr = ast
            .kind
            .as_expr_mut()
            .unwrap_or_else(|| panic!("Cannot unwrap an expression for some reason. Aborting..."));
        self.lower_expression(expr, fn_ctx)
    }

    fn lower_expression(&mut self, expr: &mut Expr, fn_ctx: &mut FnCtx) -> ExprLoweringResult {
        match expr {
            Expr::LitVal(lit_expr) =>           self.lower_literal_value_expr(lit_expr, fn_ctx),
            Expr::Ident(ident_expr) =>           self.lower_identifier_expr(ident_expr, fn_ctx),
            Expr::Binary(bin_expr) =>              self.lower_binary_expr(bin_expr, fn_ctx),
            Expr::FuncCall(func_call_expr) => self.lower_function_call_expr(func_call_expr, fn_ctx),
            Expr::RecordFieldAccess(rec_field_access) => self.lower_record_field_access(rec_field_access, fn_ctx),
            _ => unimplemented!()
        }
    }

    fn lower_function_call_expr(&mut self, func_call_expr: &mut FuncCallExpr, fn_ctx: &mut FnCtx) -> ExprLoweringResult {
        let mut func_call_args = vec![];
        for (_, arg_expr) in &mut func_call_expr.args {
            let arg_value_id = self.lower_expression(arg_expr, fn_ctx)?;
            func_call_args.push(arg_value_id);
        }

        let call_result_value = fn_ctx.next_value_id();
        self.ir_builder.inst(
            IRInstruction::Call { 
                func: func_call_expr.symbol_name.clone(), 
                args: func_call_args, 
                result: Some(call_result_value)
            }
        );
        Ok(call_result_value)
    }

    fn lower_literal_value_expr(&mut self, lit_expr: &LitValExpr, fn_ctx: &mut FnCtx) -> ExprLoweringResult {
        if let LitType::PoolStr(pool_idx) = &lit_expr.value  {
            return self.lower_load_string(*pool_idx, fn_ctx);
        }

        match lit_expr.result_type {
            LitTypeVariant::I64 |
            LitTypeVariant::U8 => {
                let const_value = *lit_expr.value.unwrap_u8().expect("No u8 value!") as i64;
                Ok(self.ir_builder.create_move(IRValue::Constant(const_value)))
            },

            _ => unimplemented!("{lit_expr:#?}")
        }
    }

    fn lower_identifier_expr(&mut self, ident_expr: &IdentExpr, fn_ctx: &mut FnCtx) -> ExprLoweringResult {
        let sym: Symbol = self.get_symbol_local_or_global(&ident_expr.sym_name).unwrap();
        let sym_off = *fn_ctx
            .var_offsets
            .get(&ident_expr.sym_name)
            .unwrap_or_else(|| panic!("Undefined symbol bug. This mustn't be happening. Aborting..."));

        let reg_sz: RegSize = sym.lit_type.to_reg_size();
        assert_ne!(reg_sz, 0);

        let value_id = fn_ctx.next_value_id();
        self.ir_builder.inst(
            IRInstruction::Load { 
                src: IRAddress::StackOffset(sym_off), 
                result: value_id 
            }
        );
        Ok(value_id)
    }

    fn lower_binary_expr(&mut self, bin_expr: &mut BinExpr, fn_ctx: &mut FnCtx) -> ExprLoweringResult {
        let lhs_value_id = self.lower_expression(&mut bin_expr.left, fn_ctx)?;
        let rhs_value_id = self.lower_expression(&mut bin_expr.right, fn_ctx)?;
        match bin_expr.operation {
            ASTOperation::AST_ADD       => Ok(self.ir_builder.create_add(IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            ASTOperation::AST_SUBTRACT  => Ok(self.ir_builder.create_subtract(IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            ASTOperation::AST_MULTIPLY  => Ok(self.ir_builder.create_divide(IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            ASTOperation::AST_DIVIDE    => Ok(self.ir_builder.create_multiply(IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            ASTOperation::AST_EQEQ      => Ok(self.ir_builder.create_conditional_jump(IRCondition::EqEq, IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            ASTOperation::AST_NEQ       => Ok(self.ir_builder.create_conditional_jump(IRCondition::NEq, IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            ASTOperation::AST_LTEQ      => Ok(self.ir_builder.create_conditional_jump(IRCondition::LTEq, IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            ASTOperation::AST_GTEQ      => Ok(self.ir_builder.create_conditional_jump(IRCondition::GTEq, IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            ASTOperation::AST_LTHAN     => Ok(self.ir_builder.create_conditional_jump(IRCondition::LThan, IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            ASTOperation::AST_GTHAN     => Ok(self.ir_builder.create_conditional_jump(IRCondition::GThan, IRValue::Var(lhs_value_id), IRValue::Var(rhs_value_id))),
            _ => unimplemented!()
        }
    }

    fn gen_ir_return(&mut self, ret_stmt: &mut AST, fn_ctx: &mut FnCtx) -> CGRes {
        if let Some(Stmt::Return(_)) = &ret_stmt.kind.as_stmt() {
            let mut ret_instrs: Vec<IR> = vec![]; 

            // always check function's existence
            if let Some(curr_fn) = &self.current_function {
                // if the function is non-void type
                if !curr_fn.return_type.is_void() {
                    let reg_sz = curr_fn.return_type.to_reg_size();
                    assert_ne!(reg_sz, 0);
                    let ret_stmt_instrs = self.gen_ir_expr(
                        ret_stmt.left.as_mut().unwrap(), 
                        fn_ctx, 
                        None
                    )?;
                    ret_stmt_instrs.iter().for_each(|instr| ret_instrs.push(IR::Instr(instr.clone())));
                }
            }

            let early_ret_instrs = if fn_ctx.early_return {
                // turn off early return ASAP
                fn_ctx.early_return = false;
                let label_id = self.early_return_label_id.unwrap_or_else(|| {
                    let next_id: usize = fn_ctx.get_next_label();
                    self.early_return_label_id = Some(next_id);
                    next_id
                });
                vec![
                    IR::Instr(IRInstr::Jump { label_id })
                ]
            }
            else {
                vec![]
            };

            ret_instrs.extend(early_ret_instrs);
            return Ok(ret_instrs);
        }
        panic!("Expected ReturnStmt but found {:?}", ret_stmt);
    }

    fn lower_return(&mut self, ret_stmt: &mut AST, fn_ctx: &mut FnCtx) -> StmtLoweringResult {
        if let Some(Stmt::Return(_)) = &ret_stmt.kind.as_stmt() {
            if let Some(curr_fn) = &self.current_function {
                let curr_block = self.ir_builder.current_block_unchecked();
                if !curr_fn.return_type.is_void() {
                    let return_value_id = self.lower_expression_ast(
                        ret_stmt.left.as_mut().unwrap(), 
                        fn_ctx
                    )?;
                    self.ir_builder.set_terminator(curr_block, Terminator::Return(Some(return_value_id)));
                }
                else {
                    self.ir_builder.set_terminator(curr_block, Terminator::Return(None));
                }
                return Ok(());
            }
            else {
                panic!("Detected ReturnStmt outside a function! Aborting...");
            }
        }
        panic!("Expected ReturnStmt but found {ret_stmt:#?}");
    }

    fn gen_ir_loop(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> CGRes {
        let label_start: usize = fn_ctx.get_next_label();
        let label_end: usize = fn_ctx.get_next_label();

        let mut loop_body: Vec<IR> = vec![];
        loop_body.extend(self.gen_ir_label(label_start)?);
        let linearized_body: Vec<&mut AST> = ast.left.as_mut().unwrap().linearize_mut();

        for body_ast  in linearized_body {
            let body_ir: Vec<IR> = self.gen_ir_from_node(body_ast, fn_ctx, ASTOperation::AST_LOOP)?;
            loop_body.extend(body_ir);
        }

        loop_body.extend(self.gen_ir_jump(label_start)?);
        loop_body.extend(self.gen_ir_label(label_end)?);
        Ok(loop_body)
    }

    fn lower_infinite_loop(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> StmtLoweringResult {
        let prev_block_id = self.ir_builder.current_block_unchecked();
        let loop_header_id = self.ir_builder.create_block("loop-header");

        self.ir_builder.set_terminator(prev_block_id, Terminator::Jump(loop_header_id));
        self.ir_builder.link_blocks(prev_block_id, loop_header_id);

        let loop_body_id = self.ir_builder.create_block("loop_body");
        self.ir_builder.set_terminator(loop_header_id, Terminator::Jump(loop_body_id));
        self.ir_builder.link_blocks(loop_header_id, loop_body_id);

        let linearized_body = ast.left.as_mut().unwrap().linearize_mut();
        for body_ast in linearized_body {
            self.lower_ir_node(body_ast, fn_ctx)?;
        }

        self.ir_builder.set_terminator(loop_body_id, Terminator::Jump(loop_header_id));
        Ok(())
    }

    fn lower_if_else(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> StmtLoweringResult {
        if let ASTKind::StmtAST(Stmt::If(if_stmt)) = &ast.kind {
            self.ctx.borrow_mut().scope.enter_scope(if_stmt.scope_id);
        }
        let prev_block_id = self.ir_builder.current_block_unchecked();

        // Evaluate the condition and store the resilt in a register.
        // Every if-else must have the `left` branch set in the main if-else AST tree.
        let if_entry_block = self.ir_builder.create_block("if-header");
        self.ir_builder.link_blocks(prev_block_id, if_entry_block);
        
        // lower the condition expression in the entry block
        let if_stmt_cond_value = self.lower_expression_ast(ast.left.as_mut().unwrap(), fn_ctx)?;

        // then-branch
        let then_block = self.ir_builder.create_block("then");
        // else-branch 
        let else_block = self.ir_builder.create_block("else");
        self.ir_builder.link_blocks_multiple(if_entry_block, vec![then_block, else_block]);

        // join branch
        let merge_block = self.ir_builder.create_block("merge");
        self.ir_builder.link_blocks(then_block, merge_block);
        self.ir_builder.link_blocks(else_block, merge_block);

        self.ir_builder.set_terminator(
            if_entry_block, 
            Terminator::CondJump { 
                cond: if_stmt_cond_value, 
                then_block, 
                else_block
            }
        );

        {
            // lower the if-block
            self.ir_builder.switch_to_block(then_block);
            let linearized_body = ast.mid.as_mut().unwrap().linearize_mut();
            for body_ast  in linearized_body {
                self.lower_ir_node(body_ast, fn_ctx)?;
            }

            // jump to merge block after
            self.ir_builder.set_terminator(then_block, Terminator::Jump(merge_block));
            // switch to the merge block afte emitting if-branch's code
            self.ir_builder.switch_to_block(merge_block);

            // end `if`'s scope
            self.ctx.borrow_mut().scope.exit_scope();
        }
        Ok(())
    }

    fn gen_ir_if(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> CGRes {
        if let ASTKind::StmtAST(Stmt::If(if_stmt)) = &ast.kind {
            self.ctx.borrow_mut().scope.enter_scope(if_stmt.scope_id);
        }

        // Label for jumping to the 'else' block if the condition is false
        let label_if_false: usize = fn_ctx.get_next_label();

        // Label marking the end of the entire if-else block
        let label_end: usize = fn_ctx.get_next_label();

        let mut output: Vec<IR> = vec![];

        // change the parent AST kind to AST_IF
        fn_ctx.change_parent_ast_kind(ASTOperation::AST_IF);
        fn_ctx.change_label_hint(label_if_false);

        // Evaluate the condition and store the resilt in a register.
        // Every if-else must have the `left` branch set in the main if-else AST tree.
        let cond_result: Vec<IRInstr> = self.gen_ir_expr(ast.left.as_mut().unwrap(), fn_ctx, None)?;
        
        fn_ctx.reset_label_hint();

        for cond_res_ir in cond_result {
            output.push(IR::Instr(cond_res_ir));
        }

        fn_ctx.reset_parent_ast_kind();

        let linearized_body: Vec<&mut AST> = ast.mid.as_mut().unwrap().linearize_mut();
        for body_ast  in linearized_body {
            let body_ir: Vec<IR> = self.gen_ir_from_node(body_ast, fn_ctx, ASTOperation::AST_IF)?;
            output.extend(body_ir);
        }

        if ast.right.is_some() {
            output.extend(self.gen_ir_jump(label_end)?);
        }

        output.extend(self.gen_ir_label(label_if_false)?);

        // end `if` scope
        self.ctx.borrow_mut().scope.exit_scope();

        // else block
        if let Some(right_ast) = ast.right.as_mut() {
            let else_block: Vec<IR> = self.gen_ir_from_node(right_ast, fn_ctx, ASTOperation::AST_GLUE)?;
            output.extend(else_block);
            output.extend(self.gen_ir_label(label_end)?);
        }

        Ok(output)
    }

    fn lower_rec_field_access_to_ir(
        &mut self, 
        access: &mut RecordFieldAccessExpr, 
        fn_ctx: &mut FnCtx, 
        dest: Option<IROperand>
    ) -> CGExprEvalRes {
        let reg_sz = access.result_type.to_reg_size();
        assert_ne!(reg_sz, 0);

        // The record's stack offset
        let rec_stack_off = *fn_ctx
            .var_offsets
            .get(&access.rec_alias)
            .unwrap_or_else(|| panic!("what the fck bro"));

        let load_data_ptr = IRInstr::Load { 
            dest: IROperand::Temp { 
                id: fn_ctx.next_temp(), 
                size: REG_SIZE_8 
            }, 
            addr: IRAddr::StackOff(rec_stack_off + 1)
        };

        let dest = dest.unwrap_or(IROperand::Temp {
            id: fn_ctx.next_temp(),
            size: reg_sz,
        });

        let load_value = IRInstr::Load { 
            dest, 
            addr: IRAddr::BaseOff(
                load_data_ptr.dest().unwrap(), 
                access.rel_stack_off as i32
            )
        };
        Ok(vec![
            load_data_ptr,
            load_value
        ])
    }

    fn lower_record_field_access(
        &mut self,
        access: &mut RecordFieldAccessExpr,
        fn_ctx: &mut FnCtx
    ) -> ExprLoweringResult {
        let reg_sz = access.result_type.to_reg_size();
        assert_ne!(reg_sz, 0);

        // The record's stack offset
        let rec_stack_off = *fn_ctx
            .var_offsets
            .get(&access.rec_alias)
            .unwrap_or_else(|| panic!("what the fck bro"));

        let base_pointer_value = fn_ctx.next_value_id();
        self.ir_builder.inst(
            IRInstruction::Load { 
                src: IRAddress::StackOffset(rec_stack_off + 1), 
                result: base_pointer_value 
            }
        );

        let field_value = fn_ctx.next_value_id();
        self.ir_builder.inst(
            IRInstruction::Load { 
                src: IRAddress::BaseOffset(
                    base_pointer_value, 
                    access.rel_stack_off
                ), 
                result: field_value
            }
        );
        Ok(field_value)
    }

    fn gen_ir_load_global_var(&mut self, idx: usize, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        let ctx_borrow = self.ctx.borrow();
        let obj = ctx_borrow.const_pool.get(idx);
        if obj.is_none() {
            panic!("ConstEntry not found! Panic caused by the index: {idx}");
        }

        let obj = obj.unwrap();
        let ob_size = self.ctx.borrow().const_pool.size(idx).unwrap();
        let mut output = vec![];

        // allocate memory for the global var
        let gc_alloc = IRInstr::MemAlloc { 
            size: ob_size,
            ob_type: obj.ob_type.clone(),
            dest: IROperand::CallValue { 
                size: REG_SIZE_8,
                temp: fn_ctx.next_temp()
            }
        };
        let gc_obj_dest = gc_alloc.dest().unwrap();

        // STR instruction's stack offset
        let store_off = fn_ctx.next_stack_off();

        // store the allocated memory's address on the stack
        let store_mem_addr = IRInstr::Store { 
            src: gc_obj_dest.clone(), 
            addr: IRAddr::StackOff(store_off)
        };

        output.push(gc_alloc);
        output.push(store_mem_addr);

        /* 
            Offset the destination register by 32 bytes. But why 32? Let me explain.

            The C struct `gc_object_t` is laid out in memory as follows (on a 64-bit system):
                - ref_count      (8 bytes)  -> offset 0
                - size           (8 bytes)  -> offset 8
                - num_children   (8 bytes)  -> offset 16
                - children       (8 bytes)  -> offset 24
                - data           (8 bytes)  -> offset 32

            Since we want to access the `data` field (a pointer to the actual payload),
            we must add an offset of 32 (0x20) to the base pointer returned by `_kgc_alloc`.
        */
        let load_gc_alloced_obj_data = IRInstr::Load { 
            dest: IROperand::Temp { 
                id: fn_ctx.next_temp(),
                size: REG_SIZE_8 
            }, 
            addr: IRAddr::BaseOff(
                gc_obj_dest, 
                GCOBJECT_BUFFER_IDX as i32
            )
        };

        let load_glob_value_from_pool = IRInstr::LoadGlobal { 
            pool_idx: idx, 
            dest: IROperand::Temp{ 
                id: fn_ctx.next_temp(), 
                size: REG_SIZE_8 // always use 8 bytes to load string literals into the stack
            } 
        };

        /*
            Load the destination pointer where the global value 
            is going to be stored.
         */
        let load_param_0_dest = IROperand::CallArg { 
            temp: fn_ctx.next_temp(), 
            position: 0, 
            size: REG_SIZE_8,
        };

        let prepare_dst_ptr = IRInstr::Mov { 
            dest: load_param_0_dest,
            src: load_gc_alloced_obj_data.dest().unwrap(),
        };

        /*
            Source pointer of the global variable. Initially, 
            the value is located at .rodata section of the executable.
         */
        let load_param_1_src = IROperand::CallArg { 
            temp: fn_ctx.next_temp(), 
            position: 1, 
            size: REG_SIZE_8,
        };
        let prepare_src_ptr = IRInstr::Mov { 
            dest: load_param_1_src,
            src: load_glob_value_from_pool.dest().unwrap(),
        };

        /*
            Size of the global value.
         */
        let load_param_2_size = IROperand::CallArg { 
            temp: fn_ctx.next_temp(), 
            position: 2, 
            size: REG_SIZE_8,
        };
        let prepare_size_ptr = IRInstr::Mov { 
            dest: load_param_2_size,
            src: IROperand::Const(IRImmVal::Int64(ob_size as i64)),
        };

        // generate code to move value from .rodata to heap
        let move_to_heap = IRInstr::MemCpy { 
            dest: load_gc_alloced_obj_data.dest().unwrap()
        };

        output.push(load_gc_alloced_obj_data);
        output.push(load_glob_value_from_pool);
        output.push(prepare_dst_ptr);
        output.push(prepare_src_ptr);
        output.push(prepare_size_ptr);
        output.push(move_to_heap);

        let load_gc_alloced_obj_canon = IRInstr::Load { 
            dest: IROperand::Temp { 
                id: fn_ctx.next_temp(), 
                size: REG_SIZE_8 
            }, 
            addr: IRAddr::StackOff(store_off)
        };

        output.push(load_gc_alloced_obj_canon);
        Ok(output)
    }

    fn lower_load_heap_allocated_value(&mut self, idx: usize, fn_ctx: &mut FnCtx) -> ExprLoweringResult {
        let ctx_borrow = self.ctx.borrow();
        let obj = ctx_borrow.const_pool.get(idx);
        if obj.is_none() {
            panic!("ConstEntry not found! Panic caused by the index: {idx}");
        }

        let obj = obj.unwrap();
        let ob_size = self.ctx.borrow().const_pool.size(idx).unwrap();

        let mem_alloc_value_id = fn_ctx.next_value_id();
        self.ir_builder.inst(
            IRInstruction::MemAlloc {
                ob_ty: obj.ob_type.clone(),
                size: ob_size,
                result: mem_alloc_value_id,
                pool_idx: idx
            }
        );
        Ok(mem_alloc_value_id)
    }

    fn lower_import_to_ir(&self) -> CGRes {
        Ok(vec![])   
    }

    fn lower_import(&mut self) -> StmtLoweringResult {
        Ok(())
    }

    fn lower_rec_decl_to_ir(&mut self, _node: &mut AST) -> CGRes {
        Ok(vec![])
    }

    fn lower_record_declaration(&mut self, _node: &mut AST) -> StmtLoweringResult {
        Ok(())
    }

    fn gen_ir_jump(&self, label_id: LabelId) -> CGRes {
        Ok(vec![
            IR::Instr(
                IRInstr::Jump { label_id }
            )
        ])
    }

    fn gen_ir_label(&self, label_id: LabelId) -> CGRes {
        Ok(
            vec![IR::Label(IRLabel(label_id))]
        )
    }

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

    fn get_func_name(&mut self, index: usize) -> Option<String> {
        let ctx_borrow = self.ctx.borrow();
        ctx_borrow.scope.lookup_fn(index).map(|func| func.name.clone())
    }

    fn get_symbol_local_or_global(&self, sym_name: &str) -> Option<Symbol> {
        let ctx_borrow = self.ctx.borrow();
        ctx_borrow.scope.deep_lookup(sym_name).cloned()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_loop_stmt_cfg_construction() {
    }
}