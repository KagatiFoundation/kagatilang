// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use core::panic;
use std::cell::RefCell;
use std::rc::Rc;
use std::vec;

use kagc_ast::*;
use kagc_ctx::*;
use kagc_errors::code::ErrCode;
use kagc_errors::diagnostic::Diagnostic;
use kagc_errors::diagnostic::Severity;
use kagc_ir::gc::GCOBJECT_BUFFER_IDX;
use kagc_ir::ir_instr::*;
use kagc_ir::ir_instr::IR;
use kagc_ir::ir_types::IRValueType;
use kagc_ir::ir_types::IRImmVal;
use kagc_ir::ir_types::TempId;
use kagc_ir::LabelId;
use kagc_symbol::function::FunctionInfo;
use kagc_symbol::*;
use kagc_target::reg::*;
use kagc_types::*;
use kagc_utils::integer::*;

use crate::fn_ctx::FnCtx;
use crate::typedefs::CGExprEvalRes;
use crate::typedefs::CGRes;
use crate::IRGen;

pub struct Aarch64IRGen {
    ctx: Rc<RefCell<CompilerCtx>>,

    // label ID tracker
    label_id: LabelId,

    /// Current function that is being parsed
    current_function: Option<FunctionInfo>,
    
    early_return_label_id: Option<usize>,
}

impl Aarch64IRGen {
    fn gen_store_param_ir(
        &self, 
        param_tmp: TempId,
        sym: &Symbol, 
        virtual_reg: RegIdx, 
        fn_ctx: &mut FnCtx
    ) -> CGRes {
        let mut output = vec![];
        let reg_sz = sym.lit_type.to_reg_size();
        assert_ne!(reg_sz, 0);

        let param_off = fn_ctx.next_stack_off();
        let alloc_param_reg = IRInstr::RegAlloc {
            dest: IRValueType::Reg {
                temp: param_tmp, 
                idx: virtual_reg, 
                size: REG_SIZE_8 
            },
            idx: virtual_reg,
            size: REG_SIZE_8
        };

        let store_param_into_stack = IRInstr::Store { 
            src: alloc_param_reg.dest().unwrap(),
            addr: IRAddr::StackOff(param_off)
        };
        output.push(IR::Instr(store_param_into_stack));

        if sym.lit_type.is_gc_alloced() {
            let load_data_off = IRInstr::Load { 
                dest: IRValueType::ExtendedTemp { 
                    id: fn_ctx.next_temp(), 
                    size: REG_SIZE_8 
                }, 
                addr: IRAddr::BaseOff(
                    alloc_param_reg.dest().unwrap(), 
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
}

impl IRGen for Aarch64IRGen {        
    fn ctx(&self) -> Rc<RefCell<CompilerCtx>> {
        self.ctx.clone()
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
        let params: Vec<IRValueType> = self.ctx
            .borrow()
            .scope
            .collect_params(func_scope)
            .iter()
            .map(|&sym| {
                let reg_sz = sym.lit_type.to_reg_size();
                assert_ne!(reg_sz, 0);

                let param_tmp: usize = fn_ctx.next_temp();
                let store_param = self.gen_store_param_ir(
                    param_tmp,
                    sym, 
                    virtual_reg, 
                    &mut fn_ctx
                );
                fn_body.extend(store_param.ok().unwrap());

                let reg: IRValueType = IRValueType::Reg { 
                    temp: param_tmp, 
                    idx: virtual_reg, 
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

    fn gen_ir_var_decl(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> CGRes {
        let var_decl = ast.kind.as_stmt().unwrap_or_else(|| panic!("Requires a VarDeclStmt"));
        if let Stmt::VarDecl(var_decl) = var_decl {
            if ast.left.is_none() {
                panic!("Variable is not assigned a value!");
            }

            // set parent ast kind as var decl
            fn_ctx.change_parent_ast_kind(ASTOperation::AST_VAR_DECL);

            let var_decl_val: Vec<IRInstr> = self.gen_ir_expr(ast.left.as_mut().unwrap(), fn_ctx)?;
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
                    dest: IRValueType::ExtendedTemp { 
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

    fn gen_ident_ir_expr(&mut self, ident_expr: &IdentExpr, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        let sym: Symbol = self.get_symbol_local_or_global(&ident_expr.sym_name).unwrap();
        let sym_off = *fn_ctx
            .var_offsets
            .get(&ident_expr.sym_name)
            .unwrap_or_else(|| panic!("Undefined symbol bug. This mustn't be happening. Aborting..."));

        let reg_sz: RegSize = sym.lit_type.to_reg_size();
        assert_ne!(reg_sz, 0);

        Ok(vec![
            IRInstr::Load {
                dest: IRValueType::ExtendedTemp { 
                    id: fn_ctx.next_temp(),
                    size: reg_sz
                },
                addr: IRAddr::StackOff(sym_off)
            }
        ])
    }

    fn gen_ir_fn_call_expr(&mut self, func_call_expr: &mut FuncCallExpr, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        let use_reg: bool = fn_ctx.force_reg_use;
        let forced_reg: Option<usize> = fn_ctx.reg_counter;

        let mut param_instrs: Vec<IRInstr> = vec![];
        let mut actual_params: Vec<(usize, IRValueType)> = vec![];

        let num_args: usize = func_call_expr.args.len();
        let mut last_instrs: Vec<(IRInstr, LitTypeVariant)> = vec![];

        for (rev_idx, (_, param_expr)) in func_call_expr.args.iter_mut().rev().enumerate() {
            let param_reg_idx = num_args - 1 - rev_idx;
            let current_tmp: usize = fn_ctx.temp_counter;
            let p_instr: Vec<IRInstr> = self.__gen_expr(param_expr, fn_ctx)?;

            let last_instr: IRInstr = p_instr.last().unwrap().clone();
            last_instrs.push((last_instr, param_expr.result_type()));

            let new_temp: usize = fn_ctx.temp_counter;
            let tmp_reg_loc: usize = current_tmp + (new_temp - current_tmp) - 1;

            let reg_sz = param_expr.result_type().to_reg_size();
            assert_ne!(reg_sz, 0);

            param_instrs.extend(p_instr);
            actual_params.push((param_reg_idx, IRValueType::ExtendedTemp{ id: tmp_reg_loc, size: reg_sz }));
        }

        if use_reg {
            fn_ctx.force_reg_use(forced_reg.unwrap());
        }
        else {
            fn_ctx.clear_reg_hint();
        }

        for (rev_idx, last_instr) in last_instrs.iter().rev().enumerate() {
            let param_tmp: usize = fn_ctx.next_temp();
            param_instrs.push(
                IRInstr::Mov {
                    dest: IRValueType::Reg{ 
                        temp: param_tmp, 
                        idx: rev_idx, 
                        size: last_instr.1.to_reg_size() 
                    }, 
                    src: last_instr.0.dest().unwrap()
                }
            );
        }

        let mut preserve_ret_val_instr = None; 
        if !func_call_expr.result_type.is_void() && !func_call_expr.result_type.is_none() {
            let reg_sz = func_call_expr.result_type.to_reg_size();
            assert_ne!(reg_sz, 0);

            preserve_ret_val_instr = Some(IRInstr::Mov {
                dest: IRValueType::ExtendedTemp { 
                    id: fn_ctx.next_temp(), 
                    size: reg_sz 
                },
                src: IRValueType::Reg { 
                    temp: fn_ctx.next_temp(), 
                    idx: 0, 
                    size: reg_sz 
                }
            });
        }

        param_instrs.push(IRInstr::Call {
            fn_name: func_call_expr.symbol_name.clone(),
            params: actual_params,
            return_type: None // fn_call_ret_type
        });

        if let Some(preserve_instr) = preserve_ret_val_instr {
            param_instrs.push(preserve_instr); 
        }

        Ok(param_instrs)
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

                    let ret_stmt_instrs = self.gen_ir_expr(ret_stmt.left.as_mut().unwrap(), fn_ctx)?;

                    // last instruction is basically the temporary which holds the return 
                    // expression's evaluation result
                    let last_instr = ret_stmt_instrs.last().cloned().unwrap();
                    ret_stmt_instrs.iter().for_each(|instr| ret_instrs.push(IR::Instr(instr.clone())));
                    let ret_tmp = fn_ctx.next_temp();
                    
                    ret_instrs.push(
                        IR::Instr(
                            IRInstr::Mov {
                                dest: IRValueType::Reg{ temp: ret_tmp, idx: 0, size: reg_sz },
                                src: last_instr.dest().unwrap()
                            }
                        )
                    );
                }
            }

            let early_ret_instrs = if fn_ctx.early_return {
                // turn off early return ASAP
                fn_ctx.early_return = false;
                let jump_to: usize = if let Some(lbl_id) = self.early_return_label_id {
                    lbl_id
                }
                else {
                    let next_id: usize = fn_ctx.get_next_label();
                    self.early_return_label_id = Some(next_id);
                    next_id
                };

                vec![
                    IR::Instr(IRInstr::Jump { label_id: jump_to })
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
        let cond_result: Vec<IRInstr> = self.gen_ir_expr(ast.left.as_mut().unwrap(), fn_ctx)?;
        
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

    fn lower_rec_field_access_to_ir(&mut self, access: &mut RecordFieldAccessExpr, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        let reg_sz = access.result_type.to_reg_size();
        assert_ne!(reg_sz, 0);

        // The record's stack offset
        let rec_stack_off = *fn_ctx
            .var_offsets
            .get(&access.rec_alias)
            .unwrap_or_else(|| panic!("what the fck bro"));

        let load_data_ptr = IRInstr::Load { 
            dest: IRValueType::ExtendedTemp { 
                id: fn_ctx.next_temp(), 
                size: REG_SIZE_8 
            }, 
            addr: IRAddr::StackOff(rec_stack_off + 1)
        };

        let load_value = IRInstr::Load { 
            dest: IRValueType::ExtendedTemp{ 
                id: fn_ctx.next_temp(), 
                size: reg_sz 
            }, 
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
            dest: IRValueType::Reg { 
                temp: fn_ctx.next_temp(), 
                idx: 0, 
                size: REG_SIZE_8
            }
        };

        // STR instruction's stack offset
        let store_off = fn_ctx.next_stack_off();

        // store the allocated memory's address on the stack
        let store_mem_addr = IRInstr::Store { 
            src: gc_alloc.dest().unwrap(), 
            addr: IRAddr::StackOff(store_off)
        };

        output.push(gc_alloc);
        output.push(store_mem_addr);

        // Load the `data` section from the `gc_object_t` object in the memory.
        // It is at the 32 bytes offset.
        let load_gc_alloced_obj_canon = IRInstr::Load { 
            dest: IRValueType::ExtendedTemp { 
                id: fn_ctx.next_temp(), 
                size: REG_SIZE_8
            }, 
            addr: IRAddr::StackOff(store_off)
        };

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
            dest: IRValueType::ExtendedTemp { 
                id: fn_ctx.next_temp(),
                size: REG_SIZE_8 
            }, 
            addr: IRAddr::BaseOff(
                load_gc_alloced_obj_canon.dest().unwrap(), 
                GCOBJECT_BUFFER_IDX as i32
            )
        };

        output.push(load_gc_alloced_obj_canon);

        let load_glob_value_from_pool = IRInstr::LoadGlobal { 
            pool_idx: idx, 
            dest: IRValueType::ExtendedTemp{ 
                id: fn_ctx.next_temp(), 
                size: REG_SIZE_8 // always use 8 bytes to load string literals into the stack
            } 
        };

        /*
            Load the destination pointer where the global value 
            is going to be stored.
         */
        let x0_reg_temp = IRValueType::Reg { 
            temp: fn_ctx.next_temp(), 
            idx: 0, 
            size: REG_SIZE_8 
        };
        let prepare_dst_ptr = IRInstr::Mov { 
            dest: x0_reg_temp,
            src: load_gc_alloced_obj_data.dest().unwrap(),
        };

        /*
            Source pointer of the global variable. Initially, 
            the value is located at .rodata section of the executable.
         */
        let x1_reg_temp = IRValueType::Reg { 
            temp: fn_ctx.next_temp(), 
            idx: 1, 
            size: REG_SIZE_8 
        };
        let prepare_src_ptr = IRInstr::Mov { 
            dest: x1_reg_temp,
            src: load_glob_value_from_pool.dest().unwrap(),
        };

        /*
            Size of the global value.
         */
        let x2_reg_temp = IRValueType::Reg { 
            temp: fn_ctx.next_temp(), 
            idx: 2, 
            size: REG_SIZE_8
        };
        let prepare_size_ptr = IRInstr::Mov { 
            dest: x2_reg_temp,
            src: IRValueType::Const(IRImmVal::Int32(ob_size as i32)),
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
            dest: IRValueType::ExtendedTemp { 
                id: fn_ctx.next_temp(), 
                size: REG_SIZE_8 
            }, 
            addr: IRAddr::StackOff(store_off)
        };

        output.push(load_gc_alloced_obj_canon);
        Ok(output)
    }

    fn lower_import_to_ir(&self) -> CGRes {
        Ok(vec![])   
    }

    fn lower_rec_decl_to_ir(&mut self, _node: &mut AST) -> CGRes {
        Ok(vec![])
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
}

impl Aarch64IRGen {
    pub fn new(ctx: Rc<RefCell<CompilerCtx>>) -> Self {
        Self {
            ctx,
            label_id: 0,
            early_return_label_id: None,
            current_function: None,
        }
    }

    fn _gen_int_value_load_code(value: &LitType, reg_name: &str) -> Result<String, ()> {
        let mut result: String = "".to_string();
        match value {
            LitType::I64(int_val) => {
                let splitted_i64: Vec<String> = i64_hex_split_quarter(to_hex(*int_val));
                if splitted_i64.is_empty() {
                    return Err(());
                }
                result.push_str(&format!("movz {}, 0x{}, lsl #48\n", reg_name, splitted_i64[0]));
                result.push_str(&format!("movk {}, 0x{}, lsl #32\n", reg_name, splitted_i64[1]));
                result.push_str(&format!("movk {}, 0x{}, lsl #16\n", reg_name, splitted_i64[2]));
                result.push_str(&format!("movk {}, 0x{}", reg_name, splitted_i64[3]));
            },
            LitType::I32(int_val) => {
                let splitted_i32: Vec<String> = i32_hex_split_half(to_hex(*int_val));
                if splitted_i32.is_empty() {
                    return Err(());
                }
                result.push_str(&format!("movz {}, 0x{}, lsl #16\n", reg_name, splitted_i32[0]));
                result.push_str(&format!("movk {}, 0x{}", reg_name, splitted_i32[1]));
            }
            LitType::I16(int_val) => {
                result.push_str(&format!("movz {}, {}", reg_name, to_hex(int_val)));
            }
            LitType::U8(u8_val) => {
                result.push_str(&format!("movz {}, {}", reg_name, to_hex(u8_val)));
            }
            _ => {
                return Err(());
            },
        };
        Ok(result)
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