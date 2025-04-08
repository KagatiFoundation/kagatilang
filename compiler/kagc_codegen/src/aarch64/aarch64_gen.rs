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

use core::panic;
use std::cell::RefCell;
use std::cell::RefMut;
use std::rc::Rc;
use std::vec;

use kagc_ast::*;
use kagc_ctx::*;
use kagc_ir::ir_instr::*;
use kagc_ir::ir_instr::IR;
use kagc_ir::ir_types::IRLitType;
use kagc_ir::LabelId;
use kagc_symbol::*;
use kagc_target::reg::*;
use kagc_target::asm::aarch64::Aarch64RegManager2;
use kagc_types::*;
use kagc_utils::integer::*;

use crate::errors::CodeGenErr;
use crate::typedefs::CGExprEvalRes;
use crate::typedefs::CGRes;
use crate::typedefs::FnCtx;
use crate::CodeGen;
use crate::CodeGenResult;

use super::stackframe::compute_stack_size;

pub(crate) const AARCH64_ALIGN_SIZE: usize = 8;

lazy_static::lazy_static! {
    static ref CMP_CONDS_LIST: Vec<&'static str> = vec!["ne", "eq", "ge", "le", "lt", "gt"];
}

pub struct Aarch64CodeGen<'aarch64> {
    reg_manager: Rc<RefCell<Aarch64RegManager2>>,

    ctx: Rc<RefCell<CompilerCtx<'aarch64>>>,

    // label ID tracker
    label_id: LabelId,

    /// current function's ID
    function_id: usize,

    /// Current function that is being parsed
    current_function: Option<FunctionInfo>,
    
    early_return_label_id: usize,
}

impl<'aarch64> CodeGen for Aarch64CodeGen<'aarch64> {
    fn gen_global_symbols(&self) {
        let ctx_borrow = self.ctx.borrow();
        for symbol in ctx_borrow.sym_table.iter() {
            // symbol information is not generated if any of the following conditions matches
            if 
                symbol.lit_type == LitTypeVariant::None 
                || symbol.sym_type == SymbolType::Function 
                || symbol.class != StorageClass::GLOBAL 
            {
                continue;
            }
            if symbol.lit_type == LitTypeVariant::Str && symbol.sym_type == SymbolType::Constant {
                let str_const_name_and_val: Vec<&str> = symbol.name.split("---").collect::<Vec<&str>>();
                let str_const_name: String = String::from(str_const_name_and_val[0]);
                println!(".data\n.global {str_const_name}");
                println!("{str_const_name}:\n\t.asciz \"{}\"", str_const_name_and_val[1]);
                continue;
            }
            println!(".data\n.global {}", symbol.name);
            if symbol.sym_type == SymbolType::Variable {
                Aarch64CodeGen::dump_global_with_alignment(symbol);
            } else if symbol.sym_type == SymbolType::Array {
                let array_data_size: usize = symbol.lit_type.size();
                println!("here i come");
                println!("{}:", symbol.name);
                for _ in 0..symbol.size {
                    Aarch64CodeGen::alloc_data_space(array_data_size);
                }
            }
        }
    }
    
    fn gen_if_stmt(&mut self, ast: &AST, reg: usize) -> CodeGenResult {
        // Label for jumping to the 'else' block if the condition is false
        let label_if_false: usize = self.get_next_label();
    
        // Label marking the end of the entire if-else block
        let label_end: usize = self.get_next_label();
    
        // Evaluate the condition and store the result in a register
        let _cond_result_reg: AllocedReg = self.gen_code_from_ast(ast.left.as_ref().unwrap(), label_if_false, ASTOperation::AST_IF)?;
    
        // Free registers to allow usage within the if-else body
        // self.reg_manager.borrow_mut().deallocate_all();
    
        // Generate code for the 'if-true' block
        _ = self.gen_code_from_ast(ast.mid.as_ref().unwrap(), reg, ASTOperation::AST_IF)?;
    
        // Free registers again to prepare for any subsequent operations
        // self.reg_manager.borrow_mut().deallocate_all();
    
        // Jump to the end of the if-else block to skip the 'else' block if present
        if ast.right.is_some() {
            _ = self.gen_jump(label_end)?;
        }
    
        // Label for the start of the 'else' block
        _ = self.gen_label(label_if_false)?;
    
        // Generate code for the 'else' block, if it exists
        if let Some(ref right_ast) = ast.right {
            _ = self.gen_code_from_ast(right_ast, reg, ASTOperation::AST_IF)?;
            // self.reg_manager.borrow_mut().deallocate_all();
            _ = self.gen_label(label_end)?; // Mark the end of the if-else block
        }
        Ok(AllocedReg::no_reg())
    }
    
    fn gen_cmp_and_set(&self, operation: ASTOperation, r1: AllocedReg, r2: AllocedReg) -> CodeGenResult {
        let r1name: String = self.reg_manager.borrow().name(r1.idx, r1.size);
        let r2name: String = self.reg_manager.borrow().name(r2.idx, r2.size);
        println!("cmp {}, {}", r1name, r2name);
        let compare_operator: &str = match operation {
            ASTOperation::AST_LTHAN => "bge",
            ASTOperation::AST_GTHAN => "ble",
            ASTOperation::AST_EQEQ => "bne",
            ASTOperation::AST_NEQ => "beq",
            ASTOperation::AST_GTEQ => "blt",
            ASTOperation::AST_LTEQ => "bgt",
            _ => panic!("Not a valid ASTOperation for cmp and set")
        };
        println!(
            "cset {}, {}",
            r2name,
            compare_operator
        );
        println!("and {}, {}, 255", r2name, r2name);
        Ok(r2)
    }

    fn gen_cmp_and_jmp(&self, operation: ASTOperation, r1: AllocedReg, r2: AllocedReg, label: usize) -> CodeGenResult {
        let r1name: String = self.reg_manager.borrow().name(r1.idx, r1.size);
        let r2name: String = self.reg_manager.borrow().name(r2.idx, r2.size);
        println!("cmp {r1name}, {r2name}");
        let compare_operator: &str = match operation {
            ASTOperation::AST_LTHAN => "bhs",
            ASTOperation::AST_GTHAN => "bls",
            ASTOperation::AST_EQEQ => "bne",
            ASTOperation::AST_NEQ => "beq",
            ASTOperation::AST_GTEQ => "blo",
            ASTOperation::AST_LTEQ => "bhi",
            _ => panic!("Not a valid ASTOperation for cmp and jmp")
        };
        println!("{compare_operator} _L{label}");
        Ok(AllocedReg::no_reg())
    }

    fn gen_function_stmt(&mut self, ast: &AST) -> CodeGenResult {
        self.reg_manager.borrow_mut().reset();

        let index: usize = if let Some(Stmt::FuncDecl(func_decl)) = &ast.kind.as_stmt() {
            func_decl.func_id
        } else {
            panic!("Not a valid symbol table indexing method");
        };

        let func_name: String = self.get_func_name(index).expect("Function name error!");

        if let Some(finfo) = self.ctx.borrow().func_table.get(&func_name) {
            self.current_function = Some(finfo.clone());
        }

        let func_info: FunctionInfo = self.current_function.as_ref().cloned().unwrap();

        // If the function is declared as extern, print its external linkage
        // declaration and return a placeholder value indicating an unresolved
        // address (0xFFFFFFFF).
        if func_info.storage_class == StorageClass::EXTERN {
            self.emit(&format!(".extern _{func_name}"));
            return Ok(AllocedReg::no_reg());
        }

        self.ctx.borrow_mut().switch_to_func_scope(func_info.func_id);

        // check if this function calls other functions
        let calls_fns: bool = ast.left.as_ref()
            .map_or(
                false, 
                |body| body.contains_operation(ASTOperation::AST_FUNC_CALL)
            );

        let stack_size: usize = compute_stack_size(&self.ctx.borrow(), ast, &func_name).ok().unwrap();
        
        if calls_fns {
            self.emit_non_leaf_fn_prol(&func_name, stack_size as usize);
        } 
        else {
            self.emit_leaf_fn_prol(&func_name, stack_size as usize);
        }

        let mut leaf_fn_stack_off: i32 = stack_size as i32 - AARCH64_ALIGN_SIZE as i32;

        // generating code for function parameters
        for param_sym in func_info.collect_params().iter() {
            if param_sym.lit_type != LitTypeVariant::None {
                let param_reg: AllocedReg = self.__allocate_reg(param_sym.lit_type.size());
                if !calls_fns {
                    self.emit(&format!("str {}, [sp, #{}]", param_reg.name(), leaf_fn_stack_off));
                    leaf_fn_stack_off -= AARCH64_ALIGN_SIZE as i32;
                }
                else {
                    println!("str {}, [x29, #-{}]", self.reg_manager.borrow().name(param_reg.idx, param_reg.size), (param_sym.local_offset * 8) + 8);
                }
                self.reg_manager.borrow_mut().free_register(param_reg.idx);
            }
        }

        if let Some(ref body) = ast.left {
            self.gen_code_from_ast(body, 0xFFFFFFFF, ast.operation)?;
            if self.early_return_label_id != NO_REG {
                println!("_L{}:", self.early_return_label_id);
                self.early_return_label_id = NO_REG; // reset early return label after function code generation
            }
        }

        self.current_function = None;
        self.ctx.borrow_mut().switch_to_global_scope();

        if calls_fns {
            self.emit_non_leaf_fn_epl(stack_size);
        }
        else if stack_size != 0 {
            self.emit_leaf_fn_epl(stack_size);
        }
        else {
            self.emit("ret\n");
        }

        Ok(AllocedReg::no_reg())
    }

    fn gen_while_stmt(&mut self, ast: &AST) -> CodeGenResult {
        let label_start: usize = self.get_next_label();
        let label_end: usize = self.get_next_label();
        _ = self.gen_label(label_start)?; // start of loop body

        let cond_res_reg: AllocedReg = self.gen_code_from_ast(ast.left.as_ref().unwrap(), label_end, ast.operation)?;
        self.reg_manager.borrow_mut().free_register(cond_res_reg.idx);

        let body_reg: AllocedReg = self.gen_code_from_ast(ast.right.as_ref().unwrap(), label_end, ast.operation)?;
        self.reg_manager.borrow_mut().free_register(body_reg.idx);

        _ = self.gen_jump(label_start)?;
        _ = self.gen_label(label_end)?;
        Ok(AllocedReg::no_reg())
    }

    fn gen_loop_stmt(&mut self, ast: &AST) -> CodeGenResult {
        // loop start label
        let label_start: usize = self.get_next_label();

        // loop end label
        let label_end: usize = self.get_next_label();

        _ = self.gen_label(label_start)?;
        
        let body_reg: AllocedReg = self.gen_code_from_ast(ast.left.as_ref().unwrap(), label_end, ast.operation)?;
        self.reg_manager.borrow_mut().free_register(body_reg.idx);

        _ = self.gen_jump(label_start)?;
        _ = self.gen_label(label_end)?;
        Ok(AllocedReg::no_reg())
    }

    fn gen_break_stmt(&mut self, break_label: usize) -> CodeGenResult {
        println!("b _L{}", break_label);
        Ok(AllocedReg::no_reg())
    }

    fn gen_load_id_into_reg(&mut self, id_name: &str) -> CodeGenResult {
        let ctx_borrow = self.ctx.borrow();
        let symbol: Symbol = if let Ok(sym) = ctx_borrow.find_sym(id_name) {
            sym.clone()
        } else {
            return Err(CodeGenErr::UndefinedSymbol);
        };

        drop(ctx_borrow);

        let val_type: &LitTypeVariant = &symbol.lit_type;

        let mut reg: AllocedReg = AllocedReg::no_reg();
        
        let calling_func: bool = self.function_id != reg.idx;

        if calling_func {
            reg = self.__allocate_param_reg(val_type.size()); // self.reg_manager.borrow_mut().allocate_param_reg(val_type);
        } else {
            reg = self.__allocate_reg(val_type.size()); // self.reg_manager.borrow_mut().allocate(val_type);
        }
        let value_reg_name: String = self.reg_manager.borrow().name(reg.idx, reg.size);

        if symbol.class == StorageClass::GLOBAL {
            let value_reg: AllocedReg = self.__allocate_reg(val_type.size()); // self.reg_manager.borrow_mut().allocate();
            let reg_name: String = value_reg.name();
            self.dump_gid_address_load_code_from_name(&reg_name, &symbol);
            println!("ldr {}, [{}]", value_reg_name, reg_name);
            self.reg_manager.borrow_mut().free_register(value_reg.idx);
        } else {
            println!("ldr {}, [x29, #-{}]", value_reg_name, 8);
        } 
        Ok(reg)
    }

    // Refer to this page for explanation on '@PAGE' and '@PAGEOFF': https://stackoverflow.com/questions/65351533/apple-clang12-llvm-unknown-aarch64-fixup-kind
    fn gen_store_reg_value_into_id(&mut self, reg: AllocedReg, id_name: &str) -> CodeGenResult {
        let reg_name: String = self.reg_manager.borrow().name(reg.idx, reg.size);

        let ctx_borrow = self.ctx.borrow();

        let symbol: Symbol = if let Ok(sym) = ctx_borrow.find_sym(id_name) {
            sym.clone()
        } else {
            return Err(CodeGenErr::UndefinedSymbol);
        };

        drop(ctx_borrow);

        let addr_reg: AllocedReg = if symbol.class == StorageClass::GLOBAL {
            let ar: AllocedReg = self.__allocate_reg(symbol.lit_type.size());
            let addr_reg_name: String = ar.name();
            self.dump_gid_address_load_code_from_name(&addr_reg_name, &symbol);
            println!("str {}, [{}]", reg_name, addr_reg_name);
            ar
        } else {
            println!("str {}, [x29, #-{}]", reg_name, symbol.local_offset);
            AllocedReg::no_reg()
        };
        self.reg_manager.borrow_mut().free_register(reg.idx);
        Ok(addr_reg)
    }

    // Load an integer literal into a register
    fn gen_load_intlit_into_reg(&mut self, value: &LitType) -> CodeGenResult {
        let mut reg: AllocedReg = AllocedReg::no_reg();
        let inside_func: bool = self.function_id != reg.idx;
        if inside_func {
            reg = self.__allocate_reg(value.variant().size()); // self.reg_manager.borrow_mut().allocate_param_reg();
        } else {
            reg = self.__allocate_param_reg(value.variant().size()); // self.reg_manager.borrow_mut().allocate();
        }
        let result: Result<String, ()> = Aarch64CodeGen::gen_int_value_load_code(value, &reg.name());
        if let Ok(code) = result {
            println!("{}", code);
        } else {
            panic!("Was that supposed to be an integer: {:?}", value);
        }
        Ok(reg)
    }

    // id --> label information of the string
    fn gen_load_global_strlit(&mut self, id: &LitType) -> CodeGenResult {
        let mut reg: AllocedReg = AllocedReg::no_reg();
        let inside_func: bool = self.function_id != reg.idx;
        if inside_func {
            reg = self.__allocate_param_reg(id.variant().size()); // self.reg_manager.borrow_mut().allocate_param_reg();
        } else {
            reg = self.__allocate_reg(id.variant().size()); // self.reg_manager.borrow_mut().allocate();
        }
        let str_addr_name: String = self.reg_manager.borrow().name(reg.idx, reg.size);
        self.dump_gid_address_load_code_from_label_id(&str_addr_name, id);
        Ok(reg)
    }

    fn gen_add(&mut self, r1: AllocedReg, r2: AllocedReg) -> CodeGenResult {
        println!(
            "add {}, {}, {}",
            r1.name(),
            r1.name(),
            r2.name()
        );
        self.reg_manager.borrow_mut().free_register(r2.idx);
        Ok(r1)
    }

    fn gen_sub(&mut self, r1: AllocedReg, r2: AllocedReg) -> CodeGenResult {
        println!(
            "sub {}, {}, {}",
            r1.name(),
            r1.name(),
            r2.name()
        );
        self.reg_manager.borrow_mut().free_register(r2.idx);
        Ok(r1)
    }

   fn gen_mul(&mut self, r1: AllocedReg, r2: AllocedReg) -> CodeGenResult {
        println!(
            "mul {}, {}, {}",
            r1.name(),
            r1.name(),
            r2.name()
        );
        self.reg_manager.borrow_mut().free_register(r2.idx);
        Ok(r1)
    }

    fn gen_return_stmt(&mut self, early_return: bool) -> CodeGenResult {
        // NOTE: Generate code depending on the function's type. i.e. use w0 for i32, x0 for i64 etc.
        // let func_ret_type: LitTypeVariant = self.sym_table.get_symbol(func_id).lit_type;
        // is it an early return? 
        if early_return {
            self.early_return_label_id = self.get_next_label();
            
            self.emit(&format!("b _L{}", self.early_return_label_id));

            return Ok(AllocedReg::early_return());
        }
        Ok(AllocedReg::no_reg())
    }

    fn gen_array_access(&mut self, id: usize, expr: &AST) -> CodeGenResult {
        let expr_res_reg: AllocedReg = self.gen_code_from_ast(expr, 0xFFFFFFFF, ASTOperation::AST_ARRAY_ACCESS)?;
        let expr_res_reg_name: String = self.reg_manager.borrow().name(expr_res_reg.idx, expr_res_reg.size);

        // dealloc the expr register
        self.reg_manager.borrow_mut().free_register(expr_res_reg.idx);

        let symbol: Symbol = self.ctx.borrow().sym_table.get_symbol(id).unwrap().clone();

        let offset_shift: usize = match symbol.lit_type {
            LitTypeVariant::I32 | // as of now, this compiler does not know how to index 32-bit int array
            LitTypeVariant::I64 => 3, // so I am using offset of 8 bytes to calculate array indexes even though
                                        // items are 32-bit
            _ => 0,
        };

        // this will contain the address + offset of an array
        let addr_reg: AllocedReg = self.__allocate_reg(64); // self.reg_manager.borrow_mut().allocate(&LitTypeVariant::I64);
        let addr_reg_name: String = self.reg_manager.borrow().name(addr_reg.idx, addr_reg.size); // self.reg_manager.borrow().name(addr_reg, 0);

        let off_addr_reg: AllocedReg = self.__allocate_reg(64); // self.reg_manager.borrow_mut().allocate();
        let off_addr_reg_name: String = self.reg_manager.borrow().name(off_addr_reg.idx, off_addr_reg.size);

        self.dump_gid_address_load_code_from_name(&addr_reg_name, &symbol);

        println!(
            "ldr {}, [{}, {}, lsl {}]",
            off_addr_reg_name, addr_reg_name, expr_res_reg_name, offset_shift
        );

        self.reg_manager.borrow_mut().free_register(addr_reg.idx);
        Ok(off_addr_reg)
    }
    
    // generate 'branch' code. Not branch with link
    fn gen_jump(&self, label_id: usize) -> CodeGenResult {
        println!("b _L{}", label_id);
        Ok(AllocedReg::no_reg())
    }

    // generate normal label code
    fn gen_label(&mut self, label: usize) -> CodeGenResult {
        println!("_L{}:", label);
        Ok(AllocedReg::no_reg())
    }
    
    fn reg_manager(&self) -> RefMut<dyn RegManager2> {
        self.reg_manager.borrow_mut()
    }
    
    fn gen_local_var_decl_stmt(&mut self, var_decl_stmt: &VarDeclStmt, expr_ast: &Expr) -> CodeGenResult {
        if self.current_function.is_none() {
            panic!("Parsing a local variable but function is not defined... Weird.");
        }

        let expr_reg_res: AllocedReg = self.gen_expr(
            expr_ast, 
            ASTOperation::AST_VAR_DECL, 
            0xFFFFFFFF, 
            ASTOperation::AST_NONE
        )?;

        let reg_name: String = expr_reg_res.name();
        self.emit(&format!("str {}, [x29, #-{}]", reg_name, (var_decl_stmt.symtbl_pos * 8) + 8));

        self.reg_manager.borrow_mut().free_register(expr_reg_res.idx);
        
        Ok(AllocedReg::no_reg())
    }
    
    fn gen_local_arr_var_decl_stmt(&mut self, arr_var_decl_stmt: &ArrVarDeclStmt) -> CodeGenResult {
        if self.current_function.is_none() {
            panic!("Parsing a local variable but function is not defined... Weird.");
        }

        let func_info: &FunctionInfo = self.current_function.as_ref().unwrap();
        let symbol: Symbol = func_info.local_syms.get_or_fail(arr_var_decl_stmt.symtbl_pos).clone();

        // dump array size information onto the stack
        let size_reg: AllocedReg = self.gen_load_intlit_into_reg(&LitType::I32(symbol.size as i32))?;
        let size_reg_name: String = size_reg.name();
        println!("str {}, [x29, #-{}]", size_reg_name, symbol.local_offset);

        let mut item_off_counter: i32 = symbol.local_offset + 4; // add '4' because we had to store size information
        for expr in &arr_var_decl_stmt.vals {
            let expr_reg: AllocedReg = self.gen_expr(expr, ASTOperation::AST_ARR_VAR_DECL, NO_REG, ASTOperation::AST_ARR_VAR_DECL)?;
            let reg_name: String = expr_reg.name();
            println!("str {}, [x29, #-{}]", reg_name, item_off_counter);
            item_off_counter += {
                match symbol.lit_type {
                    LitTypeVariant::I64 => 8,
                    LitTypeVariant::U8
                    | LitTypeVariant::I16
                    | LitTypeVariant::I32 => 4,
                    _ => panic!("cannot create offset for type: '{:?}'", symbol.lit_type)
                }
            };
            // self.reg_manager.borrow_mut().deallocate_all();
        }
        Ok(AllocedReg::no_reg())
    }
    
    fn gen_func_call_expr(&mut self, func_call_expr: &FuncCallExpr) -> CodeGenResult {
        let ctx_borrow = self.ctx.borrow_mut();
        let func_info = if let Ok(symbol) = ctx_borrow.find_sym(&func_call_expr.symbol_name) {
            ctx_borrow.func_table.get(&symbol.name).unwrap()
        } else {
            return Err(CodeGenErr::UndefinedSymbol);
        };

        let mut reg_mgr = self.reg_manager.borrow_mut();

        let mut spilled_regs: Vec<usize> = vec![];

        for i in 0..func_call_expr.args.len() {
            if !reg_mgr.is_free(i) {
                spilled_regs.push(i);
                reg_mgr.free_register(i);
            }
        }

        println!("bl _{}", func_info.name);
        Ok(AllocedReg::no_reg()) 
    }

    fn gen_var_assignment_stmt(&mut self, assign_stmt: &AssignStmt, expr_ast: &Expr) -> CodeGenResult {
        let expr_reg: AllocedReg = self.gen_expr(
            expr_ast, 
            ASTOperation::AST_ASSIGN, 
            0xFFFFFFFF, 
            ASTOperation::AST_NONE
        )?;
        _ = self.gen_store_reg_value_into_id(expr_reg, &assign_stmt.sym_name)?;
        Ok(AllocedReg::no_reg())
    }

    fn emit_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) {
        self.emit(&format!("\n.global _{fn_label}\n_{fn_label}:"));

        if stack_size != 0 {
            self.emit(&format!("sub sp, sp, #{stack_size}"));
        }
    }

    fn emit_non_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) {
        self.emit(&format!("\n.global _{fn_label}\n_{fn_label}:"));
        self.emit(&format!("sub sp, sp, #{stack_size}"));
        self.emit(&format!("stp x29, x30, [sp, #{}]", stack_size - 16));
        self.emit(&format!("add x29, sp, #{}", stack_size - 16));
    }

    fn emit_leaf_fn_epl(&self, stack_size: usize) {
        self.emit(&format!("add sp, sp, #{stack_size}\nret\n"));
    }

    fn emit_non_leaf_fn_epl(&self, stack_size: usize) {
        self.emit(&format!("ldp x29, x30, [sp, #{}]", stack_size - 16));
        self.emit(&format!("add sp, sp, #{}\nret\n", stack_size));
    }

    fn gen_ir_fn(&mut self, ast: &mut AST) -> CGRes {
        self.reg_manager.borrow_mut().reset();

        let func_id: usize = if let Some(Stmt::FuncDecl(func_decl)) = &ast.kind.as_stmt() {
            func_decl.func_id
        } else {
            panic!("Expected FuncStmt but found {:?}", ast);
        };

        let func_name: String = self.get_func_name(func_id).expect("Function name error!");

        if let Some(finfo) = self.ctx.borrow().func_table.get(&func_name) {
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
                        is_leaf: true
                    }
                )]
            );
        }

        self.ctx.borrow_mut().switch_to_func_scope(func_id);

        let store_class: StorageClass = func_info.storage_class;

        let mut stack_off: usize = 0;

        let mut virtual_reg: usize = 0;

        // Collect function parameters and map each of them to a 
        // parameter register based on the Aarch64 ABI
        let params: Vec<IRLitType> = func_info
                                    .collect_params()
                                    .iter()
                                    .map(|_| {
                                        let reg: IRLitType = IRLitType::Reg(virtual_reg);
                                        virtual_reg += 1;
                                        stack_off += 1;
                                        reg
                                    }).collect();

        let mut fn_body: Vec<IR> = vec![];

        // Setting up function context
        let mut fn_ctx: FnCtx = FnCtx {
            stack_offset: stack_off,

            // local temporary counter starts at 0
            temp_counter: 0,

            // register information
            reg_counter: None,
            force_reg_use: false,

            // early return information
            early_return: false,

            parent_ast_kind: ASTOperation::AST_FUNCTION,
            prev_ast_kind: None,

            next_label: 0,
            force_label_use: 0
        };

        let linearized_body: Vec<&mut AST> = ast.left.as_mut().unwrap().linearize_mut();

        for body_ast  in linearized_body {
            let body_ir: Vec<IR> = self.gen_ir_from_node(body_ast, &mut fn_ctx, ASTOperation::AST_FUNCTION)?;
            fn_body.extend(body_ir);
        }

        self.current_function = None;
        self.ctx.borrow_mut().switch_to_global_scope();

        let calls_fns: bool = ast.contains_operation(ASTOperation::AST_FUNC_CALL);

        Ok(
            vec![IR::Func(
                IRFunc { 
                    name: func_name, 
                    params, 
                    body: fn_body,
                    class: store_class,
                    is_leaf: !calls_fns
                }
            )]
        )
    }

    fn gen_ir_var_decl(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> CGRes {
        if let Some(Stmt::VarDecl(var_decl)) = ast.kind.as_stmt() {
            let var_sym: Symbol = self.get_symbol_local_or_global(&var_decl.sym_name).unwrap();

            if ast.left.is_none() {
                panic!("Variable is not assigned a value!");
            }

            // set parent ast kind as var decl
            fn_ctx.change_parent_ast_kind(ASTOperation::AST_VAR_DECL);

            let var_decl_val: Vec<IRInstr> = self.gen_ir_expr(ast.left.as_mut().unwrap(), fn_ctx)?;

            fn_ctx.reset_parent_ast_kind();

            let decl_ir: IR = IR::VarDecl(
                IRVarDecl {
                    sym_name: var_decl.sym_name.clone(),
                    class: var_sym.class,
                    offset: Some(fn_ctx.stack_offset),
                    value: var_decl_val.last().unwrap().clone().dest().unwrap()
                }
            );

            // increment the stack offset after use
            fn_ctx.stack_offset += 1;

            let mut result: Vec<IR> = vec![];

            for instr in var_decl_val {
                result.push(IR::Instr(instr));
            }

            result.push(decl_ir);
            
            return Ok(result);
        }

        // FIND A BETTER SOLUTIN THAN JUST PANICKING
        panic!()
    }

    fn gen_ident_ir_expr(&mut self, ident_expr: &IdentExpr, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        let lit_val_tmp: usize = fn_ctx.temp_counter;
        fn_ctx.temp_counter += 1;

        let sym: Symbol = self.get_symbol_local_or_global(&ident_expr.sym_name).unwrap();

        Ok(vec![
            IRInstr::Load {
                dest: IRLitType::Temp(lit_val_tmp),
                stack_off: sym.local_offset as usize
            }
        ])
    }

    fn gen_ir_fn_call_expr(&mut self, func_call_expr: &mut FuncCallExpr, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        let use_reg: bool = fn_ctx.force_reg_use;
        let forced_reg: Option<usize> = fn_ctx.reg_counter;

        let mut param_instrs: Vec<IRInstr> = vec![];

        let mut actual_params: Vec<(usize, IRLitType)> = vec![];

        let num_args: usize = func_call_expr.args.len();
        let mut last_instrs: Vec<IRInstr> = vec![];

        for (rev_idx, (_, param_expr)) in func_call_expr.args.iter_mut().rev().enumerate() {
            let param_reg_idx = num_args - 1 - rev_idx;

            let current_tmp: usize = fn_ctx.temp_counter;

            let p_instr: Vec<IRInstr> = self.__gen_expr(param_expr, fn_ctx)?;
            
            let last_instr: IRInstr = p_instr.last().unwrap().clone();
            last_instrs.push(last_instr);

            let new_temp: usize = fn_ctx.temp_counter;
            let tmp_reg_loc: usize = current_tmp + (new_temp - current_tmp) - 1;

            param_instrs.extend(p_instr);

            actual_params.push((param_reg_idx, IRLitType::Temp(tmp_reg_loc)));
        }

        if use_reg {
            fn_ctx.force_reg_use(forced_reg.unwrap());
        }
        else {
            fn_ctx.clear_reg_hint();
        }

        for (rev_idx, last_instr) in last_instrs.iter().rev().enumerate() {
            param_instrs.push(
                IRInstr::Mov(
                    IRLitType::Reg(rev_idx), 
                    last_instr.dest().unwrap()
                )
            );
        }

        let preserve_ret_val_instr: Option<IRInstr> = 
        if !func_call_expr.result_type.is_void() {
            if use_reg {
                Some(IRInstr::Mov(
                    IRLitType::Reg(forced_reg.unwrap()),
                    IRLitType::Reg(0)
                ))
            }
            else {
                let call_tmp: usize = fn_ctx.temp_counter;
                fn_ctx.temp_counter += 1;
                Some(IRInstr::Mov(
                    IRLitType::Temp(call_tmp),
                    IRLitType::Reg(0)
                ))
            }
        }
        else {
            None
        };

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
            let mut ret_instrs: Vec<IR> = if fn_ctx.early_return {
                // turn off early return ASAP
                fn_ctx.early_return = false;

                self.early_return_label_id = self.get_next_label();
                vec![
                    IR::Instr(IRInstr::Jump { label_id: self.early_return_label_id })
                ]
            }
            else {
                vec![]
            };

            // if the function is non-void type
            if let Some(curr_fn) = &self.current_function {
                if !curr_fn.return_type.is_void() {
                    let ret_stmt_instrs: Vec<IRInstr> = self.gen_ir_expr(ret_stmt.left.as_mut().unwrap(), fn_ctx)?;

                    // last instruction is basically the temporary which holds the return 
                    // expression evaulation result
                    let last_instr: IRInstr = ret_stmt_instrs.last().cloned().unwrap();

                    ret_stmt_instrs.iter().for_each(|instr| ret_instrs.push(IR::Instr(instr.clone())));
                    ret_instrs.push(
                        IR::Instr(
                            IRInstr::Mov(
                                IRLitType::Reg(0),
                                last_instr.dest().unwrap()
                            )
                        )
                    );
                }
            }
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
        // Label for jumping to the 'else' block if the condition is false
        let label_if_false: usize = fn_ctx.get_next_label();

        // Label marking the end of the entire if-else block
        let label_end: usize = fn_ctx.get_next_label();

        let mut output: Vec<IR> = vec![];

        // change the parent AST kind to AST_IF
        fn_ctx.change_parent_ast_kind(ASTOperation::AST_IF);

        // Evaluate the condition and store the resilt in a register.
        // Every if-else must have the `left` branch set in the main if-else AST tree.

        fn_ctx.change_label_use(label_if_false);
        
        let cond_result: Vec<IRInstr> = self.gen_ir_expr(ast.left.as_mut().unwrap(), fn_ctx)?;
        
        fn_ctx.reset_label_hint();

        for cond_res_ir in cond_result {
            output.push(IR::Instr(cond_res_ir));
        }

        fn_ctx.reset_parent_ast_kind();

        let if_body: Vec<IR> = self.gen_ir_from_node(ast.mid.as_mut().unwrap(), fn_ctx, ASTOperation::AST_IF)?;
        output.extend(if_body);

        if ast.right.is_some() {
            output.extend(self.gen_ir_jump(label_end)?);
        }

        output.extend(self.gen_ir_label(label_if_false)?);

        if let Some(right_ast) = ast.right.as_mut() {
            let else_block: Vec<IR> = self.gen_ir_from_node(right_ast, fn_ctx, ASTOperation::AST_IF)?;
            output.extend(else_block);
            output.extend(self.gen_ir_label(label_end)?);
        }

        Ok(output)
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

impl<'aarch64> Aarch64CodeGen<'aarch64> {
    pub fn sort_fn_call_args(args: &mut [FuncArg]) {
        args.sort_by(|a, b| {
            let a_priority = match &a.1 {
                Expr::FuncCall(_) => 0,
                Expr::Subscript(_) => 1,
                Expr::Binary(_) => 2,
                Expr::Widen(_) => 3,
                Expr::Ident(_) => 4,
                Expr::LitVal(_) => 5,
            };
            
            let b_priority = match &b.1 {
                Expr::FuncCall(_) => 0,
                Expr::Subscript(_) => 1,
                Expr::Binary(_) => 2,
                Expr::Widen(_) => 3,
                Expr::Ident(_) => 4,
                Expr::LitVal(_) => 5,
            };
            
            a_priority.cmp(&b_priority)
        });
    }
}

impl<'aarch64> Aarch64CodeGen<'aarch64> {
    pub fn new(
        reg_manager: Rc<RefCell<Aarch64RegManager2>>,
        ctx: Rc<RefCell<CompilerCtx<'aarch64>>>
    ) -> Self {
        Self {
            reg_manager,
            ctx,
            label_id: 0,
            function_id: NO_REG,
            early_return_label_id: NO_REG,
            current_function: None,
        }
    }

    pub fn gen_with_ctx(&mut self, nodes: &[AST]) {
        self.label_id = self.ctx.borrow().label_id + 10;
        self.start_gen(nodes);
    }

    fn get_next_label(&mut self) -> LabelId {
        let lbl = self.label_id;
        self.label_id += 1;
        lbl
    }

    // ADRP loads the address of the page of given variables. In other words, 
    // ADRP loads the address of the page where the given global identifier lies 
    // on. Doing this alone doesn't give us the address of the global identifier. 
    // We need to add the offset of the global into the page address. 
    // For example, if the page address is 40 and our global's address is 44, 
    // we first load the page address (40) into the register and add the global's 
    // offset (4) into it, creating an PC relative address.
    fn dump_gid_address_load_code_from_label_id(&self, reg_name: &str, id: &LitType) {
        let symbol_label_id: usize = match id {
            LitType::I32(_idx) => *_idx as usize,
            LitType::Str(_, label) => *label,
            _ => panic!("Can't index symtable with this type: {:?}", id),
        };
        println!("adrp {}, _L{}@PAGE", reg_name, symbol_label_id);
        println!("add {}, {}, _L{}@PAGEOFF", reg_name, reg_name, symbol_label_id);
    }

    fn dump_gid_address_load_code_from_name(&mut self, reg_name: &str, symbol: &Symbol) {
        if symbol.class == StorageClass::GLOBAL {
            let sym_name: &str = &symbol.name;
            println!("adrp {}, {}@PAGE", reg_name, sym_name);
            println!("add {}, {}, {}@PAGEOFF", reg_name, reg_name, sym_name);
        }
    }

    fn alloc_data_space(size: usize) {
        match size {
            1 => println!(".byte 0"),
            4 => println!(".word 0"),
            8 => println!(".quad 0"),
            _ => panic!("Not possible to generate space for size: {}", size),
        }
    }

    fn dump_global_with_alignment(symbol: &Symbol) {
        let def_val: String = if let Some(dv) = &symbol.default_value {
            dv.to_string()
        } 
        else { 
            "0".to_string() 
        };
        match symbol.lit_type {
            LitTypeVariant::I32 => println!("{}: .align 4\n\t.word {}", symbol.name, def_val),
            LitTypeVariant::U8 => println!("{}:\t.byte {}", symbol.name, def_val),
            LitTypeVariant::Str => {
                let label_id: i32 = if let Some(lit_val) = &symbol.default_value {
                    match lit_val {
                        LitType::I32(__id) => *__id,
                        _ => panic!("Not a valid label id for string literal '{}'", symbol.default_value.as_ref().unwrap())
                    }
                } else {
                    panic!("No label id provided for string literal");
                };
                println!("{}:\t.quad ._L{:?}", symbol.name, label_id);
            }
            _ => panic!("Symbol's size is not supported right now: '{:?}'", symbol),
        }
    }

    fn gen_int_value_load_code(value: &LitType, reg_name: &str) -> Result<String, ()> {
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

    fn __allocate_reg(&mut self, alloc_size: usize) -> AllocedReg {
        self.reg_manager.borrow_mut().allocate_register(alloc_size)
    }

    fn __allocate_param_reg(&mut self, alloc_size: usize) -> AllocedReg {
        self.reg_manager.borrow_mut().allocate_param_register(alloc_size)
    }

    fn get_func_name(&mut self, index: usize) -> Option<String> {
        let ctx_borrow = self.ctx.borrow();
        ctx_borrow.get_func_name(index)
    }

    fn get_symbol_local_or_global(&self, sym_name: &str) -> Option<Symbol> {
        let ctx_borrow = self.ctx.borrow();

        if self.current_function.is_none() {
            let sym: Option<&Symbol> = ctx_borrow.sym_table.find(sym_name);
            Some(sym.unwrap_or_else(|| panic!("Symbol not found with the name: {}", sym_name)).clone())
        }
        else {
            let func_info: &FunctionInfo = self.current_function.as_ref().unwrap();
            Some(func_info.local_syms.find(sym_name).unwrap_or_else(|| panic!("Symbol not defined inside the function!")).clone())
        }
    }
}

#[cfg(test)]
mod tests {
    
}