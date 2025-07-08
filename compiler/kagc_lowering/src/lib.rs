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
pub mod x86;
pub mod typedefs;
pub mod fn_ctx;

use std::cell::RefMut;

use errors::CodeGenErr;
use fn_ctx::FnCtx;
use kagc_ast::*;
use kagc_ir::{ir_instr::*, ir_types::*, LabelId};
use kagc_symbol::StorageClass;
use kagc_target::reg::*;
use kagc_types::*;
use typedefs::*;

pub trait CodeGen {
    fn gen_ir(&mut self, nodes: &mut [AST]) -> Vec<IR> {
        let mut output: Vec<IR> = vec![];
        for node in nodes {
            let node_ir: Result<Vec<IR>, CodeGenErr> = self.gen_ir_from_node(node, &mut FnCtx::default(), ASTOperation::AST_NONE);
            if node_ir.is_ok() {
                output.extend(node_ir.ok().unwrap());
            }
        }
        output
    }

    fn gen_ir_from_node(&mut self, node: &mut AST, fn_ctx: &mut FnCtx, parent_ast_kind: ASTOperation) -> CGRes {
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
            return self.__gen_ir_fn_call(node, fn_ctx);
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

    fn __gen_ir_fn_call(&mut self, node: &mut AST, fn_ctx: &mut FnCtx) -> CGRes {
        if let ASTKind::ExprAST(Expr::FuncCall(func_call)) = &mut node.kind {
            let mut result: Vec<IR> = vec![];
            
            self.gen_ir_fn_call_expr(func_call, fn_ctx).iter().for_each(|instrs| {
                instrs.iter().for_each(|instr| {
                    result.push(IR::Instr(instr.clone()));
                });
            });

            return Ok(result);
        }
        panic!()
    }

    /// Starts the code generation process.
    ///
    /// This method initializes code generation by generating global symbols and marking the start of the `.text` section.
    ///
    /// # Arguments
    ///
    /// * `nodes` - The Abstract Syntax Tree (AST) list used for code generation.
    fn start_gen(&mut self, nodes: &[AST]) where Self: Sized {
        self.gen_global_symbols();
        // .text section starts from here
        println!("\n.text");
        for node in nodes {
            let result: CodeGenResult = self.gen_code_from_ast(node, 0xFFFFFFFF, ASTOperation::AST_NONE);
            if result.is_err() {
                result.err().unwrap().dump();
            }
        }
    }

    /// Generate global symbols into the data section.
    ///
    /// This function iterates over the main symbol table, printing symbol 
    /// information for each global symbol. Symbols are skipped if they meet 
    /// any of the following conditions:
    ///   - The symbol type is `Function`.
    ///   - The literal type is `None`.
    ///   - The storage class is `LOCAL`.
    /// 
    /// For each processed global symbol, the appropriate `.data` directive and 
    /// symbol name are printed. If the symbol is a variable, its global data is 
    /// printed with proper alignment using `dump_global_with_alignment`. If the 
    /// symbol is an array, the appropriate data space is allocated for each element 
    /// based on its size and data type.
    fn gen_global_symbols(&self);

    /// Generates code for a given AST node and returns a register index 
    /// for further processing.
    ///
    /// This function takes a reference to an Abstract Syntax Tree (AST) node
    /// (`ast_node`) that needs code generation, a register index (`reg`) 
    /// indicating the register to use for the operation, and the parent AST 
    /// operation (`parent_ast_kind`) to provide context during code generation.
    ///
    /// # Arguments
    ///
    /// * `ast_node` - A reference to an AST node for which code needs to be generated.
    /// * `reg` - A usize value representing the register index to use for the operation.
    /// * `parent_ast_kind` - An enum representing the parent operation in the AST hierarchy.
    ///
    /// # Returns
    ///
    /// The function returns a usize value representing the register index for further 
    /// processing.
    fn gen_code_from_ast(
        &mut self, ast_node: &AST, 
        reg: usize, 
        parent_ast_kind: ASTOperation) 
    -> CodeGenResult where Self: Sized {
        if ast_node.operation == ASTOperation::AST_IF {
            self.gen_if_stmt(ast_node, reg)
        } 
        else if ast_node.operation == ASTOperation::AST_WHILE {
            self.gen_while_stmt(ast_node)
        } 
        else if ast_node.operation == ASTOperation::AST_FUNCTION {
            self.gen_function_stmt(ast_node)
        } 
        else if ast_node.operation == ASTOperation::AST_BREAK {
            self.gen_break_stmt(reg)
        }
        else if ast_node.operation == ASTOperation::AST_GLUE {
            if let Some(left) = ast_node.left.as_ref() {
                self.gen_code_from_ast(left, reg, parent_ast_kind)?;
                // self.reg_manager().deallocate_all();
            }
            if let Some(right) = ast_node.right.as_ref() {
                self.gen_code_from_ast(right, reg, parent_ast_kind)?;
                // self.reg_manager().deallocate_all();
            }
            Ok(AllocedReg::no_reg())
        }
        else if ast_node.operation == ASTOperation::AST_RETURN {
            let possible_ret_stmt: &Stmt = ast_node.kind.as_stmt().unwrap_or_else(|| panic!("How can it be? No Expr? Are you serious?"));

            return match possible_ret_stmt {
                Stmt::Return(_) => {
                    let early_return: bool = parent_ast_kind != ASTOperation::AST_FUNCTION;

                    if ast_node.left.is_some() {
                        let return_expr: &Expr = ast_node.left.as_ref().unwrap().kind.as_expr().unwrap_or_else(|| panic!("How can it be? No Expr? Are you serious?"));
                        _ = self.gen_expr(return_expr, ast_node.operation, reg, parent_ast_kind)?;
                    }

                    self.gen_return_stmt(early_return)
                }
                _ => Ok(AllocedReg::no_reg())

            };
        }
        else if ast_node.operation == ASTOperation::AST_VAR_DECL {
            if let ASTKind::StmtAST(var_decl) = &ast_node.kind {
                return match var_decl {
                    Stmt::VarDecl(var_decl_stmt) => {
                        if var_decl_stmt.class != StorageClass::LOCAL {
                            Ok(AllocedReg::no_reg())
                        } else {
                            let assign_expr: &ASTKind = &ast_node.left.as_ref().unwrap().kind;
                            if let ASTKind::ExprAST(__expr) = assign_expr {
                                _ = self.gen_local_var_decl_stmt(var_decl_stmt, __expr);
                            }
                            Ok(AllocedReg::no_reg())
                        }
                    },
                    _ => Ok(AllocedReg::no_reg())
                };
            }
            Ok(AllocedReg::no_reg())
        } 
        else if ast_node.operation == ASTOperation::AST_ARR_VAR_DECL {
            if let ASTKind::StmtAST(arr_var_decl) = &ast_node.kind {
                return match arr_var_decl {
                    Stmt::ArrVarDecl(arr_var_decl_stmt) => {
                        if arr_var_decl_stmt.class != StorageClass::LOCAL {
                            Ok(AllocedReg::no_reg())
                        } 
                        else {
                            self.gen_local_arr_var_decl_stmt(arr_var_decl_stmt)
                        }
                    },
                    _ => Ok(AllocedReg::no_reg())
                }
            }
            Ok(AllocedReg::no_reg())
        }
        else if ast_node.operation == ASTOperation::AST_NONE {
            return Ok(AllocedReg::no_reg());
        }  
        else if ast_node.operation == ASTOperation::AST_ASSIGN {
            let possible_assign_stmt: &Stmt = ast_node.kind.as_stmt().unwrap_or_else(|| panic!("How can it be? No Stmt? Are you serious?"));
            return match possible_assign_stmt {
                Stmt::Assignment(assign) => {
                    let assign_expr = &ast_node.right.as_ref().unwrap().kind;
                    if let ASTKind::ExprAST(__expr) = assign_expr {
                        _ = self.gen_var_assignment_stmt(assign, __expr);
                    }
                    Ok(AllocedReg::no_reg())
                },
                _ => Ok(AllocedReg::no_reg())
            };
        }
        else if ast_node.operation == ASTOperation::AST_LOOP {
            let possible_loop_stmt: &Stmt = ast_node.kind.as_stmt().unwrap_or_else(|| panic!("How can it be? No Stmt? Are you serious?"));
            return match possible_loop_stmt {
                Stmt::Loop => {
                    self.gen_loop_stmt(ast_node)
                },
                _ => Ok(AllocedReg::no_reg())
            }
        }
        else {
            let expr_ast: &Expr = ast_node.kind.as_expr().unwrap_or_else(|| panic!("How can it be? No Expr? Are you serious?"));
            self.gen_expr(expr_ast, ast_node.operation, reg, parent_ast_kind)
        }
    }

    fn gen_expr(
        &mut self, 
        expr: &Expr, 
        curr_ast_kind: ASTOperation, 
        reg: usize, 
        parent_ast_kind: ASTOperation
    ) -> CodeGenResult {
        match expr {
            Expr::Binary(bin) => self.gen_bin_expr(bin, curr_ast_kind, reg, parent_ast_kind),
            Expr::LitVal(lit) => self.gen_lit_expr(lit),
            Expr::Ident(ident) => self.gen_load_id_into_reg(&ident.sym_name),
            Expr::Widen(widen) => {
                match widen.from.kind.clone() {
                    ASTKind::StmtAST(_) => todo!(),
                    ASTKind::ExprAST(wexpr) => {
                        self.gen_expr(&wexpr, curr_ast_kind, reg, parent_ast_kind)
                    },
                    ASTKind::Empty => Ok(AllocedReg::no_reg())
                }
            },
            Expr::FuncCall(func_call) => {
                self.gen_func_call_expr(func_call)
            },
            _ => Ok(AllocedReg::no_reg())
        }
    }

    /// Generates code for the given literal value expression.
    ///
    /// This method generates code to load the literal value into a register based on its type.
    ///
    /// # Arguments
    ///
    /// * `lit_expr` - The literal value expression for which to generate code.
    ///
    /// # Returns
    ///
    /// The register index if code generation is successful, otherwise 0xFFFFFFFF.
    ///
    fn gen_lit_expr(&mut self, lit_expr: &LitValExpr) -> CodeGenResult {
        match lit_expr.result_type {
            LitTypeVariant::U8 |
            LitTypeVariant::I16 |
            LitTypeVariant::I64 |
            LitTypeVariant::I32 => self.gen_load_intlit_into_reg(&lit_expr.value),
            LitTypeVariant::Str => self.gen_load_global_strlit(&lit_expr.value),
            _ => Ok(AllocedReg::no_reg())
        }
    }

    fn gen_bin_expr(&mut self, bin_expr: &BinExpr, curr_ast_kind: ASTOperation, reg: usize, parent_ast_kind: ASTOperation) -> CodeGenResult {
        let leftreg: AllocedReg = self.gen_expr(&bin_expr.left, curr_ast_kind, reg, parent_ast_kind)?;
        let rightreg: AllocedReg = self.gen_expr(&bin_expr.right, curr_ast_kind, reg, parent_ast_kind)?;

        match bin_expr.operation {
            ASTOperation::AST_ADD => self.gen_add(leftreg, rightreg),
            ASTOperation::AST_SUBTRACT => self.gen_sub(leftreg, rightreg),
            ASTOperation::AST_MULTIPLY => self.gen_mul(leftreg, rightreg),
            ASTOperation::AST_GTHAN
            | ASTOperation::AST_LTHAN
            | ASTOperation::AST_LTEQ
            | ASTOperation::AST_GTEQ
            | ASTOperation::AST_NEQ
            | ASTOperation::AST_EQEQ => {
                if (parent_ast_kind == ASTOperation::AST_IF)
                || (parent_ast_kind == ASTOperation::AST_WHILE)
                {
                    self.gen_cmp_and_jmp(bin_expr.operation, leftreg, rightreg, reg)
                } else {
                    self.gen_cmp_and_set(bin_expr.operation, leftreg, rightreg)
                } 
            },
            _ => Ok(AllocedReg::no_reg())
        }
    }

    /// Generates code for an If statement AST node and returns a constant 
    /// value to signify no register allocation.
    ///
    /// This function takes a mutable reference to `self` to allow modification 
    /// of the current code generation context. It also takes a reference to an 
    /// Abstract Syntax Tree (AST) node representing an If statement (`Stmt::IfStmt`). 
    /// The function generates code for the If statement AST node and always returns 
    /// `0xFFFFFFFF` to signify that no register allocation occurred during the code 
    /// generation process.
    ///
    /// # Arguments
    ///
    /// * `ast` - A reference to an AST node representing an If statement (`Stmt::IfStmt`).
    ///
    /// # Returns
    ///
    /// The function always returns `0xFFFFFFFF` to signify no register allocation during 
    /// code generation.
    fn gen_if_stmt(&mut self, ast: &AST, reg: usize) -> CodeGenResult;
    
    fn gen_jump(&self, label_id: usize) -> CodeGenResult;

    fn gen_label(&mut self, label: usize) -> CodeGenResult;

    fn gen_cmp_and_jmp(&self, operation: ASTOperation, r1: AllocedReg, r2: AllocedReg, label: usize) -> CodeGenResult;

    fn gen_cmp_and_set(&self, operation: ASTOperation, r1: AllocedReg, r2: AllocedReg) -> CodeGenResult;

    fn gen_function_stmt(&mut self, ast: &AST) -> CodeGenResult;

    // *** IR CODE GENERATION *** //
    fn gen_ir_fn(&mut self, ast: &mut AST) -> CGRes;

    fn gen_ir_var_decl(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> CGRes;

    fn gen_ir_return(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> CGRes;

    fn gen_ir_loop(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> CGRes;

    fn gen_ir_if(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> CGRes;

    fn gen_ir_label(&self, label_id: LabelId) -> CGRes;

    fn gen_ir_jump(&self, label_id: LabelId) -> CGRes;

    fn lower_import_to_ir(&self) -> CGRes;

    fn lower_rec_decl_to_ir(&mut self, node: &mut AST) -> CGRes;

    /// Generate IR nodes from an AST expression node.
    fn gen_ir_expr(&mut self, ast: &mut AST, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        if !ast.kind.is_expr() {
            panic!("Needed an Expr--but found {:#?}", ast);
        }
        
        if let ASTKind::ExprAST(expr) = &mut ast.kind {
            return self.__gen_expr(expr, fn_ctx);
        }

        Err(CodeGenErr::NoContext)
    }

    fn __gen_expr(&mut self, expr: &mut Expr, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        match expr {
            Expr::LitVal(litexpr) => self.gen_lit_ir_expr(litexpr, fn_ctx),
            Expr::Binary(binexpr) => self.gen_bin_ir_expr(binexpr, fn_ctx),
            Expr::Ident(identexpr) => self.gen_ident_ir_expr(identexpr, fn_ctx),
            Expr::FuncCall(funccallexpr) => self.gen_ir_fn_call_expr(funccallexpr, fn_ctx),
            Expr::RecordCreation(ref mut recexpr) => self.lower_rec_creation_to_ir(recexpr, fn_ctx),
            Expr::RecordFieldAssign(recfieldexpr) => self.lower_rec_field_assign_to_ir(recfieldexpr, fn_ctx),
            Expr::RecordFieldAccess(recfieldexpr) => self.lower_rec_field_access_to_ir(recfieldexpr, fn_ctx),
            Expr::Null => self.lower_null_const_to_ir(fn_ctx),
            _ => todo!()
        }
    }

    fn lower_rec_field_assign_to_ir(&mut self, rec_field: &mut RecordFieldAssignExpr, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        let mut output = vec![];
        let expr_res = self.__gen_expr(&mut rec_field.value, fn_ctx)?;
        let expr_temp = expr_res.last().unwrap().clone();
        let mut store_src = expr_temp.dest().clone();

        if let Some(IRLitType::StackOff(off)) = store_src {
            let tmp_id: usize = fn_ctx.temp_counter;
            fn_ctx.temp_counter += 1;
            output.push(IRInstr::Load { 
                dest: IRLitType::Temp(tmp_id), 
                stack_off: off 
            });
            store_src = Some(IRLitType::Temp(tmp_id));
        }

        let store = IRInstr::Store { 
            src: store_src.unwrap(),
            stack_off: IRLitType::StackOff(rec_field.offset)
        };

        output.extend(expr_res);
        output.push(store);
        Ok(output)
    }

    fn lower_rec_creation_to_ir(&mut self, rec_creation: &mut RecordCreationExpr, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        let mut output = vec![];

        for field in &mut rec_creation.fields {
            let field_output = self.lower_rec_field_assign_to_ir(field, fn_ctx)?;
            output.extend(field_output);
        }

        Ok(output)
    }

    fn lower_null_const_to_ir(&mut self, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        let lit_val_tmp: usize = fn_ctx.temp_counter;
        fn_ctx.temp_counter += 1;

        Ok(vec![IRInstr::mov_into_temp(lit_val_tmp, IRLitType::Const(IRLitVal::Null))])
    }

    fn gen_lit_ir_expr(&mut self, lit_expr: &LitValExpr, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        if let LitType::PoolStr(pool_idx) = &lit_expr.value  {
            return self.gen_ir_load_str(*pool_idx, fn_ctx);
        }

        let ir_lit: IRLitVal = match lit_expr.result_type {
            LitTypeVariant::I32 => IRLitVal::Int32(*lit_expr.value.unwrap_i32().expect("No i32 value!")),
            LitTypeVariant::U8 => IRLitVal::U8(*lit_expr.value.unwrap_u8().expect("No u8 value!")),
            _ => todo!(),
        };

        let lit_val_tmp: usize = fn_ctx.temp_counter;
        fn_ctx.temp_counter += 1;
  
        Ok(vec![IRInstr::mov_into_temp(lit_val_tmp, IRLitType::Const(ir_lit))])
    }

    fn gen_ir_load_str(&mut self, idx: usize, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        self.gen_ir_load_global_var(idx, fn_ctx)
    }

    fn gen_ir_load_global_var(&mut self, idx: usize, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        let lit_val_tmp: usize = fn_ctx.temp_counter;
        fn_ctx.temp_counter += 1;

        Ok(
            vec![
                IRInstr::LoadGlobal { 
                    pool_idx: idx, 
                    dest: IRLitType::Temp(lit_val_tmp) 
                }
            ]
        )
    }

    fn gen_bin_ir_expr(&mut self, bin_expr: &mut BinExpr, fn_ctx: &mut FnCtx) -> CGExprEvalRes {
        let mut irs: Vec<IRInstr> = vec![];

        let left_expr: Vec<IRInstr> = self.__gen_expr(&mut bin_expr.left, fn_ctx)?;
        let right_expr: Vec<IRInstr> = self.__gen_expr(&mut bin_expr.right, fn_ctx)?;

        let left_dest: &IRInstr = left_expr.last().unwrap_or_else(|| panic!("No left destination found! Abort!"));
        let right_dest: &IRInstr = right_expr.last().unwrap_or_else(|| panic!("No left destination found! Abort!"));

        fn dest_temp(fn_ctx: &mut FnCtx) -> usize {
            let dest_temp: usize = fn_ctx.temp_counter;
            fn_ctx.temp_counter += 1;
            dest_temp
        }

        let bin_expr_type: IRInstr = match bin_expr.operation {
            ASTOperation::AST_ADD => self.lower_add_to_ir(IRLitType::Temp(dest_temp(fn_ctx)), left_dest.dest().unwrap(), right_dest.dest().unwrap()),
            ASTOperation::AST_SUBTRACT => self.lower_sub_to_ir(IRLitType::Temp(dest_temp(fn_ctx)), left_dest.dest().unwrap(), right_dest.dest().unwrap()),
            ASTOperation::AST_MULTIPLY => self.lower_mul_to_ir(IRLitType::Temp(dest_temp(fn_ctx)), left_dest.dest().unwrap(), right_dest.dest().unwrap()),
            ASTOperation::AST_DIVIDE => self.lower_div_to_ir(IRLitType::Temp(dest_temp(fn_ctx)), left_dest.dest().unwrap(), right_dest.dest().unwrap()),

            ASTOperation::AST_GTHAN
            | ASTOperation::AST_LTHAN
            | ASTOperation::AST_LTEQ
            | ASTOperation::AST_GTEQ
            | ASTOperation::AST_NEQ
            | ASTOperation::AST_EQEQ => {
                let parent_ast_kind: ASTOperation = fn_ctx.parent_ast_kind;
                if (parent_ast_kind == ASTOperation::AST_IF) || (parent_ast_kind == ASTOperation::AST_WHILE) {
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

        irs.extend(left_expr);
        irs.extend(right_expr);
        irs.push(bin_expr_type);

        Ok(irs)
    }

    fn gen_ir_cmp_and_jump(&mut self, op1: IRLitType, op2: IRLitType, label_id: LabelId, operation: IRCondOp) -> IRInstr {
        IRInstr::CondJump {
            label_id,
            op1,
            op2,
            operation
        }
    }

    fn gen_ir_fn_call_expr(&mut self, func_call_expr: &mut FuncCallExpr, fn_ctx: &mut FnCtx) -> CGExprEvalRes;

    fn lower_add_to_ir(&mut self, dest: IRLitType, op1: IRLitType, op2: IRLitType) -> IRInstr {
       IRInstr::Add { dest, op1, op2 }
    }

    fn lower_sub_to_ir(&mut self, dest: IRLitType, op1: IRLitType, op2: IRLitType) -> IRInstr {
       IRInstr::Sub { dest, op1, op2 }
    }

    fn lower_mul_to_ir(&mut self, dest: IRLitType, op1: IRLitType, op2: IRLitType) -> IRInstr {
       IRInstr::Mul { dest, op1, op2 }
    }

    fn lower_div_to_ir(&mut self, dest: IRLitType, op1: IRLitType, op2: IRLitType) -> IRInstr {
       IRInstr::Div { dest, op1, op2 }
    }

    fn lower_rec_field_access_to_ir(&mut self, access: &mut RecordFieldAccessExpr, fn_ctx: &mut FnCtx) -> CGExprEvalRes;

    fn gen_ident_ir_expr(&mut self, ident_expr: &IdentExpr, fn_ctx: &mut FnCtx) -> CGExprEvalRes;

    // *** IR CODE GENERATION *** //

    fn gen_while_stmt(&mut self, ast: &AST) -> CodeGenResult;

    /// Loads the value corresponding to the symbol with the given position in the symbol table into a register.
    /// 
    /// # Arguments
    /// 
    /// * `id` - The symbol table position for the symbol.
    /// 
    /// # Returns
    /// 
    /// The register index where the value of the symbol is loaded.
    fn gen_load_id_into_reg(&mut self, id_name: &str) -> CodeGenResult;

    fn gen_store_reg_value_into_id(&mut self, reg: AllocedReg, id_name: &str) -> CodeGenResult;

    fn gen_add(&mut self, r1: AllocedReg, r2: AllocedReg) -> CodeGenResult;

    fn gen_sub(&mut self, r1: AllocedReg, r2: AllocedReg) -> CodeGenResult;
    
    fn gen_mul(&mut self, r1: AllocedReg, r2: AllocedReg) -> CodeGenResult;
    
    fn gen_load_intlit_into_reg(&mut self, value: &LitType) -> CodeGenResult;

    fn gen_load_global_strlit(&mut self, symbol_id: &LitType) -> CodeGenResult;

    fn gen_array_access(&mut self, symbol_id: usize, expr: &AST) -> CodeGenResult;

    fn gen_return_stmt(&mut self, early_return: bool) -> CodeGenResult;

    fn gen_func_call_expr(&mut self, func_call_expr: &FuncCallExpr) -> CodeGenResult;

    fn gen_local_var_decl_stmt(&mut self, var_decl_stmt: &VarDeclStmt, expr_ast: &Expr) -> CodeGenResult;
    
    fn gen_local_arr_var_decl_stmt(&mut self, arr_var_decl_stmt: &ArrVarDeclStmt) -> CodeGenResult;

    fn gen_var_assignment_stmt(&mut self, assign_stmt: &AssignStmt, expr_ast: &Expr) -> CodeGenResult;

    fn gen_loop_stmt(&mut self, ast: &AST) -> CodeGenResult;

    fn gen_break_stmt(&mut self, break_label: usize) -> CodeGenResult;
    
    fn reg_manager(&self) -> RefMut<dyn RegManager2>;
 
    fn emit(&self, code: &str) {
        println!("{code}");
    }   
}