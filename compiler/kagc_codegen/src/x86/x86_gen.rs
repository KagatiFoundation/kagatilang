// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_ir::ir_instr::IRAddr;
use kagc_ir::ir_instr::IR;
use kagc_ir::ir_types::IRValueType;
use kagc_ir::ir_types::IRImmVal;
use kagc_ir::ir_instr::IRReturn;
use kagc_ir::ir_instr::IRLoop;

use crate::Codegen;

pub struct X86Codegen;

impl X86Codegen {
 
}

impl X86Codegen {
    pub fn gen_asm(&mut self, irs: &mut [IR]) -> String {
        let mut output: Vec<String> = vec![];
        // output.push(".data".to_string());
        output.push(String::from(".text"));
        for ir in irs {
            let output_str: String = self.gen_asm_from_ir_node(ir);
            output.push(output_str);
        }
        output.join("\n")
    }
}

impl Codegen for X86Codegen {
    fn gen_ir_fn_asm(&mut self, fn_ir: &mut kagc_ir::ir_instr::IRFunc) -> String {
        let mut output = String::new();
        output.push_str(
            &format!(
                ".global {name}\n.type {name}, @function\n",
                name = fn_ir.name,
            )
        );

        let stack_size = 32;
        if fn_ir.is_leaf {
            output.push_str(
                &format!(
                    "{code}\n", 
                    code = &self.gen_leaf_fn_prol(&fn_ir.name, stack_size)
                )
            );
        }
        else {
            output.push_str(
                &format!(
                    "{code}\n", 
                    code = &self.gen_non_leaf_fn_prol(&fn_ir.name, stack_size)
                )
            );
        }

        let mut fn_body_asm = String::new();

        for body_ir in &mut fn_ir.body {
            let asm: String = self.gen_asm_from_ir_node(body_ir);
            fn_body_asm.push_str(&format!("{asm}\n", asm = asm.trim()));
        }

        output.push_str(&fn_body_asm);

        if fn_ir.is_leaf {
            output.push_str(
                &format!(
                    "{code}\n", 
                    code = &self.gen_leaf_fn_epl(stack_size)
                )
            );
        }
        else {
            output.push_str(
                &format!(
                    "{code}\n", 
                    code = &self.gen_non_leaf_fn_epl(stack_size)
                )
            );
        }
        output
    }

    fn gen_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String {
        let mut output = String::new();
        output.push_str(&format!("{fn_label}:\n"));

        if stack_size != 0 {
            output.push_str(
                &format!("SUB rsp, #{stack_size}")
            );
        }

        output
    }

    fn gen_non_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String {
        let mut output = String::new();
        output.push_str(&format!("{fn_label}:\n"));
        output.push_str("PUSH rbp\nMOV rbp, rsp");
        if stack_size != 0 {
            output.push_str(
                &format!("SUB rsp, #{stack_size}")
            );
        }
        output
    }

    fn gen_leaf_fn_epl(&self, stack_size: usize) -> String {
        let mut output = String::new();
        if stack_size != 0 {
            output.push_str(
                &format!("ADD rsp, #{stack_size}\n")
            );
        }
        output.push_str("RET");
        output
    }

    fn gen_non_leaf_fn_epl(&self, _stack_size: usize) -> String {
        "LEAVE\nRET".to_string()
    }

    fn gen_asm_store(&mut self, src: &IRValueType, addr: &IRAddr) -> String {
        match addr {
            IRAddr::StackOff(stack_off) => {
                let off = *stack_off;
                if off == 0 {
                    "MOV [rbp], rax".to_string()
                }
                else {
                    format!("MOV [rbp-{off}], rax")
                }
            },
            _ => panic!()
        }
    }

    fn gen_asm_load(&mut self, dest: &IRValueType, addr: &IRAddr) -> String {
        todo!()
    }

    fn gen_ir_add_asm(&mut self, dest: &IRValueType, op1: &IRValueType, op2: &IRValueType) -> String {
        let mut output = "".to_string();
        let first_value = self.extract_operand(op1);
        output.push_str(&format!("MOV rax, {first_value}"));

        let second_value = self.extract_operand(op2);
        output.push_str(&format!("ADD rax, {second_value}"));
        output
    }

    fn gen_load_global_asm(&mut self, pool_idx: usize, dest: &kagc_ir::ir_types::IRValueType) -> String {
        todo!()
    }

    fn gen_cond_jmp_asm(&mut self, op1: &kagc_ir::ir_types::IRValueType, op2: &kagc_ir::ir_types::IRValueType, operation: kagc_ir::ir_types::IRCondOp, label_id: kagc_ir::LabelId) -> String {
        todo!()
    }

    fn gen_ir_fn_call_asm(&mut self, fn_name: String, params: &[(usize, kagc_ir::ir_types::IRValueType)], return_type: &Option<kagc_ir::ir_types::IRValueType>) -> String {
        todo!()
    }

    fn gen_ir_mem_alloc(&mut self, size: usize, ob_type: &kagc_types::builtins::obj::KObjType) -> String {
        todo!()
    }

    fn gen_ir_reg_alloc(&mut self, dest: &kagc_ir::ir_types::IRValueType) -> String {
        todo!()
    }

    fn gen_ir_mem_cpy(&mut self) -> String {
        todo!()
    }

    fn gen_ir_sub_asm(&mut self, dest: &kagc_ir::ir_types::IRValueType, op1: &kagc_ir::ir_types::IRValueType, op2: &kagc_ir::ir_types::IRValueType) -> String {
        todo!()
    }

    fn gen_ir_mul_asm(&mut self, dest: &kagc_ir::ir_types::IRValueType, op1: &kagc_ir::ir_types::IRValueType, op2: &kagc_ir::ir_types::IRValueType) -> String {
        todo!()
    }

    fn gen_ir_div_asm(&mut self, dest: &kagc_ir::ir_types::IRValueType, op1: &kagc_ir::ir_types::IRValueType, op2: &kagc_ir::ir_types::IRValueType) -> String {
        todo!()
    }

    fn gen_ir_mov_asm(&mut self, dest: &IRValueType, src: &IRValueType) -> String {
        
        todo!()
    }

    fn gen_ir_return_asm(&mut self, ir_return: &IRReturn) -> String {
        todo!()
    }

    fn gen_ir_loop_asm(&mut self, ir_loop: &mut IRLoop) -> String {
        todo!()
    }

    fn gen_ir_label_asm(&mut self, ir_label: &kagc_ir::ir_instr::IRLabel) -> String {
        todo!()
    }

    fn gen_ir_jump_asm(&mut self, label_id: usize) -> String {
        todo!()
    }
}

impl X86Codegen {
    /// Extract the IRLitType as an operand(String)
    fn extract_operand(&mut self, irlit: &IRValueType) -> String {
        match irlit {
            IRValueType::Const(irlit_val) => {
                match irlit_val {
                    IRImmVal::Int32(value) => format!("{:#x}", *value),
                    IRImmVal::U8(value) => format!("{:#x}", *value),
                    IRImmVal::Str(value, ..) => value.clone(),
                    IRImmVal::Null => "#0".to_string(), // Null is just '0' under the hood. LOL
                    _ => todo!()
                }
            },
            
            IRValueType::Reg { idx, size, .. } => {
                if *size == 4 {
                    format!("w{}", *idx)
                }
                else {
                    format!("x{}", *idx)
                }
            },

            _ => unimplemented!()
        }
    }
}