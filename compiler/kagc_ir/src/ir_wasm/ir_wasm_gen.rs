use crate::{ir_asm::ir_asm_gen::IRToASM, ir_instr::{IRAddr, IR}};

pub struct IRToWASM {

}

impl IRToWASM {
    pub fn gen_wasm(&mut self, irs: &mut [IR]) -> String {
        // println!("{:#?}", irs);
        // return String::new();

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

impl IRToASM for IRToWASM {
    fn gen_load_global_asm(&mut self, idx: usize, dest: &crate::ir_types::IRLitType) -> String {
        todo!()
    }

    fn gen_cond_jmp_asm(&mut self, op1: &crate::ir_types::IRLitType, op2: &crate::ir_types::IRLitType, operation: crate::ir_types::IRCondOp, label_id: crate::LabelId) -> String {
        todo!()
    }

    fn start_func_call_proc(&mut self) -> String {
        todo!()
    }

    fn stop_func_call_proc(&mut self) -> String {
        todo!()
    }

    fn gen_ir_fn_call_asm(&mut self, fn_name: String, params: &[(usize, crate::ir_types::IRLitType)], return_type: &Option<crate::ir_types::IRLitType>) -> String {
        todo!()
    }

    fn gen_ir_add_asm(&mut self, dest: &crate::ir_types::IRLitType, op1: &crate::ir_types::IRLitType, op2: &crate::ir_types::IRLitType) -> String {
        todo!()
    }

    fn gen_ir_sub_asm(&mut self, dest: &crate::ir_types::IRLitType, op1: &crate::ir_types::IRLitType, op2: &crate::ir_types::IRLitType) -> String {
        todo!()
    }

    fn gen_ir_mul_asm(&mut self, dest: &crate::ir_types::IRLitType, op1: &crate::ir_types::IRLitType, op2: &crate::ir_types::IRLitType) -> String {
        todo!()
    }

    fn gen_ir_div_asm(&mut self, dest: &crate::ir_types::IRLitType, op1: &crate::ir_types::IRLitType, op2: &crate::ir_types::IRLitType) -> String {
        todo!()
    }

    fn gen_ir_mov_asm(&mut self, dest: &crate::ir_types::IRLitType, src: &crate::ir_types::IRLitType) -> String {
        todo!()
    }

    fn gen_ir_fn_asm(&mut self, fn_ir: &mut crate::ir_instr::IRFunc) -> String {
        format!("(fn ${}) (result i32)", fn_ir.name)
    }

    fn gen_ir_local_var_decl_asm(&mut self, vdecl_ir: &crate::ir_instr::IRVarDecl) -> String {
        todo!()
    }

    fn gen_ir_return_asm(&mut self, ir_return: &crate::ir_instr::IRReturn) -> String {
        todo!()
    }

    fn gen_ir_loop_asm(&mut self, ir_loop: &mut crate::ir_instr::IRLoop) -> String {
        todo!()
    }

    fn gen_ir_label_asm(&mut self, ir_label: &crate::ir_instr::IRLabel) -> String {
        todo!()
    }

    fn gen_ir_jump_asm(&mut self, label_id: usize) -> String {
        todo!()
    }

    fn gen_asm_load(&mut self, dest: &crate::ir_types::IRLitType, addr: &IRAddr) -> String {
        todo!()
    }

    fn gen_asm_store(&mut self, src: &crate::ir_types::IRLitType, addr: &IRAddr) -> String {
        todo!()
    }

    fn gen_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String {
        todo!()
    }

    fn gen_non_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String {
        todo!()
    }

    fn gen_leaf_fn_epl(&self, stack_size: usize) -> String {
        todo!()
    }

    fn gen_non_leaf_fn_epl(&self, stack_size: usize) -> String {
        todo!()
    }
}