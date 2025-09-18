// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_ir::ir_instr::IRAddr;
use kagc_ir::ir_instr::IRFunc;
use kagc_ir::ir_instr::IR;
use kagc_ir::ir_liveness::LiveRange;
use kagc_ir::ir_liveness::LivenessAnalyzer;
use kagc_ir::ir_types::IRCondOp;
use kagc_ir::ir_types::IRValueType;
use kagc_ir::ir_types::IRImmVal;
use kagc_ir::ir_instr::IRReturn;
use kagc_ir::ir_instr::IRLoop;
use kagc_ir::ir_types::TempId;
use kagc_ir::LabelId;
use kagc_symbol::StorageClass;
use kagc_target::asm::x86::X86Reg;
use kagc_target::asm::x86::X86RegMgr;
use kagc_target::asm::x86::X86RegName;
use kagc_target::reg::RegStatus;
use kagc_target::reg::REG_SIZE_8;

use crate::Codegen;
use crate::ComptFnProps;

#[derive(Default)]
struct X86TRMap {
    reg_map: HashMap<TempId, X86Reg>
}

impl X86TRMap {
    pub fn drop(&mut self, key: &TempId) -> Option<X86Reg> {
        self.reg_map.remove(key)
    }

    /// Get temporary ID associated with the given register.
    pub fn find_temp_by_reg(&self, reg: X86RegName) -> Option<TempId> {
        self.reg_map
            .iter()
            .find(|(_, &r)| {
                r.name == reg
            })
            .map(|(temp, _)| *temp)
    }

    pub fn find_reg_by_temp(&self, temp_id: TempId) -> Option<&X86Reg> {
        self.reg_map.get(&temp_id)
    }

    pub fn clear_mappings(&mut self) {
        self.reg_map.clear();
    }
}

pub struct X86Codegen {
    rm: X86RegMgr,

    temp_reg_map: X86TRMap,

    compt_fn_props: Option<ComptFnProps>,

    /// Instruction Pointer:
    /// IP is used to track the execution of instructions inside 
    /// a function.
    func_ip: usize,
}

impl X86Codegen {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            rm: X86RegMgr::new(),
            temp_reg_map: X86TRMap::default(),
            compt_fn_props: None,
            func_ip: 0
        }
    }
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

    fn switch_to_func_scope(&mut self, func_props: ComptFnProps) {
        self.compt_fn_props = Some(func_props);
    }

    fn drop_temp(&mut self, temp: usize) {
        let freed_reg: Option<X86Reg> = self.temp_reg_map.drop(&temp);

        if let Some(reg) = freed_reg {
            self.rm.free_reg(reg.name);
        }
    }

    fn try_dropping_temp(&mut self, temp: usize) -> bool {
        if let Some(compt_fn_info) = &self.compt_fn_props {
            if let Some((start, end)) = compt_fn_info.liveness_info.get(&temp) {
                // temporary's life is over
                if (*start + *end) <= self.func_ip {
                    self.drop_temp(temp);
                    return true;
                }
            }
        }
        else {
            panic!("Compile time information not available for function!");
        }
        false
    }

    fn try_dropping_all_temps(&mut self) -> bool {
        {
            let mut temps = vec![];
            for (temp, _) in self.temp_reg_map.reg_map.iter() {
                temps.push(*temp);
            }
            temps
        }.iter().for_each(|temp| {
            _ = self.try_dropping_temp(*temp);
        });
        true
    }

    #[inline]
    fn advance_ip(&mut self) {
        self.func_ip += 1;
    }
}

impl Codegen for X86Codegen {
    fn gen_ir_fn_asm(&mut self, fn_ir: &mut IRFunc) -> String {
        if fn_ir.class == StorageClass::EXTERN {
            return format!(".extern _{}\n", fn_ir.name);
        }

        let temp_liveness: HashMap<usize, LiveRange> = LivenessAnalyzer::analyze_fn_temps(fn_ir);

        // generate the code for function body
        self.switch_to_func_scope(
            ComptFnProps { 
                is_leaf: fn_ir.is_leaf, 
                stack_size: fn_ir.params.len() * 8,
                liveness_info: temp_liveness,
                _next_stack_slot: 0,
            }
        );

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
            self.try_dropping_all_temps();
            self.advance_ip();
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
                &format!("PUSH rbp\nMOV rbp, rsp\nSUB rsp, {stack_size}")
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
                &format!("SUB rsp, {stack_size}")
            );
        }
        output
    }

    fn gen_leaf_fn_epl(&self, stack_size: usize) -> String {
        let mut output = String::new();
        if stack_size != 0 {
            output.push_str("LEAVE\n");
        }
        output.push_str("RET");
        output
    }

    fn gen_non_leaf_fn_epl(&self, _stack_size: usize) -> String {
        "LEAVE\nRET".to_string()
    }

    fn gen_asm_store(&mut self, src: &IRValueType, addr: &IRAddr) -> String {
        let src_reg = self.extract_operand(src);
        match addr {
            IRAddr::StackOff(stack_off) => {
                let off = (*stack_off + 1) * 8;
                if off == 0 {
                    format!("MOV QWORD PTR [rbp], {src}", src = src_reg)
                }
                else {
                    format!("MOV QWORD PTR [rbp-{off}], {src}", src = src_reg)
                }
            },
            _ => panic!()
        }
    }

    fn gen_asm_load(&mut self, dest: &IRValueType, addr: &IRAddr) -> String {
        let d = self.resolve_register(dest).1;
        match addr {
            IRAddr::StackOff(stack_off) => {
                let off = (*stack_off + 1) * 8;
                if off == 0 {
                    format!("MOV {dest}, QWORD PTR [rbp]", dest = d.name())
                }
                else {
                    format!("MOV {dest}, QWORD PTR [rbp-{off}]", dest = d.name())
                }
            },
            _ => panic!()
        }
    }

    fn gen_ir_add_asm(&mut self, dest: &IRValueType, op1: &IRValueType, op2: &IRValueType) -> String {
        let mut output = "".to_string();
        let dest_reg = self.resolve_register(dest).1;
        let first_value = self.extract_operand(op1);
        let second_value = self.extract_operand(op2);

        output.push_str(&format!("MOV {dest}, {first_value}\n", dest = dest_reg.name()));
        output.push_str(&format!("ADD {dest}, {second_value}", dest = dest_reg.name()));
        output
    }

    fn gen_load_global_asm(&mut self, pool_idx: usize, dest: &IRValueType) -> String {
        todo!()
    }

    fn gen_cond_jmp_asm(&mut self, op1: &IRValueType, op2: &IRValueType, operation: IRCondOp, label_id: LabelId) -> String {
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
        let s = self.extract_operand(src);
        let d = self.resolve_register(dest).1;

        let mut output = String::new();
        output.push_str(&format!("MOV {dest}, {src}", dest = d.name(), src = s));
        output
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
                    IRImmVal::Null => "0".to_string(), // Null is just '0' under the hood. LOL
                    _ => todo!()
                }
            },

            IRValueType::ExtendedTemp { id, .. } => {
                let reg = self.temp_reg_map.reg_map.get(id).unwrap_or_else(|| panic!());
                reg.name().to_string()
            }

            IRValueType::RetIn { size, .. } => {
                let src_reg = X86Reg {
                    name: X86RegName::RAX,
                    size: *size,
                    status: RegStatus::Free,
                };
                src_reg.name().to_string()
            }

            _ => todo!("{irlit:#?}")
        }
    }

    fn resolve_register(&mut self, irlit: &IRValueType) -> (TempId, X86Reg) {
        match irlit {
            IRValueType::ExtendedTemp { id, size } => {
                let reg = self.rm.allocate(*size).ok().unwrap();
                self.temp_reg_map.reg_map.insert(*id, reg);
                (*id, reg)
            },

            IRValueType::RetOut { temp, .. } => {
                let reg = self.rm.allocate_fixed_register(X86RegName::RAX, REG_SIZE_8).ok().unwrap();
                self.temp_reg_map.reg_map.insert(*temp, reg);
                (*temp, reg)
            },

            _ => panic!()
        }
    }
}