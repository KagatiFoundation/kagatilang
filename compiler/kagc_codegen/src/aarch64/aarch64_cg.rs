// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::Codegen;
use crate::ComptFnProps;
use crate::CustomMap;
use crate::SpillMap;
use crate::TRMap;
use kagc_ir::ir_instr::*;
use kagc_ir::ir_liveness::LivenessAnalyzer;
use kagc_ir::ir_operands::IROperand;
use kagc_ir::ir_operands::TempId;
use kagc_ir::ir_operands::IRImmVal;
use kagc_ir::ir_operands::IRAddr;
use kagc_ir::LabelId;

use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::format;
use std::rc::Rc;

use kagc_const::pool::ConstEntry;
use kagc_const::pool::KagcConst;
use kagc_ctx::CompilerCtx;
use kagc_symbol::StorageClass;
use kagc_target::{asm::aarch64::*, reg::*};
use kagc_types::builtins::obj::KObjType;

use crate::IRToASMState;

/// Handles the translation of IR (Intermediate Representation) to 
/// AArch64 assembly.
///
/// - `ctx`: A reference-counted, mutable context for the compiler, 
///   which manages global state.
/// 
/// - `reg_manager`: A reference-counted, mutable register manager for 
///   AArch64, handling register allocation.
/// 
/// - `compt_fn_props`: Optional function properties, used for tracking 
///   whether a function is a leaf and its stack size.
pub struct Aarch64Codegen {
    pub ctx: Rc<RefCell<CompilerCtx>>,

    reg_manager: Aarch64RegMgr,

    compt_fn_props: Option<ComptFnProps>,

    temp_reg_map: TRMap,

    spill_map: SpillMap,

    /// Instruction Pointer:
    /// IP is used to track the execution of instructions inside 
    /// a function.
    func_ip: usize,

    /// Code generators state
    state: IRToASMState,

    /// Liveness analyzer
    la: LivenessAnalyzer,

    refill_stack: HashSet<RegIdx>
}

impl Aarch64Codegen {
    #[allow(clippy::new_without_default)]
    pub fn new(ctx: Rc<RefCell<CompilerCtx>>, rm: Aarch64RegMgr) -> Self {
        Self {
            ctx,
            reg_manager: rm,
            compt_fn_props: None,
            temp_reg_map: TRMap::default(),
            func_ip: 0,
            state: IRToASMState::Global,
            la: LivenessAnalyzer::default(),
            refill_stack: HashSet::new(),
            spill_map: SpillMap::default()
        }
    }

    pub fn gen_asm(&mut self, irs: &mut [IR]) -> String {
        // println!("{:#?}", irs);
        // return String::new();

        let mut output: Vec<String> = vec![];
        // output.push(".data".to_string());
        output.push(String::from(".text"));

        for ir in irs {
            let output_str: String = self.gen_asm_from_ir_node(ir);
            output.push(output_str);
        }
        output.push(self.dump_globals());
        output.join("\n")
    }

    /// Sets function-specific properties when entering a function scope.
    fn switch_to_func_scope(&mut self, func_props: ComptFnProps) {
        self.compt_fn_props = Some(func_props);
        self.state = IRToASMState::Local;
    }
    
    /// Clears function-specific properties when returning to the global scope.
    fn switch_to_global_scope(&mut self) {
        self.compt_fn_props = None;
        self.state = IRToASMState::Global;
    }
    
    /// Computes the aligned stack size required for a function.
    fn pre_compute_fn_stack_size(&mut self, func_ir: &IRFunc) -> Option<usize> {
        let mut stack_size: usize = func_ir.params.len() * 8; // Each parameter is 8 bytes

        if !func_ir.is_leaf {
            stack_size += 16; // Space for x29 and x30
        }
    
        for ir in &func_ir.body {
            if let IR::Instr(IRInstr::Store { .. }) = ir {
                stack_size += 8;
            }
            else if let IR::Instr(IRInstr::MemAlloc { .. }) = ir {
                stack_size += 8; // to store the allocated memory's address
            }
        }
    
        Some(Self::align_to_16(stack_size))
    }

    /// Align the given address into an address divisible by 16.
    fn align_to_16(value: usize) -> usize {
        (value + 16 - 1) & !15
    }

    fn drop_temp(&mut self, temp: usize) {
        let freed_reg: Option<AllocedReg> = self.temp_reg_map.drop(&temp);

        if let Some(reg) = freed_reg {
            self.reg_manager.free_register(reg.idx);
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

    fn dump_globals(&self) -> String {
        if self.ctx.borrow().const_pool.is_empty() {
            return "".to_string();
        }

        let mut output_str: String = String::new();
        for (index, c_item) in self.ctx.borrow().const_pool.iter_enumerated() {
            output_str.push_str(&self.dump_const(index, c_item, false));
        }
        output_str
    }

    fn dump_const(&self, c_item_index: usize, c_item: &ConstEntry, parent_is_record: bool) -> String {
        let mut output_str = String::new();
        if let KagcConst::Str(str_value) = &c_item.value {
            if parent_is_record {
                output_str.push_str(&format!("\t.xword .L.str.{c_item_index}\n"));
            }
            else {
                output_str.push_str(
                    &format!(
                        ".section __TEXT,__cstring\n\t.L.str.{}:\n\t.asciz \"{}\"\n", 
                        c_item_index, 
                        str_value
                    )
                );
            }
        }
        else if let KagcConst::Int(int_value) = &c_item.value {
            if parent_is_record {
                // output_str.push_str(&format!("\t.word {int_value}\n\t.zero 4\n"));
                output_str.push_str(&format!("\t.xword {int_value}\n"));
            }
        }
        else if let KagcConst::Record(rec) = &c_item.value {
            output_str.push_str(
                &format!(
                    ".section __DATA,__const\n.align {}\n.L__const.{}.{}:\n", 
                    rec.alignment, 
                    c_item.origin_func.unwrap(), 
                    rec.alias.clone()
                )
            );
            for rec_field in &rec.fields {
                if let Some(rec_pool_item) = self.ctx.borrow().const_pool.get(*rec_field.1) {
                    output_str.push_str(&self.dump_const(*rec_field.1, rec_pool_item, true));
                }
            }
        }
        output_str
    }

    /// Spill register to stack pointer (SP)
    fn gen_str_sp(&self, reg: &AllocedReg, stack_size: usize, slot: usize) -> String {
        let stack_off = stack_size - (slot * 8);
        let dest_addr = if stack_off != stack_size { // if not at the beginning of the stack frame
            format!("[SP, {stack_off:#x}]")
        }
        else {
            "[SP]".to_string()
        };
        format!("\tSTR {}, {dest_addr}\n", reg.name_aarch64())
    }

    /// Load register from stack pointer (SP)
    fn gen_ldr_sp(&self, reg: &AllocedReg, stack_size: usize, slot: usize) -> String {
        let stack_off = stack_size - (slot * 8);
        let src_addr = if stack_off != stack_size { // if not at the beginning of the stack frame
            format!("[SP, {stack_off:#x}]")
        }
        else {
            "[SP]".to_string()
        };
        format!("\tLDR {}, {src_addr}\n", reg.name_aarch64())
    }

    /// Spill register to frame pointer (x29)
    fn gen_str_fp(&self, reg: &AllocedReg, slot: usize) -> String {
        let stack_off = slot * 8;
        let dest_addr = if stack_off != 0 { // if not at the beginning of the frame pointer
            format!("[x29, -{stack_off:#x}]")
        }
        else {
            "[x29]".to_string()
        };
        format!("\tSTR {}, {dest_addr}\n", reg.name_aarch64())
    }

    /// Load register from frame pointer (x29)
    fn gen_ldr_fp(&self, reg: &AllocedReg, slot: usize) -> String {
        let stack_off = slot * 8;
        let src_addr = if stack_off != 0 { // if not at the beginning of the frame pointer
            format!("[x29, -{stack_off:#x}]")
        }
        else {
            "[x29]".to_string()
        };
        format!("\tLDR {}, {src_addr}\n", reg.name_aarch64())
    }

    fn gen_reg_spill(&self, reg: &AllocedReg, stack_size: usize, slot: usize) -> Option<String> {
        if reg.status != RegStatus::Spilled {
            return None;
        }
        Some(self.gen_str_sp(reg, stack_size, slot))
    }

    fn _gen_reg_unspill(&self, reg: &AllocedReg, stack_size: usize, slot: usize) -> Option<String> {
        if reg.status != RegStatus::Spilled {
            return None;
        }
        Some(self.gen_ldr_sp(reg, stack_size, slot))
    }

    fn _get_current_fn_props(&self) -> &ComptFnProps {
        self.compt_fn_props
            .as_ref()
            .expect("Compile time function info not found! Aborting...")
    }

    fn get_current_fn_props_mut(&mut self) -> &mut ComptFnProps {
        self.compt_fn_props
            .as_mut()
            .expect("Compile time function info not found! Aborting...")
    }

    #[inline]
    fn advance_ip(&mut self) {
        self.func_ip += 1;
    }

    #[inline]
    fn reset(&mut self) {
        // reset IP before moving into into another function
        self.func_ip = 0;

        // stay on the global scope; no function available
        self.switch_to_global_scope();

        // clear temp-reg mappings
        self.temp_reg_map.clear_mappings();
    }
}

impl Codegen for Aarch64Codegen {
    fn gen_ir_fn_asm(&mut self, fn_ir: &mut IRFunc) -> String {
        if fn_ir.class == StorageClass::EXTERN {
            // self.advance_ip();
            return format!(".extern _{}\n", fn_ir.name);
        }

        let mut output_str: String = "".to_string();

        // Temporary liveness information of this function
        self.la.analyze_fn_temps(fn_ir);
        let temp_liveness = self.la.liveness_info.clone();

        let stack_size: usize = self.pre_compute_fn_stack_size(fn_ir).unwrap();

        if fn_ir.is_leaf {
            output_str.push_str(&format!("\t{}\n", self.gen_leaf_fn_prol(&fn_ir.name, stack_size)));
        }
        else {
            output_str.push_str(&format!("\t{}\n", self.gen_non_leaf_fn_prol(&fn_ir.name, stack_size)));
        }

        // generate the code for function body
        self.switch_to_func_scope(
            ComptFnProps { 
                is_leaf: fn_ir.is_leaf, 
                stack_size,
                liveness_info: temp_liveness,
                next_stack_slot: 0,
            }
        );

        let mut fn_body_asm = String::new();
        for body_ir in &mut fn_ir.body {
            let _body_asm: String = self.gen_asm_from_ir_node(body_ir);
            self.try_dropping_all_temps();
            self.advance_ip();
            fn_body_asm.push_str(&format!("\t{}\n", _body_asm.trim()));
        }

        output_str.push_str(&fn_body_asm);

        // reset IP and scope before moving into another function
        self.reset();

        if fn_ir.is_leaf {
            output_str.push_str(&self.gen_leaf_fn_epl(stack_size));
        }
        else {
            output_str.push_str(&self.gen_non_leaf_fn_epl(stack_size));
        }
        output_str
    }

    fn gen_ir_return_asm(&mut self, _ir_return: &IRReturn) -> String {
        "hello".to_string()
    }
    
    fn gen_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String {
        let mut output_str: String = "".to_string();
        output_str.push_str(&format!("\n.global _{fn_label}\n_{fn_label}:"));

        if stack_size != 0 {
            output_str.push_str(&format!("\n\tSUB SP, SP, #{stack_size}"));
        }
        output_str
    }
    
    fn gen_leaf_fn_epl(&self, stack_size: usize) -> String {
        if stack_size != 0 {
            format!("\tADD SP, SP, #{stack_size}\n\tRET\n")
        }
        else {
            "\tRET\n".to_string()
        }
    }

    fn gen_non_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String {
        let mut output_str: String = "".to_string();
        output_str.push_str(&format!("\n.global _{fn_label}\n_{fn_label}:\n"));
        output_str.push_str(&format!("\tSUB SP, SP, #{stack_size}\n"));
        output_str.push_str(&format!("\tSTP x29, x30, [SP, #{}]\n", stack_size - 16));
        output_str.push_str(&format!("\tADD x29, SP, #{}", stack_size - 16));
        output_str
    }
    
    fn gen_non_leaf_fn_epl(&self, stack_size: usize) -> String {
        let mut output_str: String = "".to_string();
        output_str.push_str(&format!("\tLDP x29, x30, [SP, #{}]\n", stack_size - 16));
        output_str.push_str(&format!("\tADD SP, SP, #{}\nRET\n", stack_size));
        output_str
    }
    
    fn gen_asm_load(&mut self, dest: &IROperand, addr: &IRAddr) -> String {
        let compt_fun = self.get_current_fn_props_mut();
        let stack_size = compt_fun.stack_size;
        let is_leaf_fn = compt_fun.is_leaf;
        let next_stack_slot = compt_fun.next_stack_slot();

        let dest_reg = self.resolve_register(dest).1;
        let mut output_code = String::new();

        output_code.push_str(
            &self.gen_reg_spill(
                &dest_reg, 
                stack_size,
                next_stack_slot
            ).unwrap_or_default()
        );

        match addr {
            IRAddr::StackOff(stack_off) => {
                if is_leaf_fn {
                    output_code.push_str(
                        &self.gen_ldr_sp(
                            &dest_reg, 
                            stack_size, 
                            *stack_off
                        )
                    );
                }
                else {
                    output_code.push_str(
                        &self.gen_ldr_fp(
                            &dest_reg, 
                            *stack_off
                        )
                    );
                }
            },
            IRAddr::BaseOff(base, off) => {
                let src_reg = self.resolve_register(base);
                output_code.push_str(
                    &format!(
                        "\tLDR {}, [{}, {}]", 
                        dest_reg.name_aarch64(), 
                        src_reg.1.name_aarch64(), 
                        format_args!("#{}", *off * 8)
                    )
                );
            }
        }
        output_code
    }

    fn gen_asm_store(&mut self, src: &IROperand, addr: &IRAddr) -> String {
        let compt_fun = self.get_current_fn_props_mut();
        let stack_size = compt_fun.stack_size;
        let is_leaf_fn = compt_fun.is_leaf;

        let src_reg = self.resolve_register(src).1;
        match addr {
            IRAddr::StackOff(stack_off) => {
                if is_leaf_fn {
                    self.gen_str_sp(&src_reg, stack_size, *stack_off)
                }
                else {
                    self.gen_str_fp(&src_reg, *stack_off)
                }
            },
            IRAddr::BaseOff(base, off) => {
                let dest_reg = self.resolve_register(base).1;
                format!(
                    "\tSTR {}, [{}, {}]", 
                    src_reg.name_aarch64(), 
                    dest_reg.name_aarch64(), 
                    format_args!("#{}", off * 8)
                )
            }
        }
    }

    fn gen_ir_fn_call_asm(
        &mut self,
        fn_name: String,
        args: &[(usize, IROperand)],
        _return_type: &Option<IROperand>,
    ) -> String {
        self.try_dropping_all_temps();
        let mut out = String::new();
        let args_count = args.len();

        let mut regs_to_spill: Vec<AllocedReg> = Vec::new();
        for reg in Aarch64RegMgr::caller_saved_regs(REG_SIZE_8) {
            if reg.idx < args_count {
                continue;
            }
            if let Some(temp) = self.temp_reg_map.reverse_get(reg.clone()) {
                if self.la.is_temp_alive_after(temp, self.func_ip + 1) {
                    regs_to_spill.push(reg);
                }
            }
        }

        if regs_to_spill.is_empty() {
            out.push_str(&format!("\tBL _{fn_name}\n"));
            return out;
        }

        let mut transfer_regs: Vec<AllocedReg> = Vec::with_capacity(regs_to_spill.len());
        for _ in 0..regs_to_spill.len() {
            transfer_regs.push(self.reg_manager.allocate_callee_saved_register(REG_SIZE_8));
        }

        let spilled_transfer_count = transfer_regs
            .iter()
            .filter(|tr| tr.status == RegStatus::Spilled)
            .count();
        let dyn_size = spilled_transfer_count * 8;
        if dyn_size > 0 {
            out.push_str(&format!("\tSUB SP, SP, #{}\n", dyn_size));
        }

        let mut stack_offset = 0usize;
        for tr in &transfer_regs {
            if tr.status == RegStatus::Spilled {
                out.push_str(&format!(
                    "\tSTR {}, [SP, #{}]\n",
                    tr.name_aarch64(),
                    stack_offset
                ));
                stack_offset += 8;
            }
        }

        let mut spills: SpillMap = SpillMap::default();

        for (i, orig_reg) in regs_to_spill.iter().enumerate() {
            let tr = &transfer_regs[i];
            out.push_str(&format!(
                "\tMOV {}, {}\n",
                tr.name_aarch64(),
                orig_reg.name_aarch64()
            ));
            spills.reg_map.insert(orig_reg.clone(), tr.clone());
        }

        out.push_str(&format!("\tBL _{fn_name}\n"));

        for (i, orig_reg) in regs_to_spill.iter().enumerate() {
            let tr = &transfer_regs[i];
            out.push_str(&format!(
                "\tMOV {}, {}\n",
                orig_reg.name_aarch64(),
                tr.name_aarch64()
            ));
        }

        if dyn_size > 0 {
            let mut offset = 0usize;
            for tr in &transfer_regs {
                if tr.status == RegStatus::Spilled {
                    out.push_str(&format!(
                        "\tLDR {}, [SP, #{}]\n",
                        tr.name_aarch64(),
                        offset
                    ));
                    offset += 8;
                }
            }
            out.push_str(&format!("\tADD SP, SP, #{}\n", dyn_size));
        }
        out
    }
    
    fn gen_ir_mov_asm(&mut self, dest: &IROperand, src: &IROperand) -> String {
        let dest_reg: (usize, AllocedReg) = self.resolve_register(dest);
        let reg_name: String = dest_reg.1.name_aarch64();

        let operand: String = self.extract_operand(src);
        format!("\tMOV {}, {}", reg_name, operand)
    }
    
    fn gen_ir_add_asm(&mut self, dest: &IROperand, op1: &IROperand, op2: &IROperand) -> String {
        self.gen_ir_bin_op_asm(dest, op1, op2, "ADD")
    }

    fn gen_ir_sub_asm(&mut self, dest: &IROperand, op1: &IROperand, op2: &IROperand) -> String {
        self.gen_ir_bin_op_asm(dest, op1, op2, "SUB")
    }
    
    fn gen_ir_mul_asm(&mut self, dest: &IROperand, op1: &IROperand, op2: &IROperand) -> String {
        self.gen_ir_bin_op_asm(dest, op1, op2, "MUL")
    }
    
    fn gen_ir_div_asm(&mut self, dest: &IROperand, op1: &IROperand, op2: &IROperand) -> String {
        self.gen_ir_bin_op_asm(dest, op1, op2, "DIV")
    }
    
    fn gen_ir_jump_asm(&mut self, label_id: usize) -> String {
        format!("\tB .LB_{label_id}")
    }

    fn gen_ir_loop_asm(&mut self, ir_loop: &mut IRLoop) -> String {
        let mut output_str: String = String::new();

        for body_ir in &mut ir_loop.body {
            output_str.push_str(&format!("\t{}\n", self.gen_asm_from_ir_node(body_ir)));
            self.advance_ip();
        }

        output_str
    }

    fn gen_ir_label_asm(&mut self, ir_label: &IRLabel) -> String {
        format!(".LB_{}:", ir_label.0)
    }

    fn gen_load_global_asm(&mut self, pool_idx: usize, dest: &IROperand) -> String {
        let dest_reg: (usize, AllocedReg) = self.resolve_register(dest);
        let dest_reg_name: &str = &dest_reg.1.name_aarch64();

        let mut output_str: String = String::new();
        if let Some(c_item) = self.ctx.borrow().const_pool.get(pool_idx) {
            match &c_item.value {
                KagcConst::Str(_) => {
                    output_str.push_str(&format!("\tADRP {dest_reg_name}, .L.str.{pool_idx}@PAGE\n"));
                    output_str.push_str(&format!("\tADD {dest_reg_name}, {dest_reg_name}, .L.str.{pool_idx}@PAGEOFF"));
                },
                KagcConst::Record(rec_value) => {
                    output_str.push_str(
                        &format!(
                            "\tADRP {dest_reg_name}, .L__const.{}.{}@PAGE\n", 
                            c_item.origin_func.unwrap(), 
                            rec_value.alias.clone()
                        )
                    );
                    output_str.push_str(
                        &format!(
                            "\tADD {dest_reg_name}, {dest_reg_name}, .L__const.{}.{}@PAGEOFF\n", 
                            c_item.origin_func.unwrap(), 
                            rec_value.alias.clone()
                        )
                    );
                }
                _ => panic!()
            }
        }

        output_str
    }

    fn gen_cond_jmp_asm(&mut self, op1: &IROperand, op2: &IROperand, operation: IRCondOp, label_id: LabelId) -> String {
        let compare_operator: &str = match operation {
            IRCondOp::IRLThan => "BGE",
            IRCondOp::IRGThan => "BLE",
            IRCondOp::IREqEq => "BNE",
            IRCondOp::IRNEq => "BEQ",
            IRCondOp::IRGTEq => "BLT",
            IRCondOp::IRLTEq => "BGT"
        };
        
        let mut output_str: String = String::new();
        output_str.push_str(&format!("\tCMP {}, {}\n", self.extract_operand(op1), self.extract_operand(op2)));
        output_str.push_str(&format!("{compare_operator} .LB_{label_id}"));
        output_str
    }

    fn gen_ir_mem_alloc(&mut self, size: usize, ob_type: &KObjType) -> String {
        let mut output = "".to_string();

        // load the parameter
        let x0 = self.allocate_specific_register(0, REG_SIZE_8);
        let load_size = format!("MOV {}, {:#x}", x0.name_aarch64(), size);
        let x1 = self.allocate_specific_register(1, REG_SIZE_8);
        let load_type = format!("MOV {}, {:#x}", x1.name_aarch64(), ob_type.value());

        output.push_str(&format!(
        "{load_size}
        {load_type}
        BL _object_new
        CBNZ x0, 1f
        BL _obj_alloc_fail
        1:
        " 
        ));
        output
    }

    fn gen_ir_mem_cpy(&mut self) -> String {
        let mut output = "".to_string();
        output.push_str("\tBL _k_memcpy\n");
        output
    }
}

impl Aarch64Codegen {
    fn gen_ir_bin_op_asm(&mut self, dest: &IROperand, op1: &IROperand, op2: &IROperand, operation: &str) -> String {
        let compt_props = self.get_current_fn_props_mut();
        let stack_size = compt_props.stack_size;
        let next_slot = compt_props.next_stack_slot();

        let mut output = "".to_string();
        let dest_reg = self.resolve_register(dest).1;
        let reg_name: String = dest_reg.name_aarch64();

        let op1: String = self.extract_operand(op1);
        output.push_str(&self.refill_spilled_registers());

        let op2: String = self.extract_operand(op2);
        output.push_str(&self.refill_spilled_registers());

        output.push_str(
            &self.gen_reg_spill(
                &dest_reg, 
                stack_size, 
                next_slot
            ).unwrap_or_default()
        );

        output.push_str(
            &format!(
                "\t{} {}, {}, {}",
                operation, reg_name, 
                op1, op2
            )
        );
        output
    }

    /// Extract the IRLitType as an operand(String)
    fn extract_operand(&mut self, irlit: &IROperand) -> String {
        match irlit {
            IROperand::Const(irlit_val) => {
                match irlit_val {
                    IRImmVal::Int32(value) => format!("{:#x}", *value),
                    IRImmVal::Int64(value) => format!("{:#x}", *value),
                    IRImmVal::U8(value) => format!("{:#x}", *value),
                    IRImmVal::Str(value, ..) => value.clone(),
                    IRImmVal::Null => "#0".to_string(), // Null is just '0' under the hood. LOL
                }
            },

            IROperand::Temp { id, .. } => {
                let src_reg = self.temp_reg_map.reg_map.get(id).unwrap().clone();
                if src_reg.status == RegStatus::Spilled {
                    self.refill_stack.insert(src_reg.idx);
                }
                src_reg.name_aarch64()
            },

            IROperand::CallValue { position, size } => {
                let src_reg = AllocedReg {
                    idx: *position,
                    size: *size,
                    status: RegStatus::Free,
                };
                src_reg.name_aarch64()
            }

            IROperand::Return { position, size, .. } => {
                let reg = AllocedReg {
                    idx: *position,
                    size: *size,
                    status: RegStatus::Free
                };
                reg.name_aarch64()
            }

            _ => unimplemented!("{irlit:#?}")
        }
    }

    fn refill_spilled_registers(&mut self) -> String {
        let mut output = "".to_string();
        for rs in &self.refill_stack {
            output.push_str(&format!("LDR x{rs}, [SP, #0]\n"));
        }
        output
    }

    /// Get the compile time register mapping of an IR literal type. 
    /// Returns temporary ID with its mapped AllocedReg.
    fn resolve_register(&mut self, irlit: &IROperand) -> (TempId, AllocedReg) {
        match irlit {
            IROperand::Temp { 
                id, 
                size 
            } => (*id, self.get_or_allocate_temp_register(*id, *size)),
            
            IROperand::CallArg { 
                temp, 
                size, 
                position 
            } => (*temp, self.get_or_allocate_specific_register(*position, *temp, *size)),
            
            IROperand::Return { 
                position,
                temp,
                size
            } => (*temp, self.get_or_allocate_specific_register(*position, *temp, *size)),

            IROperand::Param { size, position } => {
                let reg = AllocedReg {
                    idx: *position,
                    size: *size,
                    status: RegStatus::Free
                };
                (0, reg)
            },

            IROperand::CallValue { size, position } => {
                let reg = AllocedReg {
                    idx: *position,
                    size: *size,
                    status: RegStatus::Free
                };
                (0, reg)
            },

            _ => {
                println!("{irlit:#?}");
                todo!()
            }
        }
    }
    
    fn get_or_allocate_temp_register(&mut self, temp_value: usize, sz: usize) -> AllocedReg {
        self.temp_reg_map
            .reg_map
            .get(&temp_value)
            .cloned()
            .unwrap_or_else(|| {
                let alloced_reg: AllocedReg = self.allocate_register(sz);
                self.temp_reg_map.reg_map.insert(temp_value, alloced_reg.clone());
                alloced_reg
            })
    }
    
    fn get_or_allocate_specific_register(&mut self, reg: RegIdx, temp: usize, sz: RegSize) -> AllocedReg {
        self.temp_reg_map
            .reg_map
            .get(&temp)
            .cloned()
            .unwrap_or_else(|| {
                let alloced_reg: AllocedReg = self.allocate_specific_register(reg, sz);
                self.temp_reg_map.reg_map.insert(temp, alloced_reg.clone());
                alloced_reg
            })
    }
    
    fn allocate_register(&mut self, sz: RegSize) -> AllocedReg {
        assert_ne!(sz, 0);
        let reg_mgr = &mut self.reg_manager;
        let reg = reg_mgr.allocate_register(sz);
        assert!(reg.size != 0);
        reg
    }
    
    fn allocate_specific_register(&mut self, reg: RegIdx, sz: RegSize) -> AllocedReg {
        assert_ne!(sz, 0);
        let reg_mgr = &mut self.reg_manager;
        let reg = reg_mgr.allocate_register_with_idx(sz, reg, AllocStrategy::Spill);
        assert!(reg.size != 0);
        reg
    }
}