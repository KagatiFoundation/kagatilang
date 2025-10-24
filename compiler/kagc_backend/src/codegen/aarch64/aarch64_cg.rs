// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::regalloc::aarch64::Aarch64RegMgr;
use crate::codegen::*;
use crate::reg::*;

use kagc_mir::ir_instr::*;
use kagc_mir::ir_liveness::LivenessAnalyzer;
use kagc_mir::ir_operands::IROperand;
use kagc_mir::ir_operands::TempId;
use kagc_mir::ir_operands::IRImmVal;
use kagc_mir::ir_operands::IRAddr;
use kagc_mir::LabelId;

use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::panic;
use std::rc::Rc;

use kagc_const::pool::ConstEntry;
use kagc_const::pool::KagcConst;
use kagc_ctx::CompilerCtx;
use kagc_symbol::StorageClass;
use kagc_types::builtins::obj::KObjType;

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

    spill_map: HashMap<TempId, usize>, // TempId --> (Reg, Stack Slot)

    /// Instruction Pointer:
    /// IP is used to track the execution of instructions inside 
    /// a function.
    func_ip: usize,

    /// Code generators state
    state: IRToASMState,

    /// Liveness analyzer
    la: LivenessAnalyzer,

    refill_stack: HashSet<(AllocedRegister, usize)>
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
            spill_map: HashMap::default(),
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
        let freed_reg: Option<AllocedRegister> = self.temp_reg_map.drop(&temp);

        if let Some(reg) = freed_reg {
            self.reg_manager.free_register(reg.idx);
        }
    }

    fn try_dropping_temp(&mut self, temp: usize) -> bool {
        if let Some(compt_fn_info) = &self.compt_fn_props {
            if let Some((start, end)) = compt_fn_info.liveness_info.get(temp) {
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
    fn gen_str_sp(&self, reg: &AllocedRegister, stack_size: usize, slot: usize) -> String {
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
    fn gen_ldr_sp(&self, reg: &AllocedRegister, stack_size: usize, slot: usize) -> String {
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
    fn gen_str_fp(&self, reg: &AllocedRegister, slot: usize) -> String {
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
    fn gen_ldr_fp(&self, reg: &AllocedRegister, slot: usize) -> String {
        let stack_off = slot * 8;
        let src_addr = if stack_off != 0 { // if not at the beginning of the frame pointer
            format!("[x29, -{stack_off:#x}]")
        }
        else {
            "[x29]".to_string()
        };
        format!("\tLDR {}, {src_addr}\n", reg.name_aarch64())
    }

    fn gen_reg_spill(&self, reg: &AllocedRegister, stack_size: usize, slot: usize) -> Option<String> {
        if reg.status != RegStatus::Spilled {
            return None;
        }
        Some(self.gen_str_sp(reg, stack_size, slot))
    }

    fn _gen_reg_unspill(&self, reg: &AllocedRegister, stack_size: usize, slot: usize) -> Option<String> {
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

        // reset everything
        self.reg_manager.reset();

        self.compt_fn_props = None;
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

            if !_body_asm.is_empty() {
                fn_body_asm.push_str(&format!("\t{}\n", _body_asm.trim()));
            }
        }

        output_str.push_str(&fn_body_asm);
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
        let mut output_str = "".to_string();
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
        let mut output_str = "".to_string();
        output_str.push_str(&format!("\n.global _{fn_label}\n_{fn_label}:\n"));
        output_str.push_str(&format!("\tSUB SP, SP, #{stack_size}\n"));
        output_str.push_str(&format!("\tSTP x29, x30, [SP, #{}]\n", stack_size - 16));
        output_str.push_str(&format!("\tADD x29, SP, #{}", stack_size - 16));
        output_str
    }
    
    fn gen_non_leaf_fn_epl(&self, stack_size: usize) -> String {
        let mut output_str = "".to_string();
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
                    &self.maybe_spill_register(
                        src_reg.0, 
                        &src_reg.1
                    ).unwrap_or_default()
                );
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

        let mut output = "".to_string();
        let (temp, src_reg) = self.resolve_register(src);
        output.push_str(&self.maybe_spill_register(temp, &src_reg).unwrap_or_default());
        
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
                let (temp, dest_reg) = self.resolve_register(base);
                output.push_str(&self.maybe_spill_register(temp, &dest_reg).unwrap_or_default());
                output.push_str(
                    &format!(
                        "\tSTR {}, [{}, {}]", 
                        src_reg.name_aarch64(), 
                        dest_reg.name_aarch64(), 
                        format_args!("#{}", off * 8)
                    )
                );
                output
            }
        }
    }

    fn gen_ir_fn_call_asm(
        &mut self,
        fn_name: String,
        _args: &[(usize, IROperand)],
        _return_type: &Option<IROperand>,
    ) -> String {
        self.try_dropping_all_temps();
        let mut out = String::new();

        let mut regs_to_spill: Vec<AllocedRegister> = Vec::new();
        for (reg_idx, reg) in Aarch64RegMgr::caller_saved_regs(REG_SIZE_8).iter().enumerate() {
            if reg_idx < _args.len() {
                continue;
            }
            if let Some(temp) = self.temp_reg_map.reverse_get(reg.clone()) {
                let must_spill = self.la.is_temp_alive_after(temp, self.func_ip);
                if must_spill {
                    regs_to_spill.push(reg.clone());
                    let ns = self.get_current_fn_props_mut().next_stack_slot();

                    // register must be spilled before another instruciton uses it
                    self.spill_map.insert(temp, ns);
                    // no temporary should hold this specific register now. it has 
                    // to be freed
                    self.temp_reg_map.drop(&temp);
                    // enjoy your freedom, lovely register
                    self.reg_manager.free_register(reg_idx);
                }
            }
        }

        if regs_to_spill.is_empty() {
            if let Some(ret) = _return_type {
                let reg = self.resolve_register(ret);
                out.push_str(&self.maybe_spill_register(reg.0, &reg.1).unwrap_or_default());
            }
            out.push_str(&format!("\tBL _{fn_name}\n"));
            return out;
        }

        let dyn_size = regs_to_spill.len() * 8;
        if dyn_size > 0 {
            out.push_str(&format!("\tSUB SP, SP, #{}\n", dyn_size));
        }

        let mut stack_offset = 0usize;
        for tr in &regs_to_spill {
            out.push_str(&format!(
                "\tSTR {}, [SP, #{}]\n",
                tr.name_aarch64(),
                stack_offset
            ));
            stack_offset += 8;
            self.reg_manager.free_register(tr.idx);
        }

        if let Some(ret) = _return_type {
            let reg = self.resolve_register(ret);
            out.push_str(&self.maybe_spill_register(reg.0, &reg.1).unwrap_or_default());
        }

        out.push_str(&format!("\tBL _{fn_name}\n"));
        out
    }
    
    fn gen_ir_mov_asm(&mut self, dest: &IROperand, src: &IROperand) -> String {
        let mut output_str = "".to_string();
        let dest_reg = self.resolve_register(dest);
        output_str.push_str(
            &self.maybe_spill_register(
                dest_reg.0, 
                &dest_reg.1
            ).unwrap_or_default()
        );

        let reg_name: String = dest_reg.1.name_aarch64();
        let operand: String = self.operand_to_string(src);
        output_str.push_str(&self.refill_spilled_registers());

        if reg_name == operand { "".to_string() }
        else { format!("\tMOV {}, {}", reg_name, operand) }
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
        let dest_reg: (usize, AllocedRegister) = self.resolve_register(dest);
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
        output_str.push_str(&format!("\tCMP {}, {}\n", self.operand_to_string(op1), self.operand_to_string(op2)));
        output_str.push_str(&format!("{compare_operator} .LB_{label_id}"));
        output_str
    }

    fn gen_ir_mem_alloc(&mut self, size: usize, ob_type: &KObjType) -> String {
        let mut output = "".to_string();
        {
            // load the parameter
            let x0 = self.allocate_specific_register(0, REG_SIZE_8);
            output.push_str(
                &self.maybe_spill_register(
                    0xFFFFFFFF, 
                    &x0
                ).unwrap_or_default()
            );
            output.push_str(&format!("MOV {}, {:#x}", x0.name_aarch64(), size));
        }
        
        {
            let x1 = self.allocate_specific_register(1, REG_SIZE_8);
            output.push_str(
                &self.maybe_spill_register(
                    0xFFFFFFF0, 
                    &x1
                ).unwrap_or_default()
            );
            output.push_str(&format!("MOV {}, {:#x}", x1.name_aarch64(), ob_type.value()));
        }

        output.push_str(
        "BL _object_new
        CBNZ x0, 1f
        BL _obj_alloc_fail
        1:
        " 
        );
        output
    }

    fn gen_ir_mem_cpy(&mut self) -> String {
        "\tBL _k_memcpy\n".to_string()
    }
}

impl Aarch64Codegen {
    fn gen_ir_bin_op_asm(&mut self, dest: &IROperand, op1: &IROperand, op2: &IROperand, operation: &str) -> String {
        let mut output = "".to_string();
        let dest_reg = self.resolve_register(dest);
        let reg_name: String = dest_reg.1.name_aarch64();

        output.push_str(
            &self.maybe_spill_register(
                dest_reg.0, 
                &dest_reg.1
            ).unwrap_or_default()
        );

        let op1: String = self.operand_to_string(op1);
        let op2: String = self.operand_to_string(op2);
        output.push_str(&self.refill_spilled_registers());

        output.push_str(
            &format!(
                "\t{} {}, {}, {}",
                operation, reg_name, 
                op1, op2
            )
        );
        output
    }

    fn operand_to_string(&mut self, op: &IROperand) -> String {
        match op {
            IROperand::Const(irlit_val) => {
                match irlit_val {
                    IRImmVal::Int32(value) => format!("{:#x}", *value),
                    IRImmVal::Int64(value) => format!("{:#x}", *value),
                    IRImmVal::U8(value) => format!("{:#x}", *value),
                    IRImmVal::Str(value, ..) => value.clone(),
                    IRImmVal::Null => "#0".to_string(), // Null is just '0' under the hood. LOL
                }
            },

            IROperand::Temp { id, .. } 
            | IROperand::CallArg { temp: id, .. }
            | IROperand::CallValue { temp: id, .. } => {
                if let Some(&reg_stack) = self.spill_map.get(id) {
                    let src_reg = self.resolve_register(&op.clone()).1;
                    self.refill_stack.insert((src_reg.clone(), reg_stack));
                    return src_reg.name_aarch64();
                }
                else if let Some(src_reg) = self.temp_reg_map.reg_map.get(id) {
                    return src_reg.name_aarch64();
                }
                panic!("Impossible scenario! Value '{op:#?}' is nowhere to be found :D! Aborting...");
            }

            IROperand::Return { size, .. } => {
                let reg = AllocedRegister {
                    idx: 0,
                    size: *size,
                    status: RegStatus::Free
                };
                reg.name_aarch64()
            }

            _ => unimplemented!("{op:#?}")
        }
    }

    fn maybe_spill_register(&mut self, temp: TempId, reg: &AllocedRegister) -> Option<String> {
        if reg.status != RegStatus::Spilled {
            return None;
        }
        let (is_leaf, stack_slot) = {
            let compt_props = self.get_current_fn_props_mut();
            (compt_props.is_leaf, compt_props.next_stack_slot())
        };

        self.temp_reg_map.drop(&temp); // drop the register holding this temporary
        self.spill_map.insert(temp, stack_slot); // mark the spot for the dropped temporary in the stack

        if is_leaf {
            Some(self.gen_str_sp(reg, 0, stack_slot))
        }
        else {
            Some(self.gen_str_fp(reg, 0))
        }
    }

    fn refill_spilled_registers(&mut self) -> String {
        let mut output = "".to_string();
        for (reg, stack_slot) in &self.refill_stack {
            output.push_str(
                &format!(
                    "\tLDR {name}, [SP, #{off}]\n", 
                    name = reg.name_aarch64(),
                    off = *stack_slot * 8
                )
            );
        }
        output
   }

    /// Get the compile time register mapping of an IR literal type. 
    /// Returns temporary ID with its mapped AllocedReg.
    fn resolve_register(&mut self, irlit: &IROperand) -> (TempId, AllocedRegister) {
        match irlit {
            IROperand::Temp { 
                id, 
                size 
            } => (*id, self.get_or_allocate_temp_register(*id, *size)),
            
            IROperand::CallArg { 
                temp, 
                size, 
                position 
            } => {
                (*temp, self.get_or_allocate_specific_register(*position, *temp, *size))
            }
            
            IROperand::Return { 
                temp,
                ..
            } => {
                let src = AllocedRegister {
                    idx: 0,
                    size: REG_SIZE_8,
                    status: RegStatus::Free
                };
                (*temp, src)
            }

            IROperand::Param { size, position } => {
                let reg = AllocedRegister {
                    idx: *position,
                    size: *size,
                    status: RegStatus::Free
                };
                (0, reg)
            },

            IROperand::CallValue { 
                temp,
                size, 
                ..
            } => (*temp, self.get_or_allocate_specific_register(0, *temp, *size)),

            _ => {
                println!("{irlit:#?}");
                todo!()
            }
        }
    }
    
    fn get_or_allocate_temp_register(&mut self, temp_value: usize, sz: usize) -> AllocedRegister {
        self.temp_reg_map
            .reg_map
            .get(&temp_value)
            .cloned()
            .unwrap_or_else(|| {
                let alloced_reg: AllocedRegister = self.allocate_register(sz);
                self.temp_reg_map.reg_map.insert(temp_value, alloced_reg.clone());
                alloced_reg
            })
    }
    
    fn get_or_allocate_specific_register(&mut self, reg: RegIdx, temp: usize, sz: RegSize) -> AllocedRegister {
        self.temp_reg_map
            .reg_map
            .get(&temp)
            .cloned()
            .unwrap_or_else(|| {
                let alloced_reg: AllocedRegister = self.allocate_specific_register(reg, sz);
                self.temp_reg_map.reg_map.insert(temp, alloced_reg.clone());
                alloced_reg
            })
    }
    
    fn allocate_register(&mut self, sz: RegSize) -> AllocedRegister {
        assert_ne!(sz, 0);
        let reg_mgr = &mut self.reg_manager;
        let reg = reg_mgr.allocate_register(sz);
        assert!(reg.size != 0);
        reg
    }
    
    fn allocate_specific_register(&mut self, reg: RegIdx, sz: RegSize) -> AllocedRegister {
        assert_ne!(sz, 0);
        let reg_mgr = &mut self.reg_manager;
        let reg = reg_mgr.allocate_register_with_idx(sz, reg, AllocStrategy::Spill);
        assert!(reg.size != 0);
        reg
    }
}