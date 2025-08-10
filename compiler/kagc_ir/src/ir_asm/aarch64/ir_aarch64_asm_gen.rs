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

use std::{
    cell::RefCell, 
    collections::HashMap, 
    hash::Hash, 
    rc::Rc
};

use kagc_const::pool::{ConstEntry, KagcConst};
use kagc_ctx::CompilerCtx;
use kagc_symbol::{StorageClass, Symbol};
use kagc_target::{asm::aarch64::*, reg::*};
use kagc_types::{LitType, LitTypeVariant};

use crate::{
    ir_asm::ir_asm_gen::*, 
    ir_instr::*, 
    ir_liveness::*, 
    ir_types::*, LabelId
};

#[derive(Debug)]
struct TempRegMap<KT: Eq + Hash> {
    pub reg_map: HashMap<KT, AllocedReg>
}

impl<KT: Eq + Hash> Default for TempRegMap<KT> {
    fn default() -> Self {
        Self {
            reg_map: HashMap::new()
        }
    }
}

impl<KT: Eq + Hash> TempRegMap<KT> {
    pub fn drop(&mut self, key: &KT) -> Option<AllocedReg> {
        self.reg_map.remove(key)
    }
}


/// Represents properties of a compiled function.
/// 
/// - `is_leaf`: Indicates whether the function is a leaf function 
///   (i.e., it makes no function calls).
/// - `stack_size`: The amount of stack space allocated for this function.
#[derive(Debug, Clone)]
struct ComptFnProps {
    pub is_leaf: bool,
    pub stack_size: usize,
    pub liveness_info: HashMap<usize, LiveRange>,
}

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
pub struct Aarch64IRToASM {
    pub ctx: Rc<RefCell<CompilerCtx>>,

    reg_manager: Rc<RefCell<Aarch64RegManager2>>,

    compt_fn_props: Option<ComptFnProps>,

    temp_reg_map: TempRegMap<usize>,

    /// Instruction Pointer:
    /// IP is used to track the execution of instructions inside 
    /// a function.
    func_ip: usize,

    /// Code generators state
    state: IRToASMState,
}

impl Aarch64IRToASM {
    #[allow(clippy::new_without_default)]
    pub fn new(ctx: Rc<RefCell<CompilerCtx>>, rm: Rc<RefCell<Aarch64RegManager2>>) -> Self {
        Self {
            ctx,
            reg_manager: rm,
            compt_fn_props: None,
            temp_reg_map: TempRegMap { reg_map: HashMap::new() },
            func_ip: 0,
            state: IRToASMState::Global
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

    /// Switches to the given state returning the old state.
    fn switch_cg_state(&mut self, new_state: IRToASMState) -> IRToASMState {
        let old: IRToASMState = self.state;
        self.state = new_state;
        old
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
    fn compute_stack_size_fn_ir(&self, func_ir: &IRFunc) -> Option<usize> {
        let mut stack_size: usize = func_ir.params.len() * 8; // Each parameter is 8 bytes
    
        if !func_ir.is_leaf {
            stack_size += 16; // Space for x29 and x30
        }
    
        for ir in &func_ir.body {
            if let IR::VarDecl(_) = ir {
                stack_size += 8; // Each variable takes 8 bytes
            }
            else if let IR::Instr(IRInstr::Store { .. }) = ir {
                stack_size += 8;
            }
        }
    
        Some(Aarch64IRToASM::align_to_16(stack_size))
    }

    /// Align the given address into an address divisible by 16.
    fn align_to_16(value: usize) -> usize {
        (value + 16 - 1) & !15
    }

    fn gen_fn_param_asm(&self, param: &IRLitType, stack_size: usize, is_leaf_fn: bool) -> String {
        match param {
            IRLitType::Reg { idx, size } => {
                let name = self.reg_manager.borrow().name(*idx, *size);
                let stack_off: usize = (*idx * 8) + 8;
                if is_leaf_fn {
                    format!("STR {name}, [SP, #{}]", stack_size - stack_off)
                }
                else {
                    format!("STR {name}, [x29, #-{stack_off}]")
                }
            },
            _ => unimplemented!()
        }
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
    }

    fn drop_temp(&mut self, temp: usize) {
        let freed_reg: Option<AllocedReg> = self.temp_reg_map.drop(&temp);

        if let Some(reg) = freed_reg {
            self.reg_manager.borrow_mut().free_register(reg.idx);
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
        let mut output_str: String = String::new();
        output_str.push_str(".data\n.align 3\n");
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
                output_str.push_str(&format!(".L.str.{}:\n\t.asciz \"{}\"\n", c_item_index, str_value));
            }
        }
        else if let KagcConst::Int(int_value) = &c_item.value {
            if parent_is_record {
                output_str.push_str(&format!("\t.word {int_value}\n\t.zero 4\n"));
            }
        }
        else if let KagcConst::Record(rec) = &c_item.value {
            output_str.push_str(
                &format!(
                    ".align {}\n.L__const.{}.{}:\n", 
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

    fn _dump_global_with_alignment(symbol: &Symbol) -> String {
        let def_val: String = if let Some(dv) = &symbol.default_value {
            dv.to_string()
        } 
        else { 
            "0".to_string() 
        };

        match symbol.lit_type {
            LitTypeVariant::I32 => format!("{}: .align 4\n\t.word {}", symbol.name, def_val),

            LitTypeVariant::U8 => format!("{}:\t.byte {}", symbol.name, def_val),

            LitTypeVariant::Str => {
                let label_id: i32 = if let Some(lit_val) = &symbol.default_value {
                    if let LitType::I32(__id) = lit_val {
                        *__id
                    }
                    else {
                        panic!("Not a valid label id for string literal '{}'", symbol.default_value.as_ref().unwrap())
                    }
                } 
                else {
                    panic!("No label id provided for string literal");
                };

                format!("{}:\t.quad .LB_{}", symbol.name, label_id)
            },

            _ => panic!("Symbol's size is not supported right now: '{:?}'", symbol),
        }
    }
    
    fn _alloc_data_space(size: usize) -> String {
        match size {
            1 => ".byte 0".to_string(),
            4 => ".word 0".to_string(),
            8 => ".quad 0".to_string(),
            _ => panic!("Not possible to generate space for size: {}", size),
        }
    }
}

impl IRToASM for Aarch64IRToASM {
    fn gen_ir_fn_asm(&mut self, fn_ir: &mut IRFunc) -> String {
        if fn_ir.class == StorageClass::EXTERN {
            // self.advance_ip();
            return format!(".extern _{}\n", fn_ir.name);
        }

        let mut output_str: String = "".to_string();

        // Temporary liveness information of this function
        let temp_liveness: HashMap<usize, LiveRange> = LivenessAnalyzer::analyze_fn_temps(fn_ir);

        let stack_size: usize = self.compute_stack_size_fn_ir(fn_ir).unwrap();

        if fn_ir.is_leaf {
            output_str.push_str(&format!("{}\n", self.gen_leaf_fn_prol(&fn_ir.name, stack_size)));
        }
        else {
            output_str.push_str(&format!("{}\n", self.gen_non_leaf_fn_prol(&fn_ir.name, stack_size)));
        }

        for param in &fn_ir.params {
            output_str.push_str(&format!("{}\n", self.gen_fn_param_asm(param, stack_size, fn_ir.is_leaf)));
        }

        // generate the code for function body
        self.switch_to_func_scope(
            ComptFnProps { 
                is_leaf: fn_ir.is_leaf, 
                stack_size,
                liveness_info: temp_liveness,
            }
        );

        for body_ir in &mut fn_ir.body {
            let body_asm: String = self.gen_asm_from_ir_node(body_ir);
            self.try_dropping_all_temps();
            self.advance_ip();
            output_str.push_str(&format!("{}\n", body_asm.trim()));
        }

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
    
    fn gen_ir_local_var_decl_asm(&mut self, vdecl_ir: &IRVarDecl) -> String {
        let mut output_str: String = "".to_string();

        let stack_off: usize = vdecl_ir.offset.unwrap_or_else(|| panic!("Local variables must have stack offset!"));

        let value_reg: AllocedReg = match &vdecl_ir.value {
            IRLitType::ExtendedTemp { id, .. } => {
                let temp_reg: AllocedReg = self.temp_reg_map.reg_map.get(id).unwrap().clone();
                temp_reg
            }

            // IRLitType::AllocReg { temp, reg } => {
            //     println!("Reg: {reg}");
            //     self.get_or_allocate_specific_register(*reg, *temp, 64)
            // },
            
            _ => todo!()
        };

        // since we are parsing a local variable, the compile-time function props cannot be None
        let fn_props: &ComptFnProps = self.compt_fn_props.as_ref().unwrap_or_else(|| panic!("Compile time information not available for the function!"));
        
        if !fn_props.is_leaf {
            output_str.push_str(&format!("STR {}, [x29, #-{}]", value_reg.name(), (stack_off * 8) + 8));
        }
        else {
            output_str.push_str(&format!("STR {}, [SP, #{}]", value_reg.name(), fn_props.stack_size - ((stack_off * 8) + 8)));
        }

        output_str
    }
    
    fn gen_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String {
        let mut output_str: String = "".to_string();
        output_str.push_str(&format!("\n.global _{fn_label}\n_{fn_label}:"));

        if stack_size != 0 {
            output_str.push_str(&format!("\nSUB SP, SP, #{stack_size}"));
        }
        output_str
    }
    
    fn gen_leaf_fn_epl(&self, stack_size: usize) -> String {
        if stack_size != 0 {
            format!("ADD SP, SP, #{stack_size}\nRET\n")
        }
        else {
            "RET\n".to_string()
        }
    }

    fn gen_non_leaf_fn_prol(&self, fn_label: &str, stack_size: usize) -> String {
        let mut output_str: String = "".to_string();
        output_str.push_str(&format!("\n.global _{fn_label}\n_{fn_label}:\n"));
        output_str.push_str(&format!("SUB SP, SP, #{stack_size}\n"));
        output_str.push_str(&format!("STP x29, x30, [SP, #{}]\n", stack_size - 16));
        output_str.push_str(&format!("ADD x29, SP, #{}", stack_size - 16));
        output_str
    }
    
    fn gen_non_leaf_fn_epl(&self, stack_size: usize) -> String {
        let mut output_str: String = "".to_string();
        output_str.push_str(&format!("LDP x29, x30, [SP, #{}]\n", stack_size - 16));
        output_str.push_str(&format!("ADD SP, SP, #{}\nRET\n", stack_size));
        output_str
    }
    
    fn gen_asm_load(&mut self, dest: &IRLitType, addr: &IRAddr) -> String {
        let (stack_size, is_leaf_fn) = if let Some(func_props) = &self.compt_fn_props {
            (func_props.stack_size, func_props.is_leaf)
        }
        else {
            panic!("Trying to load value from the stack outside of a function!");
        };

        let dest_reg = self.resolve_register(dest);
        match addr {
            IRAddr::StackOff(stack_off) => {
                let soff: usize = stack_off * 8;
                if is_leaf_fn {
                    format!("LDR {}, [SP, #{}]", dest_reg.1.name(), stack_size - soff)
                }
                else {
                    format!("LDR {}, [x29, #-{}]", dest_reg.1.name(), soff + 8)
                }
            },
            IRAddr::BaseOff(base, off) => {
                let src_reg = self.resolve_register(base);
                format!("LDR {}, [{}, {}]", dest_reg.1.name(), src_reg.1.name(), format_args!("#{}", *off * 8))
            }
        }
    }

    fn gen_asm_store(&mut self, src: &IRLitType, addr: &IRAddr) -> String {
        let (stack_size, is_leaf_fn) = if let Some(func_props) = &self.compt_fn_props {
            (func_props.stack_size, func_props.is_leaf)
        }
        else {
            panic!("Trying to load value from the stack outside of a function!");
        };

        let src_reg = self.resolve_register(src);
        match addr {
            IRAddr::StackOff(stack_off) => {
                let soff: usize = (stack_off * 8) + 8;
                if is_leaf_fn {
                    format!("STR {}, [SP, #{}]", src_reg.1.name(), stack_size - soff)
                }
                else {
                    format!("STR {}, [x29, #-{}]", src_reg.1.name(), soff)
                }
            },
            IRAddr::BaseOff(base, off) => {
                let dest_reg = self.resolve_register(base);
                format!("STR {}, [{}, {}]", src_reg.1.name(), dest_reg.1.name(), format_args!("#-{}", off * 8))
            }
        }
    }

    fn gen_ir_fn_call_asm(&mut self, fn_name: String, _: &[(usize, IRLitType)], _return_type: &Option<IRLitType>) -> String {
        let mut output_str: String = String::new();
        output_str.push_str(&format!("BL _{fn_name}"));
        output_str
    }
    
    fn gen_ir_mov_asm(&mut self, dest: &IRLitType, src: &IRLitType) -> String {
        let dest_reg: (usize, AllocedReg) = self.resolve_register(dest);
        let reg_name: String = dest_reg.1.name();

        let operand: String = self.extract_operand(src);
        format!("MOV {}, {}", reg_name, operand)
    }
    
    fn gen_ir_add_asm(&mut self, dest: &IRLitType, op1: &IRLitType, op2: &IRLitType) -> String {
        self.gen_ir_bin_op_asm(dest, op1, op2, "ADD")
    }

    fn gen_ir_sub_asm(&mut self, dest: &IRLitType, op1: &IRLitType, op2: &IRLitType) -> String {
        self.gen_ir_bin_op_asm(dest, op1, op2, "SUB")
    }
    
    fn gen_ir_mul_asm(&mut self, dest: &IRLitType, op1: &IRLitType, op2: &IRLitType) -> String {
        self.gen_ir_bin_op_asm(dest, op1, op2, "MUL")
    }
    
    fn gen_ir_div_asm(&mut self, dest: &IRLitType, op1: &IRLitType, op2: &IRLitType) -> String {
        self.gen_ir_bin_op_asm(dest, op1, op2, "DIV")
    }
    
    fn start_func_call_proc(&mut self) -> String {
        self.switch_cg_state(IRToASMState::FuncCall);
        NO_INSTR.to_string()
    }
    
    fn stop_func_call_proc(&mut self) -> String {
        // function calls happen only inside the local scope; thus, 
        // switch to local scope after a function call finishes
        self.switch_cg_state(IRToASMState::Local);

        NO_INSTR.to_string()
    }
    
    fn gen_ir_jump_asm(&mut self, label_id: usize) -> String {
        format!("B .LB_{label_id}")
    }

    fn gen_ir_loop_asm(&mut self, ir_loop: &mut IRLoop) -> String {
        let mut output_str: String = String::new();

        for body_ir in &mut ir_loop.body {
            output_str.push_str(&format!("{}\n", self.gen_asm_from_ir_node(body_ir)));
            self.advance_ip();
        }

        output_str
    }

    fn gen_ir_label_asm(&mut self, ir_label: &IRLabel) -> String {
        format!(".LB_{}:", ir_label.0)
    }

    fn gen_load_global_asm(&mut self, pool_idx: usize, dest: &IRLitType) -> String {
        let dest_reg: (usize, AllocedReg) = self.resolve_register(dest);
        let dest_reg_name: &str = &dest_reg.1.name();

        let mut output_str: String = String::new();
        if let Some(c_item) = self.ctx.borrow().const_pool.get(pool_idx) {
            match &c_item.value {
                KagcConst::Str(_) => {
                    output_str.push_str(&format!("ADRP {dest_reg_name}, .L.str.{pool_idx}@PAGE\n"));
                    output_str.push_str(&format!("ADD {dest_reg_name}, {dest_reg_name}, .L.str.{pool_idx}@PAGEOFF"));
                },
                KagcConst::Record(rec_value) => {
                    output_str.push_str(
                        &format!(
                            "ADRP {dest_reg_name}, .L__const.{}.{}@PAGE\n", 
                            c_item.origin_func.unwrap(), 
                            rec_value.alias.clone()
                        )
                    );
                    output_str.push_str(
                        &format!(
                            "ADD {dest_reg_name}, {dest_reg_name}, .L__const.{}.{}@PAGEOFF\n", 
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

    fn gen_cond_jmp_asm(&mut self, op1: &IRLitType, op2: &IRLitType, operation: IRCondOp, label_id: LabelId) -> String {
        let compare_operator: &str = match operation {
            IRCondOp::IRLThan => "BGE",
            IRCondOp::IRGThan => "BLE",
            IRCondOp::IREqEq => "BNE",
            IRCondOp::IRNEq => "BEQ",
            IRCondOp::IRGTEq => "BLT",
            IRCondOp::IRLTEq => "BGT"
        };
        
        let mut output_str: String = String::new();
        output_str.push_str(&format!("CMP {}, {}\n", self.extract_operand(op1), self.extract_operand(op2)));
        output_str.push_str(&format!("{compare_operator} .LB_{label_id}"));
        output_str
    }
}

impl Aarch64IRToASM {
    fn gen_ir_bin_op_asm(&mut self, dest: &IRLitType, op1: &IRLitType, op2: &IRLitType, operation: &str) -> String {
        let dest_reg = self.resolve_register(dest);
        let reg_name: String = dest_reg.1.name();

        let op1: String = self.extract_operand(op1);
        let op2: String = self.extract_operand(op2);

        format!(
            "{} {}, {}, {}",
            operation,
            reg_name, 
            op1, 
            op2
        ) 
    }

    /// Extract the IRLitType as an operand(String)
    fn extract_operand(&mut self, irlit: &IRLitType) -> String {
        match irlit {
            IRLitType::Const(irlit_val) => {
                match irlit_val {
                    IRLitVal::Int32(value) => value.to_string(),
                    IRLitVal::U8(value) => value.to_string(),
                    IRLitVal::Str(value, ..) => value.clone(),
                    IRLitVal::Null => "#0".to_string(), // Null is just '0' under the hood. LOL
                    _ => todo!()
                }
            },
            
            IRLitType::Reg { idx, .. } => format!("x{}", idx),

            IRLitType::ExtendedTemp { id, .. } => {
                let src_reg: AllocedReg = self.temp_reg_map.reg_map.get(id).unwrap().clone();
                src_reg.name()
            },

            _ => unimplemented!()
        }
    }

    /// Get the compile time register mapping of a IR literal type. 
    /// Returns temporary ID with it's mapped AllocedReg.
    fn resolve_register(&mut self, irlit: &IRLitType) -> (TempId, AllocedReg) {
        match irlit {
            IRLitType::ExtendedTemp { id, size } => (*id, self.get_or_allocate_temp_register(*id, *size)),
            IRLitType::AllocReg { reg, temp } => (*temp, self.get_or_allocate_specific_register(*reg, *temp, 64)),
            IRLitType::Reg { idx, size } => (0, AllocedReg { 
                size: *size, 
                idx: *idx, 
                width: RegWidth::QWORD,
                status: RegStatus::Alloced 
            }),
            _ => todo!(),
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
        let mut reg_mgr = self.reg_manager.borrow_mut();
        reg_mgr.allocate_register(sz)
    }
    
    fn allocate_specific_register(&mut self, reg: RegIdx, sz: RegSize) -> AllocedReg {
        let mut reg_mgr = self.reg_manager.borrow_mut();
        reg_mgr.allocate_register_with_idx(sz, reg, AllocStrategy::Spill)
    }
}