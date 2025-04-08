use std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc};

use kagc_ctx::CompilerCtx;
use kagc_symbol::{StorageClass, Symbol, SymbolType};
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
///             (i.e., it makes no function calls).
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
///         which manages global state.
/// 
/// - `reg_manager`: A reference-counted, mutable register manager for 
///                 AArch64, handling register allocation.
/// 
/// - `compt_fn_props`: Optional function properties, used for tracking 
///                     whether a function is a leaf and its stack size.
pub struct Aarch64IRToASM<'asmgen> {
    pub ctx: Rc<RefCell<CompilerCtx<'asmgen>>>,

    reg_manager: Rc<RefCell<Aarch64RegManager2>>,

    compt_fn_props: Option<ComptFnProps>,

    temp_reg_map: TempRegMap<usize>,

    /// Instruction Pointer:
    /// IP is used to track the execution of instructions inside 
    /// a function.
    ip: usize,

    /// Code generators state
    state: IRToASMState,
}

impl<'asmgen> Aarch64IRToASM<'asmgen> {
    #[allow(clippy::new_without_default)]
    pub fn new(ctx: Rc<RefCell<CompilerCtx<'asmgen>>>, rm: Rc<RefCell<Aarch64RegManager2>>) -> Self {
        Self {
            ctx,
            reg_manager: rm,
            compt_fn_props: None,
            temp_reg_map: TempRegMap { reg_map: HashMap::new() },
            ip: 0,
            state: IRToASMState::Global
        }
    }

    pub fn gen_asm(&mut self, irs: &mut [IR]) -> Vec<String> {
        // println!("{:#?}", irs);
        // return vec![];

        let mut output: Vec<String> = vec![];
        output.push(self.dump_global_strings());
        output.push(String::from(".text"));

        for ir in irs {
            let output_str: String = self.gen_asm_from_ir_node(ir);
            output.push(output_str);
        }
        output
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
        }
    
        Some(Aarch64IRToASM::align_to_16(stack_size))
    }
    

    /// Align the given address into an address divisible by 16.
    fn align_to_16(value: usize) -> usize {
        (value + 16 - 1) & !15
    }

    fn gen_fn_param_asm(&self, param: &IRLitType, stack_size: usize, is_leaf_fn: bool) -> String {
        match param {
            IRLitType::Reg(alloced_reg) => {
                let stack_off: usize = (*alloced_reg * 8) + 8;
                if is_leaf_fn {
                    format!("STR x{}, [sp, #{}]", alloced_reg, stack_size - stack_off)
                }
                else {
                    format!("STR x{}, [x29, #-{}]", alloced_reg, stack_off)
                }
            },
            _ => unimplemented!()
        }
    }

    #[inline]
    fn advance_ip(&mut self) {
        self.ip += 1;
    }

    #[inline]
    fn reset(&mut self) {
        // reset IP before moving into into another function
        self.ip = 0;

        // stay on the global scope; no function available
        self.switch_to_global_scope();
    }

    fn drop_temp(&mut self, temp: usize) {
        let freed_reg: Option<AllocedReg> = self.temp_reg_map.drop(&temp);
        // println!("Attempting to free: {} --> {:#?}", temp, freed_reg);

        if let Some(reg) = freed_reg {
            self.reg_manager.borrow_mut().free_register(reg.idx);
        }
        else {
            panic!("Problem while freeing temporary {}!", temp);
        }
    }

    fn try_dropping_temp(&mut self, temp: usize) {
        if let Some(compt_fn_info) = &self.compt_fn_props {
            if let Some((start, end)) = compt_fn_info.liveness_info.get(&temp) {
                // temporary's life is over
                if (*start + *end) <= self.ip {
                    self.drop_temp(temp);
                }
            }
        }
        else {
            panic!("Compile time information not avaailable for function!");
        }
    }

    fn dump_global_strings(&self) -> String {
        let ctx_borrow = self.ctx.borrow();

        // output
        let mut output_str: String = String::new();

        for symbol in ctx_borrow.sym_table.iter() {
            // ignore non-global constants
            if (symbol.lit_type == LitTypeVariant::None) || symbol.sym_type == SymbolType::Function || symbol.class != StorageClass::GLOBAL {
                continue;
            }

            if symbol.lit_type == LitTypeVariant::Str && symbol.sym_type == SymbolType::Constant {
                let str_const_name_and_val: Vec<&str> = symbol.name.split("---").collect::<Vec<&str>>();
                let str_const_name: String = String::from(str_const_name_and_val[0]);
                
                output_str.push_str(&format!(".data\n.global {str_const_name}\n"));
                output_str.push_str(&format!("{str_const_name}:\n\t.asciz \"{}\"\n", str_const_name_and_val[1]));

                continue;
            }

            output_str.push_str(&format!(".data\n.global {}", symbol.name));
            
            if symbol.sym_type == SymbolType::Variable {
                output_str.push_str(&Self::dump_global_with_alignment(symbol));
            } 
            else if symbol.sym_type == SymbolType::Array {
                let array_data_size: usize = symbol.lit_type.size();
                
                output_str.push_str(&format!("{}:", symbol.name));

                for _ in 0..symbol.size {
                    output_str.push_str(&Self::alloc_data_space(array_data_size));
                }
            }
        }

        output_str
    }

    fn dump_global_with_alignment(symbol: &Symbol) -> String {
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

                format!("{}:\t.quad ._L{:?}", symbol.name, label_id)
            },

            _ => panic!("Symbol's size is not supported right now: '{:?}'", symbol),
        }
    }
    
    fn alloc_data_space(size: usize) -> String {
        match size {
            1 => ".byte 0".to_string(),
            4 => ".word 0".to_string(),
            8 => ".quad 0".to_string(),
            _ => panic!("Not possible to generate space for size: {}", size),
        }
    }
}

impl<'irgen> IRToASM for Aarch64IRToASM<'irgen> {
    fn gen_ir_fn_asm(&mut self, fn_ir: &mut IRFunc) -> String {
        if fn_ir.class == StorageClass::EXTERN {
            return format!(".extern _{}\n", fn_ir.name);
        }

        let mut output_str: String = "".to_string();

        // SpillCompilerPass::analyze_fn_for_spills(fn_ir);

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

            // maybe I should increment the IP here???
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

    fn gen_ir_return_asm(&mut self, ir_return: &IRReturn) -> String {
        format!("hello")
    }
    
    fn gen_ir_local_var_decl_asm(&mut self, vdecl_ir: &IRVarDecl) -> String {
        let mut output_str: String = "".to_string();

        let stack_off: usize = vdecl_ir.offset.unwrap_or_else(|| panic!("Local variables must have stack offset!"));

        let value_reg: AllocedReg = match &vdecl_ir.value {
            IRLitType::Temp(temp_value) => {
                let temp_reg: AllocedReg = self.temp_reg_map.reg_map.get(temp_value).unwrap().clone();
                self.try_dropping_temp(*temp_value);
                temp_reg
            },

            IRLitType::AllocReg { temp, reg } => {
                let temp_reg = self.get_or_allocate_specific_register(*reg, *temp);
                self.try_dropping_temp(*temp);
                temp_reg
            },
            
            _ => todo!()
        };

        // since we are parsing a local variable, then compile-time function props is not None
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
        output_str.push_str(&format!("sub sp, sp, #{stack_size}\n"));
        output_str.push_str(&format!("stp x29, x30, [sp, #{}]\n", stack_size - 16));
        output_str.push_str(&format!("add x29, sp, #{}", stack_size - 16));

        output_str
    }
    
    fn gen_non_leaf_fn_epl(&self, stack_size: usize) -> String {
        let mut output_str: String = "".to_string();

        output_str.push_str(&format!("ldp x29, x30, [sp, #{}]\n", stack_size - 16));
        output_str.push_str(&format!("add sp, sp, #{}\nret\n", stack_size));

        output_str
    }
    
    fn gen_asm_load(&mut self, dest: &IRLitType, stack_off: usize) -> String {
        let dest_reg = self.resolve_register(dest);
        
        let (stack_size, is_leaf_fn) = if let Some(func_props) = &self.compt_fn_props {
            (func_props.stack_size, func_props.is_leaf)
        }
        else {
            panic!("Trying to load value from the stack outside of a function!");
        };

        let soff: usize = (stack_off * 8) + 8;
        
        if is_leaf_fn {
            format!("LDR {}, [sp, #{}]", dest_reg.1.name(), stack_size - soff)
        }
        else {
            format!("LDR {}, [x29, #-{}]", dest_reg.1.name(), soff)
        }
    }

    fn gen_asm_store(&mut self, src: &IRLitType, stack_off: usize) -> String {
        format!("STR {:?}", src.as_reg())
    }

    fn gen_ir_fn_call_asm(&mut self, fn_name: String, params: &[(usize, IRLitType)], return_type: &Option<IRLitType>) -> String {
        let mut output_str: String = String::new();

        for (_, param) in params{
            if let IRLitType::Temp(temp) = param {
                self.try_dropping_temp(*temp);
            }
        }

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
        let dest_reg = self.resolve_register(dest);
        let reg_name: String = dest_reg.1.name();

        let op1: String = self.extract_operand(op1);
        let op2: String = self.extract_operand(op2);

        format!(
            "ADD {}, {}, {}",
            reg_name, 
            op1, 
            op2
        )
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
        format!("B _L{}", label_id)
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
        format!("_L{}:", ir_label.0)
    }

    fn gen_load_global_asm(&mut self, var_name: &str, dest: &IRLitType) -> String {
        let dest_reg: (usize, AllocedReg) = self.resolve_register(dest);
        let dest_reg_name: &str = &dest_reg.1.name_64();

        let mut output_str: String = String::new();
        output_str.push_str(&format!("ADRP {dest_reg_name}, {var_name}@PAGE\n"));
        output_str.push_str(&format!("ADD {dest_reg_name}, {dest_reg_name}, {var_name}@PAGEOFF"));

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
        output_str.push_str(&format!("{compare_operator} _L{label_id}"));
        output_str
    }
}

impl<'asmgen> Aarch64IRToASM<'asmgen> {
    /// Extract the IRLitType as an operand(String)
    fn extract_operand(&mut self, irlit: &IRLitType) -> String {
        match irlit {
            IRLitType::Const(irlit_val) => {
                match irlit_val {
                    IRLitVal::Int32(value) => value.to_string(),
                    IRLitVal::U8(value) => value.to_string(),
                    IRLitVal::Str(value, ..) => value.clone(),
                    _ => todo!()
                }
            },
            
            IRLitType::Reg(idx) => format!("x{}", idx),
            
            IRLitType::Temp(temp_value) => {
                let src_reg: AllocedReg = self.temp_reg_map.reg_map.get(temp_value).unwrap().clone();
                self.try_dropping_temp(*temp_value);
                src_reg.name()
            },

            _ => unimplemented!()
        }
    }

    /// Get the compile time register mapping of a IR literal type. 
    /// Returns temporary ID with it's mapped AllocedReg.
    fn resolve_register(&mut self, irlit: &IRLitType) -> (TempId, AllocedReg) {
        match irlit {
            IRLitType::Temp(temp_value) => (*temp_value, self.get_or_allocate_temp_register(*temp_value)),

            IRLitType::AllocReg { reg, temp } => (*temp, self.get_or_allocate_specific_register(*reg, *temp)),
            
            IRLitType::Reg(idx) => (0, AllocedReg { size: 64, idx: *idx, status: RegStatus::Alloced }),
            
            _ => todo!(),
        }
    }
    
    fn get_or_allocate_temp_register(&mut self, temp_value: usize) -> AllocedReg {
        self.temp_reg_map
            .reg_map
            .get(&temp_value)
            .cloned()
            .unwrap_or_else(|| {
                let alloced_reg: AllocedReg = self.allocate_register();
                self.temp_reg_map.reg_map.insert(temp_value, alloced_reg.clone());
                self.try_dropping_temp(temp_value);
                alloced_reg
            })
    }
    
    fn get_or_allocate_specific_register(&mut self, reg: RegIdx, temp: usize) -> AllocedReg {
        self.temp_reg_map
            .reg_map
            .get(&temp)
            .cloned()
            .unwrap_or_else(|| {
                let alloced_reg: AllocedReg = self.allocate_specific_register(reg);
                self.temp_reg_map.reg_map.insert(temp, alloced_reg.clone());
                self.try_dropping_temp(temp);
                alloced_reg
            })
    }
    
    fn allocate_register(&mut self) -> AllocedReg {
        let mut reg_mgr = self.reg_manager.borrow_mut();
        reg_mgr.allocate_register(64)
    }
    
    fn allocate_specific_register(&mut self, reg: RegIdx) -> AllocedReg {
        let mut reg_mgr = self.reg_manager.borrow_mut();
        reg_mgr.allocate_register_with_idx(64, reg, AllocStrategy::Spill)
    }
}