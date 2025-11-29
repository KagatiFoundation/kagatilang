// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

pub mod opcode;
pub mod object;
pub mod frame;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use kagc_ctx::CompilerCtx;
use kagc_lir::block::LirBasicBlock;
use kagc_lir::function::LirFunction;
use kagc_lir::instruction::LirInstruction;
use kagc_lir::operand::LirOperand;
use kagc_lir::vreg::VReg;

use crate::codegen_lsvm::frame::FrameMapping;
use crate::codegen_lsvm::opcode::builder::OpcodeBuilder;
use crate::codegen_lsvm::opcode::LsvmConst;
use crate::CodeGenerator;

pub struct LsvmCodeGenerator {
    compiler_cx: Rc<RefCell<CompilerCtx>>,
    opcode_builder: OpcodeBuilder,
    constants: Vec<LsvmConst>,

    // VReg to constant mapping in the const table
    vreg_const_mapping: HashMap<usize, usize>,
    next_index: usize,

    frame_mapping: FrameMapping
}

impl LsvmCodeGenerator {
    pub fn new(ctx: Rc<RefCell<CompilerCtx>>) -> Self {
        Self {
            compiler_cx: ctx,
            opcode_builder: OpcodeBuilder::default(),
            vreg_const_mapping: HashMap::default(),
            next_index: 0,
            constants: vec![],
            frame_mapping: FrameMapping::default()
        }
    }
}

impl CodeGenerator for LsvmCodeGenerator {
    fn gen_function(&mut self, func: &LirFunction) {
        self.opcode_builder.start_function();

        let mut block_ids: Vec<_> = func.blocks.keys().cloned().collect();
        block_ids.sort_by_key(|b| b.0);

        for bid in block_ids {
            let block = &func.blocks[&bid];
            self.gen_block(block);
        }

        self.end_function();
    }

    fn gen_block(&mut self, block: &LirBasicBlock) {
        for instr in block.instructions.iter() {
            self.gen_instruction(instr);
        }
    }

    fn gen_instruction(&mut self, instr: &LirInstruction) {
        match instr {
            LirInstruction::Mov { src, dest } => {
                match src {
                    LirOperand::Constant(imm) => {
                        let ci = self.push_const(LsvmConst::Int64(*imm));
                        self.vreg_const_mapping.insert(dest.0, ci);
                    },
                    LirOperand::VReg(vreg) => {
                        let value = self.get_vreg_index(vreg);
                        self.vreg_const_mapping.insert(dest.0, value);
                    }
                }
            },
            LirInstruction::Store { src, .. } => {
                let si = self.get_vreg_index(src);
                self.opcode_builder.push(si);
            },
            LirInstruction::Load { src, dest } => {

            },
            LirInstruction::Add { lhs, rhs, .. } => {
                match (lhs, rhs) {
                    (LirOperand::VReg(val1), LirOperand::VReg(val2)) => {
                        let c1 = self.get_vreg_index(val1);
                        let c2 = self.get_vreg_index(val2);
                        self.opcode_builder.add(c1, c2);
                    },
                    _ => unreachable!()
                }
            }
            _ => unimplemented!("{instr:#?}")
        }
    }
}

impl LsvmCodeGenerator {
    fn push_const(&mut self, value: LsvmConst) -> usize {
        let idx = self.next_index;
        self.constants.push(value);
        self.next_index += 1;
        idx
    }

    fn get_vreg_index(&mut self, vreg: &VReg) -> usize {
        if let Some(&index) = self.vreg_const_mapping.get(&vreg.0) {
            index
        } 
        else {
            let index = self.next_index;
            self.vreg_const_mapping.insert(vreg.0, index);
            self.next_index += 1;
            index
        }
    }

    fn end_function(&mut self) {
        self.opcode_builder.end_function();
    }

    pub fn generate_final(self) -> Vec<usize> {
        self.opcode_builder.build()
    }
}

pub mod executor {
    use crate::codegen_lsvm::opcode::LsvmOpcode;

    pub struct OpcodeExecutor;

    impl OpcodeExecutor {
        pub fn execute(&self, opcodes: &[LsvmOpcode]) {
            for opcode in opcodes {
                println!("{opcode:#?}");
            }
        }
    }
}