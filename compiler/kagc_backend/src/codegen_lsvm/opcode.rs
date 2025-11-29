// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub enum LsvmConst {
    Int64(i64),
    Float64(f64),
    Int32(i32),
    Float32(f32),
    UInt32(u32),
    UInt64(u64),
    Bool(bool)
}

impl Default for LsvmConst {
    fn default() -> Self {
        LsvmConst::Int32(0)
    }
}

impl Display for LsvmConst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int64(value) => write!(f, "Int64({value})"),
            Self::Int32(value) => write!(f, "Int32({value})"),
            Self::UInt64(value) => write!(f, "UInt64({value})"),
            Self::UInt32(value) => write!(f, "UInt32({value})"),
            Self::Float32(value) => write!(f, "Float32({value})"),
            Self::Float64(value) => write!(f, "Float64({value})"),
            Self::Bool(value) => write!(f, "Boolean({value})"),
        }
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub enum LsvmOpcode {
    StartFunctionExec,
    ReturnValue,
    Push,
    Add,
    #[default]
    NoOperation
}

impl From<LsvmOpcode> for u8 {
    fn from(value: LsvmOpcode) -> Self {
        match value {
            LsvmOpcode::StartFunctionExec => 0,
            LsvmOpcode::ReturnValue => 1,
            LsvmOpcode::Push => 2,
            LsvmOpcode::Add => 3,
            LsvmOpcode::NoOperation => 4,
        }
    }
}

impl Display for LsvmOpcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LsvmOpcode::StartFunctionExec => write!(f, "START_FUNC"),
            LsvmOpcode::ReturnValue => write!(f, "RETURN_VALUE"),
            LsvmOpcode::Push => write!(f, "PUSH"),
            LsvmOpcode::Add => write!(f, "ADD"),
            LsvmOpcode::NoOperation => write!(f, "NO_OP"),
        }
    }
}

#[derive(Debug, Default)]
pub struct InstructionPos(pub usize);

pub mod builder {
    use crate::codegen_lsvm::opcode::{InstructionPos, LsvmOpcode};

    #[derive(Debug, Default)]
    pub struct OpcodeBuilder {
        pub(crate) code: Vec<usize>,
        instr_pos: usize
    }

    impl OpcodeBuilder {
        pub fn start_function(&mut self) {
            self.code.push(LsvmOpcode::StartFunctionExec as usize);
        }

        pub fn end_function(&mut self) {
            self.code.push(LsvmOpcode::ReturnValue as usize);
        }

        pub fn emit(&mut self, opcode: LsvmOpcode, args: &[usize]) {
            self.code.push(opcode as usize);
            self.code.extend_from_slice(args);
        }

        pub fn add(&mut self, lhs: usize, rhs: usize) {
            self.emit(LsvmOpcode::Add, &[lhs, rhs]);
        }

        pub fn push(&mut self, value: usize) {
            self.emit(LsvmOpcode::Push, &[value]);
        }

        fn next_instr_pos(&mut self) -> InstructionPos {
            let nip = self.instr_pos;
            self.instr_pos += 1;
            InstructionPos(nip)
        }

        pub fn build(self) -> Vec<usize> {
            self.code.clone()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen_lsvm::opcode::builder::OpcodeBuilder;

    #[test]
    fn test_simple_opcode_builder() {
        let mut opcode_builder = OpcodeBuilder::default();
        let _ = opcode_builder.start_function();
        let _ = opcode_builder.end_function();

        println!("{opcodes:#?}", opcodes = opcode_builder.code);
    }
}