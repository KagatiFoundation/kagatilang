// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::instruction::IRInstruction;

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq)]
pub struct IRValueId(pub usize);

#[derive(Debug)]
pub enum IRValueKind {
    Argument(usize),
    Constant(i64),
    Instruction(IRInstruction)
}

#[derive(Debug)]
pub enum IRType {
    I64,
    Void,
}

#[derive(Debug)]
pub struct IRValue {
    pub id: IRValueId,
    pub kind: IRValueKind,
    pub ty: IRType
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_simple_ir_value_creation() {
        
    }
}