// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::{cell::RefCell, rc::Rc};

use crate::instruction::IRInstruction;

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq)]
pub struct IRValueId(pub usize);

#[derive(Debug)]
pub struct IRValue {
    pub id: IRValueId,
    pub instr: Rc<RefCell<IRInstruction>>,
    pub users: Vec<IRValueId>
}

impl IRValue {
    pub fn new(id: IRValueId, instr: IRInstruction) -> Self {
        IRValue {
            id,
            instr: Rc::new(RefCell::new(instr)),
            users: Vec::new(),
        }
    }

    /// Add a user of this SSA value
    pub fn add_user(&mut self, user: IRValueId) {
        self.users.push(user);
    }
}

#[cfg(test)]
mod tests {
    use crate::value::*;
    use crate::operand::*;

    #[test]
    fn test_simple_ir_value_creation() {
        let mut v1 = IRValue::new(
            IRValueId(0), 
            IRInstruction::Assign { src: IROperand::Imm(32) }
        );

        let mut v2 = IRValue::new(
            IRValueId(1), 
            IRInstruction::Assign { src: IROperand::Imm(8) }
        );

        let v3 = IRValue::new(
            IRValueId(2), 
            IRInstruction::Add { 
                lhs: IROperand::Value(IRValueId(0)), 
                rhs: IROperand::Value(IRValueId(1)) 
            }
        );

        v1.add_user(IRValueId(2));
        v2.add_user(IRValueId(2));

        assert_eq!(v3.users.len(), 0);
        assert_eq!(v1.users[0], IRValueId(2));
        assert_eq!(v2.users[0], IRValueId(2));
    }
}