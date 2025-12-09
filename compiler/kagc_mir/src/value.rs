// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Default, Hash)]
pub struct IRValueId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Default)]
pub struct ParamPosition(pub usize);

#[derive(Debug, Clone, Copy)]
pub enum IRValue {
    Constant(i64),
    Var(IRValueId),

    // memory address
    SymbolicOffset(usize) // argument is label id
}

impl IRValue {
    pub fn as_value_id(&self) -> Option<IRValueId> {
        match self {
            IRValue::Var(id) => Some(*id),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::value::{IRValue, IRValueId};

    #[test]
    fn test_as_value_id_correctness() {
        let value = IRValue::Constant(32);
        assert_eq!(value.as_value_id(), None);

        let value = IRValue::Var(IRValueId(12));
        assert_eq!(value.as_value_id(), Some(IRValueId(12)));
    }
}