// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_target::reg::RegSize;

use crate::LabelId;

/// Temporary identifier.
pub type TempId = usize;

/// Argument position.
pub type ArgPos = usize;

#[derive(Debug, Clone)]
pub enum IRImmVal {
    Str(String, LabelId),
    Int64(i64),
    Int32(i32),
    U8(u8),
    Null
}

impl IRImmVal {
    pub fn into_str(&self) -> String {
        match self {
            IRImmVal::Str(value, ..) => value.clone(),
            IRImmVal::Int64(value) => value.to_string(),
            IRImmVal::Int32(value) => value.to_string(),
            IRImmVal::U8(value) => value.to_string(),
            IRImmVal::Null => "null".to_string()
        }
    }
}

#[derive(Debug, Clone)]
pub enum IROperand {
    Const(IRImmVal),

    Temp {
        id: TempId,
        size: RegSize
    },

    CallArg {
        temp: TempId,
        position: ArgPos,
        size: usize,
    },

    Param {
        position: ArgPos,
        size: usize
    },

    CallValue {
        position: usize, // TODO: Remove this field
        size: usize
    },

    Return {
        position: usize, // TODO: Remove this field
        temp: TempId,
        size: usize
    },

    /// Stack offset
    StackSlot(usize),
}

#[derive(Debug, Clone)]
pub enum IRAddr {
    /// Absolute stack offset
    StackOff(usize),

    /// Stack offset relative to some register
    BaseOff(IROperand, i32)
}

macro_rules! check_instr_type {
    ($fn_name:ident, $variant:ident) => {
        pub fn $fn_name(&self) -> bool {
            matches!(self, Self::$variant(..))
        }
    };
}

macro_rules! impl_as_irlit_type {
    ($fn_name:ident, $self_type:ident, $value_type:ident) => {
        pub fn $fn_name(&self) -> Option<$value_type> {
            match self {
                Self::$self_type(variant_value) => Some(variant_value.clone()),
                _ => None
            }
        }
    };
}

impl IROperand {
    pub fn into_str(&self) -> String {
        match self {
            Self::Const(irlit_val) => irlit_val.into_str(),
            Self::StackSlot(off) => off.to_string(),
            Self::Temp { id, .. } => id.to_string(),
            Self::CallArg { temp, .. } => temp.to_string(),
            Self::Param { position, .. } => position.to_string(),
            Self::CallValue { position, .. } => position.to_string(),
            Self::Return { position, .. } => position.to_string(),
        }
    }

    check_instr_type!(is_const, Const);
    check_instr_type!(is_stack_off, StackSlot);

    impl_as_irlit_type!(as_const, Const, IRImmVal);
    impl_as_irlit_type!(as_stack_off, StackSlot, usize);

    pub fn as_ext_temp(&self) -> Option<usize> {
        match self {
            Self::Temp { id, .. } => Some(*id),
            _ => None
        }
    }
}