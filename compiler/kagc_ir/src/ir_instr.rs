// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_ast::ASTOperation;
use kagc_symbol::StorageClass;
use kagc_target::reg::RegIdx;
use kagc_target::reg::RegSize;
use kagc_types::builtins::obj::KObjType;

use crate::{ir_operands::*, LabelId};


#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum IRCondOp {
    IRLThan,
    IRGThan,
    IREqEq,
    IRNEq,
    IRGTEq,
    IRLTEq
}

impl From<ASTOperation> for IRCondOp {
    fn from(value: ASTOperation) -> Self {
        match value {
            ASTOperation::AST_GTHAN => Self::IRGThan,
            ASTOperation::AST_LTHAN => Self::IRLThan,
            ASTOperation::AST_EQEQ => Self::IREqEq,
            ASTOperation::AST_NEQ => Self::IRNEq,
            ASTOperation::AST_GTEQ => Self::IRGTEq,
            ASTOperation::AST_LTEQ => Self::IRLTEq,
            _ => panic!("Cannot convert!")
        }
    }
}

#[derive(Debug, Clone)]
pub enum IRInstr {
    Mov {
        dest: IROperand, 
        src: IROperand
    },
    
    Add {
        dest: IROperand, 
        op1: IROperand, 
        op2: IROperand
    },

    Sub {
        dest: IROperand,
        op1: IROperand,
        op2: IROperand,
    },

    Mul {
        dest: IROperand,
        op1: IROperand,
        op2: IROperand,
    },

    Div {
        dest: IROperand,
        op1: IROperand,
        op2: IROperand,
    },

    Call {
        /// Function name to call.
        fn_name: String,

        /// Function's parameters explained:
        /// 
        /// `usize`: The parameter's position in the argument list.
        /// 
        /// `IRLitType`: The parameter.
        params: Vec<(usize, IROperand)>,

        /// Return type of the function call.
        return_type: Option<IROperand>
    },

    /**
     * Stack operations
     */
    Store {
        /// Value to be stored. `src` is always a register type.
        src: IROperand,

        addr: IRAddr 
    },
    
    Load {
        /// Destination to load to. `dest` is always a register type
        dest: IROperand,

        addr: IRAddr
    },

    /**
     * Stack operations end here
    */

    /// Load global variable
    LoadGlobal {
        /// Variable's name
        pool_idx: usize,

        /// Destination to load to
        dest: IROperand
    },

    /// Conditional jump
    CondJump {
        /// Operand 1
        op1: IROperand,

        /// Operand 2
        op2: IROperand,

        /// Label ID to jump to.
        label_id: LabelId,

        operation: IRCondOp
    },

    Jump {
        /// Label ID to jump to.
        label_id: usize
    },

    /*
        The instructions after this point are used by the compiler to
        utilize garbage collection feature.
    */

    /// Grabage Collector's allocation instruction
    MemAlloc {
        /// How much memory to allocate
        size: usize,

        /// Destination where the address of newly allocated memory's
        /// address lives
        dest: IROperand,

        /// Object type
        ob_type: KObjType
    },

    /// Grabage Collector's allocation instruction
    MemRelease {
        /// Location where the pointer is stored
        addr: IRAddr
    },

    /// Grabage Collector's copy instruction
    MemCpy {
        dest: IROperand
    },

    /// Mark the register as "allocated"
    RegAlloc2 {
        idx: RegIdx,
        size: RegSize,
        dest: IROperand
    }
}

impl IRInstr {
    pub fn dest(&self) -> Option<IROperand> {
        match self {
            Self::Mov { dest, .. } => Some(dest.clone()),
            Self::Add { dest, .. } => Some(dest.clone()),
            Self::Sub { dest, .. } => Some(dest.clone()),
            Self::Mul { dest, .. } => Some(dest.clone()),
            Self::Div { dest, .. } => Some(dest.clone()),
            Self::Call { return_type, .. } => return_type.clone(),
            Self::Load { dest, .. } => Some(dest.clone()),
            Self::LoadGlobal { dest, .. } => Some(dest.clone()),

            Self::Store { addr, .. } => {
                match addr {
                    IRAddr::StackOff(off) => Some(IROperand::StackSlot(*off)),
                    IRAddr::BaseOff(base, _) => Some(base.clone()),
                }
            },

            Self::MemCpy { dest } => Some(dest.clone()),
            Self::MemAlloc { dest, .. } => Some(dest.clone()),
            _ => None
        }
    }

    pub fn mov_into_temp(temp: usize, value: IROperand, reg_size: usize) -> Self {
        Self::Mov {
            dest: IROperand::Temp{ id: temp, size: reg_size }, 
            src: value
        }
    }
}

#[derive(Debug, Clone)]
pub struct IRFunc {
    pub name: String,
    pub params: Vec<IROperand>,
    pub body: Vec<IR>,
    pub class: StorageClass,
    pub is_leaf: bool,
    pub scope_id: usize,
    pub id: usize
}

#[derive(Debug, Clone)]
pub struct IRReturn {
    pub early: bool,

    pub early_label_id: LabelId
}

#[derive(Debug, Default, Clone)]
pub struct IRLabel(pub LabelId);

#[derive(Debug, Default, Clone)]
pub struct IRLoop {
    pub start_label: IRLabel,

    pub end_label: IRLabel,

    pub body: Vec<IR>
}

#[derive(Debug, Clone)]
pub enum IR {
    Func(IRFunc),

    Return(IRReturn),
    
    Loop(IRLoop),
    
    Label(IRLabel),
    
    Instr(IRInstr)
}