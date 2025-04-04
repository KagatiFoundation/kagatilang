use kagc_symbol::StorageClass;

use crate::ir_types::*;

#[derive(Debug, Clone)]
pub enum IRInstr {
    Mov(IRLitType, IRLitType),
    
    Add(
        IRLitType, 
        IRLitType, 
        IRLitType
    ),

    Call {
        fn_name: String,

        params: Vec<(usize, IRLitType)>,

        return_type: Option<IRLitType>
    },

    Store {
        /// Value to be stored. `src` is always a register type.
        src: IRLitType,

        /// Location to store to. `IRLitType` must be of type `IRLitType::StackOff(usize)`.
        stack_off: IRLitType
    },
    
    Load {
        /// Destination to load to. `dest` is always a register type
        dest: IRLitType,

        /// Stack offset to load value from
        /// TODO: change the type of `IRLitType`
        stack_off: usize
    },

    Jump {
        label_id: usize
    },

    /// Function call start
    CallStart,

    /// Function call end
    CallEnd
}

impl IRInstr {
    pub fn dest(&self) -> Option<IRLitType> {
        match self {
            Self::Mov(dst, _) => Some(dst.clone()),

            Self::Add(dst, _, _) => Some(dst.clone()),

            Self::Call { return_type, .. } => return_type.clone(),

            Self::Load { dest, .. } => Some(dest.clone()),

            Self::Store { stack_off, .. } => Some(stack_off.clone()),

            _ => None
        }
    }

    pub fn mov_into_temp(temp: usize, value: IRLitType) -> Self {
        Self::Mov(
            IRLitType::Temp(temp), 
            value
        )
    }
}

#[derive(Debug, Clone)]
pub struct IRFunc {
    pub name: String,
    pub params: Vec<IRLitType>,
    pub body: Vec<IR>,
    pub class: StorageClass,
    pub is_leaf: bool
}

#[derive(Debug, Clone)]
pub struct IRVarDecl {
    pub sym_name: String,
    pub class: StorageClass,
    pub value: IRLitType,
    pub offset: Option<usize>
}

#[derive(Debug, Clone)]
pub struct IRReturn {
    pub early: bool,

    pub early_label_id: usize
}

#[derive(Debug, Clone)]
pub enum IR {
    Func(IRFunc),
    VarDecl(IRVarDecl),
    Return(IRReturn),
    Instr(IRInstr)
}