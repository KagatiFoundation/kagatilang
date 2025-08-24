use kagc_symbol::StorageClass;

use crate::{ir_types::*, LabelId};

#[derive(Debug, Clone)]
pub enum IRAddr {
    /// Absolute stack offset
    StackOff(usize),

    /// Stack offset relative to some register
    BaseOff(IRLitType, i32)
}

#[derive(Debug, Clone)]
pub enum IRInstr {
    Mov {
        dest: IRLitType, 
        src: IRLitType
    },
    
    Add {
        dest: IRLitType, 
        op1: IRLitType, 
        op2: IRLitType
    },

    Sub {
        dest: IRLitType,
        op1: IRLitType,
        op2: IRLitType,
    },

    Mul {
        dest: IRLitType,
        op1: IRLitType,
        op2: IRLitType,
    },

    Div {
        dest: IRLitType,
        op1: IRLitType,
        op2: IRLitType,
    },

    Call {
        /// Function name to call.
        fn_name: String,

        /// Function's parameters explained:
        /// 
        /// `usize`: The parameter's position in the argument list.
        /// 
        /// `IRLitType`: The parameter.
        params: Vec<(usize, IRLitType)>,

        /// Return type of the function call.
        return_type: Option<IRLitType>
    },

    /**
     * Stack operations
     */
    Store {
        /// Value to be stored. `src` is always a register type.
        src: IRLitType,

        addr: IRAddr 
    },
    
    Load {
        /// Destination to load to. `dest` is always a register type
        dest: IRLitType,

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
        dest: IRLitType
    },

    /// Conditional jump
    CondJump {
        /// Operand 1
        op1: IRLitType,

        /// Operand 2
        op2: IRLitType,

        /// Label ID to jump to.
        label_id: LabelId,

        operation: IRCondOp
    },

    Jump {
        /// Label ID to jump to.
        label_id: usize
    },

    /*
        The following instructions are used by the compiler to
        utilize garbage collection feature.
    */

    /// Grabage Collector's allocation instruction
    MemAlloc {
        /// Destination to load the allocated raw address into
        // dest: IRLitType,

        /// This field is always IRLitType::Reg
        // src: IRLitType,

        /// How much memory to allocate
        size: usize
    },

    /// Grabage Collector's copy instruction
    MemCpy {
        /// Destination to load the allocated raw address into
        dest: IRLitType,

        /// This field is always IRLitType::Reg
        src: IRLitType,

        /// How much to copy
        size: usize
    }
}

impl IRInstr {
    pub fn dest(&self) -> Option<IRLitType> {
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
                    IRAddr::StackOff(off) => Some(IRLitType::StackOff(*off)),
                    IRAddr::BaseOff(base, _) => Some(base.clone()),
                }
            },
            
            Self::MemCpy { dest, .. } => Some(dest.clone()),

            _ => None
        }
    }

    pub fn mov_into_temp(temp: usize, value: IRLitType, reg_size: usize) -> Self {
        Self::Mov {
            dest: IRLitType::ExtendedTemp{ id: temp, size: reg_size }, 
            src: value
        }
    }
}

#[derive(Debug, Clone)]
pub struct IRFunc {
    pub name: String,
    pub params: Vec<IRLitType>,
    pub body: Vec<IR>,
    pub class: StorageClass,
    pub is_leaf: bool,
    pub scope_id: usize,
    pub id: usize
}

#[derive(Debug, Clone)]
pub struct IRVarDecl {
    /// Name of the variable being declared.
    pub sym_name: String,

    /// Storage class of the variable.
    pub class: StorageClass,

    /// Value of the variable.
    pub value: IRLitType,

    /// Stack offset of the variable.
    pub offset: Option<usize>
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

    VarDecl(IRVarDecl),
    
    Return(IRReturn),
    
    Loop(IRLoop),
    
    Label(IRLabel),
    
    Instr(IRInstr)
}