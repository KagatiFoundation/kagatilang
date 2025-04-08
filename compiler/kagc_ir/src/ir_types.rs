use kagc_ast::ASTOperation;
use kagc_symbol::Symbol;
use kagc_target::reg::RegIdx;

/// Temporary identifier.
pub type TempId = usize;

#[derive(Debug, Clone)]
pub enum IRLitVal {
    Str(String),
    Int64(i64),
    Int32(i32),
    U8(u8)
}

impl IRLitVal {
    pub fn into_str(&self) -> String {
        match self {
            IRLitVal::Str(value) => value.clone(),
            IRLitVal::Int64(value) => value.to_string(),
            IRLitVal::Int32(value) => value.to_string(),
            IRLitVal::U8(value) => value.to_string()
        }
    }
}

#[derive(Debug, Clone)]
pub enum IRLitType {
    Var(Symbol),

    Const(IRLitVal),
    
    Reg(RegIdx),
    
    Temp(TempId),

    /// Experimental
    /// Allocate a register and associate it with a temporary
    AllocReg {
        /// Index of the register to be allocated.
        reg: RegIdx,

        /// Temporary to associate the allocate register with.
        temp: TempId
    },

    /// Stack offset
    StackOff(usize)
}

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

pub struct IRSymbol {
    pub symbol: Symbol,
    pub offset: usize
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

impl IRLitType {
    pub fn into_str(&self) -> String {
        match self {
            Self::Var(var) => var.name.clone(),
            
            Self::Const(irlit_val) => irlit_val.into_str(),
            
            Self::Reg(reg) => reg.to_string(),
            
            Self::Temp(tmp) => tmp.to_string(),

            Self::AllocReg { .. } => "".to_string(),

            Self::StackOff(off) => off.to_string()
        }
    }

    check_instr_type!(is_temp, Temp);
    check_instr_type!(is_var, Var);
    check_instr_type!(is_const, Const);
    check_instr_type!(is_reg, Reg);
    check_instr_type!(is_stack_off, StackOff);

    pub fn is_alloc_reg(&self) -> bool {
        matches!(self, Self::AllocReg { .. })
    }

    impl_as_irlit_type!(as_temp, Temp, usize);
    impl_as_irlit_type!(as_reg, Reg, RegIdx);
    impl_as_irlit_type!(as_var, Var, Symbol);
    impl_as_irlit_type!(as_const, Const, IRLitVal);
    impl_as_irlit_type!(as_stack_off, StackOff, usize);

    pub fn as_alloc_reg(&self) -> Option<(RegIdx, usize)> {
        match self {
            Self::AllocReg { reg, temp } => Some((*reg, *temp)),
            _ => None
        }
    }
}