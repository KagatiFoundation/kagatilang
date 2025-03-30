use kagc_symbol::Symbol;
use kagc_target::reg::RegIdx;

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
    
    Temp(usize),

    /// Experimental
    /// Allocate a register and associate it with a temporary
    AllocReg {
        /// Index of the register to be allocated.
        reg: RegIdx,

        /// Temporary to associate the allocate register with.
        temp: usize
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

            Self::AllocReg { .. } => "".to_string()
        }
    }

    check_instr_type!(is_temp, Temp);
    check_instr_type!(is_var, Var);
    check_instr_type!(is_const, Const);
    check_instr_type!(is_reg, Reg);

    pub fn is_alloc_reg(&self) -> bool {
        matches!(self, Self::AllocReg { .. })
    }

    impl_as_irlit_type!(as_temp, Temp, usize);
    impl_as_irlit_type!(as_reg, Reg, RegIdx);
    impl_as_irlit_type!(as_var, Var, Symbol);
    impl_as_irlit_type!(as_const, Const, IRLitVal);

    pub fn as_alloc_reg(&self) -> Option<(RegIdx, usize)> {
        match self {
            Self::AllocReg { reg, temp } => Some((*reg, *temp)),
            _ => None
        }
    }
}