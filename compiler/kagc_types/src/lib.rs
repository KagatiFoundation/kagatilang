// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

pub mod record;
pub mod builtins;
pub mod type_;
pub mod ctx;

pub use type_::*;

use core::panic;
use std::collections::HashMap;
use std::fmt::Display;

use lazy_static::lazy_static;

/// Represents literal value types used in the language.
#[derive(Debug, Clone)]
pub enum LitValue<'tcx> {
    /// 64-bit signed integer.
    I64(i64),

    /// 32-bit signed integer.
    I32(i32),

    /// 16-bit signed integer.
    I16(i16),

    /// 8-bit unsigned integer.
    /// Often referred to as `char` in C-like languages.
    U8(u8),

    /// 64-bit floating-point number (double precision).
    F64(f64),

    /// 32-bit floating-point number (single precision).
    F32(f32),

    /// Represents a `void` type, typically used for functions with no return value.
    Void,

    RawStr(&'tcx str),

    /// Represents a string literal paired with a unique label identifier.
    PoolStr(usize),

    /// Value which allocates space in heap memory.
    PoolValue(usize), // takes pool index as the sole parameter

    /// Represents an array literal.
    ///
    /// The first `usize` denotes the array length,
    /// and the second `usize` represents the size of each element.
    Array(LitTypeArray),

    /// Represents a null value (e.g., for optional pointers or uninitialized references).
    Null,

    Record,

    /// Placeholder value, typically used during intermediate stages of compilation.
    None,
}

impl<'tcx> From<LitValue<'tcx>> for String {
    fn from(value: LitValue) -> Self {
        match value {
            LitValue::I32(val) => val.to_string(),
            _ => todo!()
        }
    }
}

impl<'tcx> LitValue<'tcx> {
    pub fn kind(&self) -> TyKind<'tcx> {
        match self {
            LitValue::I64(_) |
            LitValue::I32(_) |
            LitValue::U8(_)         => TyKind::I64,
            LitValue::Void          => TyKind::Void,
            LitValue::RawStr(_)     => TyKind::Str,
            LitValue::PoolStr(_)    => TyKind::Str,
            LitValue::Null          => TyKind::Null,
            LitValue::Record        => TyKind::Record { name: "undefined" },
            LitValue::None          => TyKind::None,
            _                       => todo!()
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LitTypeArray {
    items_count: usize,
    item_size: usize,
    items_type: Box<LitTypeVariant2>
}

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub enum LitTypeVariant2 {
    #[default] I32,
    I64,
    I16,
    U8,
    F64,
    F32,
    Void,
    Str,
    PoolStr,

    /// Value which allocated space in heap memory.
    PoolValue,
    RawStr,
    Array,
    Null,
    None, // placeholder
    Record {
        name: String
    }
}

impl LitTypeVariant2 {
    /// Get the register size for this type of value
    pub fn to_reg_size(&self) -> usize {
        match self {
            // 8 bytes; every type is 8-bytes in size for now
            Self::I32
            | Self::U8
            | Self::I64
            | Self::Record{..}
            | Self::RawStr
            | Self::PoolStr
            | Self::Str
            | Self::Void
            | Self::Array => 8,

            // 0 bytes
            _ => 0
        }
    }

    pub fn is_gc_alloced(&self) -> bool {
        matches!(
            self, 
            Self::Record { .. }
            | Self::RawStr
            | Self::PoolStr
            | Self::Str
        )
    }
}

impl Display for LitTypeVariant2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I32 => write!(f, "integer"),
            Self::I64 => write!(f, "long"),
            Self::U8 => write!(f, "byte"),
            Self::Str
            | Self::PoolStr
            | Self::RawStr => write!(f, "string"),
            Self::Array => write!(f, "array"),
            Self::None => write!(f, "none"),
            Self::Void => write!(f, "void"),
            Self::Record{name} => write!(f, "{name}(record)"),
            _ => write!(f, ""),
        }
    }
}

macro_rules! check_lit_type_var_fn_impl {
    ($fn_name:ident, $variant:ident) => {
        pub fn $fn_name(&self) -> bool {
            matches!(self, Self::$variant)
        }
    };
}


impl LitTypeVariant2 {
    pub fn size(&self) -> usize {
        match self {
            // 8 bytes
            Self::I64 
            | Self::F64
            | Self::Str
            | Self::PoolStr
            | Self::U8
            | Self::RawStr => 8,

            // 4 bytes
            Self::F32 | 
            Self::I32 => 4,

            // 2 bytes
            Self::I16 => 2,

            // hmmm... suspicious
            _ => 0,
        }
    }

    pub fn is_int(&self) -> bool {
        matches!(self, LitTypeVariant2::I32 | LitTypeVariant2::I16 | LitTypeVariant2::I64 | LitTypeVariant2::U8)
    }

    check_lit_type_var_fn_impl!(is_void, Void);
    check_lit_type_var_fn_impl!(is_none, None);
    check_lit_type_var_fn_impl!(is_str, Str);
    check_lit_type_var_fn_impl!(is_int32, I32);
    check_lit_type_var_fn_impl!(is_int8, U8);
}

impl<'tcx> PartialEq for LitValue<'tcx> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::I32(l0), Self::I32(r0)) => *l0 == *r0,
            (Self::I64(l0), Self::I64(r0)) => *l0 == *r0,
            (Self::I16(l0), Self::I16(r0)) => *l0 == *r0,
            (Self::F64(l0), Self::F64(r0)) => *l0 == *r0,
            (Self::F32(l0), Self::F32(r0)) => *l0 == *r0,
            (Self::U8(l0), Self::U8(r0)) => *l0 == *r0,
            _ => false,
        }
    }
}

macro_rules! impl_ltype_unwrap {
    ($fn_name:ident, $variant:ident, $ty:ty) => {
        pub fn $fn_name(&self) -> Option<&$ty> {
            if let LitValue::$variant(val, ..) = self {
                Some(val)
            } else {
                None
            }
        }
    };
}

macro_rules! impl_ltype_tychk {
    ($fn_name:ident, $variant:ident) => {
        pub fn $fn_name(&self) -> bool {
            matches!(self, Self::$variant(..))
        }
    };
}

impl<'tcx> LitValue<'tcx> {
    impl_ltype_tychk!(is_i32, I32);
    impl_ltype_tychk!(is_i64, I64);
    impl_ltype_tychk!(is_char, U8);

    impl_ltype_unwrap!(unwrap_i64, I64, i64);
    impl_ltype_unwrap!(unwrap_i32, I32, i32);
    impl_ltype_unwrap!(unwrap_i16, I16, i16);
    impl_ltype_unwrap!(unwrap_u8, U8, u8);
    impl_ltype_unwrap!(unwrap_f64, F64, f64);
    impl_ltype_unwrap!(unwrap_f32, F32, f32);

    pub fn variant(&self) -> LitTypeVariant2 {
        match self {
            Self::I32(_) => LitTypeVariant2::I32,
            Self::I64(_) => LitTypeVariant2::I64,
            Self::I16(_) => LitTypeVariant2::I16,
            Self::U8(_) => LitTypeVariant2::U8,
            Self::F64(_) => LitTypeVariant2::F64,
            Self::F32(_) => LitTypeVariant2::F32,
            Self::PoolStr(_) => LitTypeVariant2::PoolStr,
            Self::Void => LitTypeVariant2::Void,
            Self::Array(_) => LitTypeVariant2::Array,
            _ => panic!("not a valid type to calculate variant of!"),
        }
    }

    pub fn size(&self) -> usize {
        match self {
            LitValue::I32(_) | LitValue::F32(_) => 32,
            LitValue::I16(_) => 16,
            LitValue::U8(_) => 8,
            _ => 0,
        }
    }

    pub fn coerce_to(&self, target_type: LitValue) -> Result<LitValue, String> {
        match target_type {
            LitValue::I16(_) => self.coerce_to_i16(),
            LitValue::I32(_) => self.coerce_to_i32(),
            LitValue::I64(_) => self.coerce_to_i64(),
            _ => panic!("Error")
        }
    }

    fn coerce_to_i16(&self) -> Result<LitValue, String> {
        match self {
            LitValue::U8(val) => Ok(LitValue::I16(*val as i16)),
            _ => panic!("Error")
        }
    }

    fn coerce_to_i32(&self) -> Result<LitValue, String> {
        match self {
            LitValue::U8(val) => Ok(LitValue::I32(*val as i32)),
            LitValue::I16(val) => Ok(LitValue::I32(*val as i32)),
            _ => panic!("Error")
        }
    }

    fn coerce_to_i64(&self) -> Result<LitValue, String> {
        match self {
            LitValue::U8(val) => Ok(LitValue::I64(*val as i64)),
            LitValue::I16(val) => Ok(LitValue::I64(*val as i64)),
            LitValue::I32(val) => Ok(LitValue::I64(*val as i64)),
            _ => panic!("Error")
        }
    }
}

impl<'tcx> Display for LitValue<'tcx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = match self {
            LitValue::I32(value) => format!("{}", *value),
            LitValue::U8(value) => format!("{}", *value),
            LitValue::PoolStr(pos) => format!("__G_{pos}"),
            _ => panic!()
        };
        _ = writeln!(f, "{}", result);
        Ok(())
    }
}

/// Represents possible errors that may occur during type conversion.
pub enum TypeConversionError {
    /// Indicates an error where a conversion from a larger size to a 
    /// smaller size is attempted, which may not always be possible 
    /// depending on the context.
    BigSizeToSmallSize {
        /// The source literal type variant from which the conversion was attempted.
        from: LitTypeVariant2, 
        /// The target literal type variant to which the conversion was attempted.
        to: LitTypeVariant2
    }
}

lazy_static! {
    pub static ref TYPE_PRECEDENCE: std::collections::HashMap<u8, u8> = {
        let typ: std::collections::HashMap<u8, u8> = HashMap::new();
        // typ.insert(LitTypeVariant::I64 as u8, 3);
        // typ.insert(LitTypeVariant::I32 as u8, 2);
        // typ.insert(LitTypeVariant::I16 as u8, 1);
        // typ.insert(LitTypeVariant::U8 as u8, 0);
        typ
    };
}

pub fn is_type_coalescing_possible(src: LitTypeVariant2, dest: LitTypeVariant2) -> bool {
    match src {
        LitTypeVariant2::U8 => matches!(dest, LitTypeVariant2::U8 | LitTypeVariant2::I16 | LitTypeVariant2::I32 | LitTypeVariant2::I64),
        LitTypeVariant2::I16 => matches!(dest, LitTypeVariant2::I16 | LitTypeVariant2::I32 | LitTypeVariant2::I64),
        LitTypeVariant2::I32 => matches!(dest, LitTypeVariant2::I32 | LitTypeVariant2::I64),
        _ => false
    }
}

// tests
#[cfg(test)]
mod tests {
    use crate::is_type_coalescing_possible;

    use super::LitTypeVariant2 as LitTypeVariant;

    #[test]
    // Trying to conver 1223 into unsigned char. This should fail.
    fn test_type_coalescing() {
        assert!(is_type_coalescing_possible(LitTypeVariant::U8, LitTypeVariant::I32));
        assert!(is_type_coalescing_possible(LitTypeVariant::U8, LitTypeVariant::I16));
        assert!(is_type_coalescing_possible(LitTypeVariant::U8, LitTypeVariant::I64));
        assert!(!is_type_coalescing_possible(LitTypeVariant::I32, LitTypeVariant::U8));
        assert!(!is_type_coalescing_possible(LitTypeVariant::I32, LitTypeVariant::I16));
        assert!(is_type_coalescing_possible(LitTypeVariant::I32, LitTypeVariant::I32));
        assert!(is_type_coalescing_possible(LitTypeVariant::I32, LitTypeVariant::I64));
    }
}