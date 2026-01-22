// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use core::panic;
use std::collections::HashMap;
use std::fmt::Display;

use kagc_types::TyKind;
use lazy_static::lazy_static;

/// Represents literal value types used in the language.
#[derive(Debug, Clone)]
pub enum Literal<'tcx> {
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

    /// Represents a null value (e.g., for optional pointers or uninitialized references).
    Null,

    Record,

    /// Placeholder value, typically used during intermediate stages of compilation.
    Placeholder,
}

impl<'tcx> From<Literal<'tcx>> for String {
    fn from(value: Literal) -> Self {
        match value {
            Literal::I32(val) => val.to_string(),
            _ => todo!()
        }
    }
}

impl<'tcx> Literal<'tcx> {
    pub fn kind(&self) -> TyKind<'tcx> {
        match self {
            Literal::I64(_) |
            Literal::I32(_) |
            Literal::U8(_)         => TyKind::I64,
            Literal::Void          => TyKind::Void,
            Literal::RawStr(_)     => TyKind::Str,
            Literal::PoolStr(_)    => TyKind::Str,
            Literal::Null          => TyKind::Null,
            Literal::Record        => TyKind::Record { name: "undefined" },
            Literal::Placeholder   => TyKind::None,
            _                       => todo!()
        }
    }
}

impl<'tcx> PartialEq for Literal<'tcx> {
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
            if let Literal::$variant(val, ..) = self {
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

impl<'tcx> Literal<'tcx> {
    impl_ltype_tychk!(is_i32, I32);
    impl_ltype_tychk!(is_i64, I64);
    impl_ltype_tychk!(is_char, U8);

    impl_ltype_unwrap!(unwrap_i64, I64, i64);
    impl_ltype_unwrap!(unwrap_i32, I32, i32);
    impl_ltype_unwrap!(unwrap_i16, I16, i16);
    impl_ltype_unwrap!(unwrap_u8, U8, u8);
    impl_ltype_unwrap!(unwrap_f64, F64, f64);
    impl_ltype_unwrap!(unwrap_f32, F32, f32);

    pub fn size(&self) -> usize {
        match self {
            Literal::I32(_) | Literal::F32(_) => 32,
            Literal::I16(_) => 16,
            Literal::U8(_) => 8,
            _ => 0,
        }
    }

    pub fn coerce_to(&self, target_type: Literal) -> Result<Literal, String> {
        match target_type {
            Literal::I16(_) => self.coerce_to_i16(),
            Literal::I32(_) => self.coerce_to_i32(),
            Literal::I64(_) => self.coerce_to_i64(),
            _ => panic!("Error")
        }
    }

    fn coerce_to_i16(&self) -> Result<Literal, String> {
        match self {
            Literal::U8(val) => Ok(Literal::I16(*val as i16)),
            _ => panic!("Error")
        }
    }

    fn coerce_to_i32(&self) -> Result<Literal, String> {
        match self {
            Literal::U8(val) => Ok(Literal::I32(*val as i32)),
            Literal::I16(val) => Ok(Literal::I32(*val as i32)),
            _ => panic!("Error")
        }
    }

    fn coerce_to_i64(&self) -> Result<Literal, String> {
        match self {
            Literal::U8(val) => Ok(Literal::I64(*val as i64)),
            Literal::I16(val) => Ok(Literal::I64(*val as i64)),
            Literal::I32(val) => Ok(Literal::I64(*val as i64)),
            _ => panic!("Error")
        }
    }
}

impl<'tcx> Display for Literal<'tcx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = match self {
            Literal::I32(value) => format!("{}", *value),
            Literal::U8(value) => format!("{}", *value),
            Literal::PoolStr(pos) => format!("__G_{pos}"),
            _ => panic!()
        };
        _ = writeln!(f, "{}", result);
        Ok(())
    }
}

lazy_static! {
    pub static ref TYPE_PRECEDENCE: std::collections::HashMap<u8, u8> = {
        HashMap::new()
    };
}