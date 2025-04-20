/*
MIT License

Copyright (c) 2023 Kagati Foundation

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

use core::panic;
use std::{collections::HashMap, fmt::Display};

use lazy_static::lazy_static;

/// Trait for types that can be compared based on their variant and equality.
pub trait BTypeComparable {
    /// Compares `self` with another instance of the same type for equality.
    ///
    /// # Parameters
    /// - `other`: Another instance of the same type to compare with.
    ///
    /// # Returns
    /// - `true` if the types are equal, `false` otherwise.
    fn cmp(&self, other: &Self) -> bool;

    /// Returns the variant of the type.
    ///
    /// # Returns
    /// - The `LitTypeVariant` representing the type's variant.
    fn variant(&self) -> LitTypeVariant;
}

/// Trait for types that have a defined size.
pub trait TypeSized {
    /// Returns the size of the type in bytes.
    ///
    /// # Returns
    /// - The size of the type as a `usize` value.
    fn type_size(&self) -> usize;
}

/// Represents literal value types used in the language.
#[derive(Debug, Clone)]
pub enum LitType {
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

    /// Represents a string literal paired with a unique label identifier.
    Str {
        /// The actual string value.
        value: String,

        /// The unique label ID associated with the string.
        label_id: usize,
    },

    /// Represents an array literal.
    ///
    /// The first `usize` denotes the array length,
    /// and the second `usize` represents the size of each element.
    Array(LitTypeArray),

    /// Represents a null value (e.g., for optional pointers or uninitialized references).
    Null,

    /// Placeholder value, typically used during intermediate stages of compilation.
    None,
}

impl From<LitType> for String {
    fn from(value: LitType) -> Self {
        match value {
            LitType::I64(_) => todo!(),
            LitType::I32(val) => val.to_string(),
            LitType::I16(_) => todo!(),
            LitType::U8(_) => todo!(),
            LitType::F64(_) => todo!(),
            LitType::F32(_) => todo!(),
            LitType::Void => todo!(),
            LitType::Str { .. } => todo!(),
            LitType::Array(_) => todo!(),
            LitType::Null => todo!(),
            LitType::None => todo!(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LitTypeArray {
    items_count: usize,
    item_size: usize,
    items_type: Box<LitTypeVariant>
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LitTypeVariant {
    I32,
    I64,
    I16,
    U8,
    F64,
    F32,
    Void,
    Str,
    Array,
    Null,
    None, // placeholder
}

impl Display for LitTypeVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I32 => write!(f, "integer"),
            Self::I64 => write!(f, "long"),
            Self::U8 => write!(f, "byte"),
            Self::Str => write!(f, "str"),
            Self::Array => write!(f, "array"),
            Self::None => write!(f, "none"),
            Self::Void => write!(f, "void"),
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


impl LitTypeVariant {
    pub fn size(&self) -> usize {
        match self {
            Self::I64 
            | Self::F64
            | Self::Str => 8,

            Self::F32 | 
            Self::I32 => 4,

            Self::U8 => 1,
            Self::I16 => 2,
            _ => 0,
        }
    }

    pub fn is_int(&self) -> bool {
        matches!(self, LitTypeVariant::I32 | LitTypeVariant::I16 | LitTypeVariant::I64 | LitTypeVariant::U8)
    }

    check_lit_type_var_fn_impl!(is_void, Void);
    check_lit_type_var_fn_impl!(is_none, None);
    check_lit_type_var_fn_impl!(is_str, Str);
    check_lit_type_var_fn_impl!(is_int32, I32);
    check_lit_type_var_fn_impl!(is_int8, U8);
}

impl PartialEq for LitType {
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
            if let LitType::$variant(val, ..) = self {
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

impl LitType {
    impl_ltype_tychk!(is_i32, I32);
    impl_ltype_tychk!(is_i64, I64);
    impl_ltype_tychk!(is_char, U8);

    impl_ltype_unwrap!(unwrap_i64, I64, i64);
    impl_ltype_unwrap!(unwrap_i32, I32, i32);
    impl_ltype_unwrap!(unwrap_i16, I16, i16);
    impl_ltype_unwrap!(unwrap_u8, U8, u8);
    impl_ltype_unwrap!(unwrap_f64, F64, f64);
    impl_ltype_unwrap!(unwrap_f32, F32, f32);

    pub fn unwrap_str(&self) -> Option<&String> {
        if let LitType::Str { value, .. } = self {
            Some(value)
        }
        else {
            None
        }
    }

    pub fn variant(&self) -> LitTypeVariant {
        match self {
            Self::I32(_) => LitTypeVariant::I32,
            Self::I64(_) => LitTypeVariant::I64,
            Self::I16(_) => LitTypeVariant::I16,
            Self::U8(_) => LitTypeVariant::U8,
            Self::F64(_) => LitTypeVariant::F64,
            Self::F32(_) => LitTypeVariant::F32,
            Self::Str { .. } => LitTypeVariant::Str,
            Self::Void => LitTypeVariant::Void,
            Self::Array(_) => LitTypeVariant::Array,
            _ => panic!("not a valid type to calculate variant of!"),
        }
    }

    pub fn size(&self) -> usize {
        match self {
            LitType::I32(_) | LitType::F32(_) => 32,
            LitType::I16(_) => 16,
            LitType::U8(_) => 8,
            _ => 0,
        }
    }

    pub fn coerce_to(&self, target_type: LitType) -> Result<LitType, String> {
        match target_type {
            LitType::I16(_) => self.coerce_to_i16(),
            LitType::I32(_) => self.coerce_to_i32(),
            LitType::I64(_) => self.coerce_to_i64(),
            _ => panic!("Error")
        }
    }

    fn coerce_to_i16(&self) -> Result<LitType, String> {
        match self {
            LitType::U8(val) => Ok(LitType::I16(*val as i16)),
            _ => panic!("Error")
        }
    }

    fn coerce_to_i32(&self) -> Result<LitType, String> {
        match self {
            LitType::U8(val) => Ok(LitType::I32(*val as i32)),
            LitType::I16(val) => Ok(LitType::I32(*val as i32)),
            _ => panic!("Error")
        }
    }

    fn coerce_to_i64(&self) -> Result<LitType, String> {
        match self {
            LitType::U8(val) => Ok(LitType::I64(*val as i64)),
            LitType::I16(val) => Ok(LitType::I64(*val as i64)),
            LitType::I32(val) => Ok(LitType::I64(*val as i64)),
            _ => panic!("Error")
        }
    }
}

impl Display for LitType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = match self {
            LitType::I32(value) => format!("{}", *value),
            LitType::U8(value) => format!("{}", *value),
            LitType::Str { value, .. } => value.clone(),
            _ => panic!()
        };
        _ = writeln!(f, "{}", result);
        Ok(())
    }
}

impl BTypeComparable for LitType {
    fn cmp(&self, other: &LitType) -> bool {
        self.variant() == other.variant()
    }
    
    fn variant(&self) -> LitTypeVariant {
        self.variant()
    }
}

impl TypeSized for LitType {
    fn type_size(&self) -> usize {
        self.size()
    }
}

/// Represents possible errors that may occur during type conversion.
pub enum TypeConversionError {
    /// Indicates an error where a conversion from a larger size to a 
    /// smaller size is attempted, which may not always be possible 
    /// depending on the context.
    BigSizeToSmallSize {
        /// The source literal type variant from which the conversion was attempted.
        from: LitTypeVariant, 
        /// The target literal type variant to which the conversion was attempted.
        to: LitTypeVariant
    }
}

lazy_static! {
    pub static ref TYPE_PRECEDENCE: std::collections::HashMap<u8, u8> = {
        let mut typ: std::collections::HashMap<u8, u8> = HashMap::new();
        typ.insert(LitTypeVariant::I64 as u8, 3);
        typ.insert(LitTypeVariant::I32 as u8, 2);
        typ.insert(LitTypeVariant::I16 as u8, 1);
        typ.insert(LitTypeVariant::U8 as u8, 0);
        typ
    };
}

pub fn is_type_coalescing_possible(src: LitTypeVariant, dest: LitTypeVariant) -> bool {
    match src {
        LitTypeVariant::U8 => matches!(dest, LitTypeVariant::U8 | LitTypeVariant::I16 | LitTypeVariant::I32 | LitTypeVariant::I64),
        LitTypeVariant::I16 => matches!(dest, LitTypeVariant::I16 | LitTypeVariant::I32 | LitTypeVariant::I64),
        LitTypeVariant::I32 => matches!(dest, LitTypeVariant::I32 | LitTypeVariant::I64),
        _ => false
    }
}

// tests
#[cfg(test)]
mod tests {
    use crate::is_type_coalescing_possible;

    use super::LitTypeVariant;

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