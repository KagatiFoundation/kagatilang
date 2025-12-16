// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_types::*;

pub type SymbolId = usize;

pub trait STableLookupKey {
    fn key_as_str(&self) -> Option<&str> { None }
    fn key_as_int(&self) -> Option<SymbolId> { None }
}

impl STableLookupKey for &str {
    fn key_as_str(&self) -> Option<&str> {
        Some(self)
    }
}

impl STableLookupKey for SymbolId {
    fn key_as_int(&self) -> Option<SymbolId> {
        Some(*self)
    }
}

/// The types of symbol names inside the symbol table
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolType {
    Variable,
    Record {
        name: String
    },
    Function,
    Array,
    Constant, // for now, this is used to represent only the string literals
}

#[derive(Copy, Clone, PartialEq, Debug, Default)]
pub enum StorageClass {
    /// Globally visible symbol.
    #[default]
    GLOBAL, 

    /// Locally visible symbol.
    LOCAL, 

    /// Locally visible function parameter.
    PARAM, 

    /// Externally defined symbol.
    EXTERN
}

/// A trait to signify that a struct can be stored in a symbol table.
pub trait SymbolTrait { 
    fn uninit() -> Self;

    fn name(&self) -> String;

    fn is_unused(&self) -> bool;
}

/// Represents a symbol in Bichara, which can be a variable, 
/// function, or other identifier.
#[derive(Clone, PartialEq, Debug)]
pub struct Symbol {
    /// The name of the symbol.
    pub name: String,

    /// The literal type of the symbol, indicating the kind of 
    /// value it holds (e.g., integer, float).
    pub lit_type: LitTypeVariant,

    /// The type of the symbol, which can indicate whether it's a 
    /// variable, function, etc.
    pub sym_type: SymbolType,

    /// The number of elements in the symbol, which is particularly 
    /// relevant for arrays.
    pub size: usize,

    /// The storage class of the symbol, indicating its storage duration 
    /// and linkage (e.g., local, global).
    pub class: StorageClass,

    /// The offset from the stack base pointer for local symbols. This 
    /// is used to locate the symbol in the stack frame.
    pub local_offset: i32,

    /// Function ID if the `class` is `LOCAL`
    pub func_id: Option<usize>,

    /// The default value of the symbol, applicable only for global 
    /// symbols.
    pub default_value: Option<LitType>,

    __use_count: usize, // how many times has this symbol been used
}

impl SymbolTrait for Symbol {
    fn uninit() -> Self {
        Symbol {
            class: StorageClass::GLOBAL,
            default_value: None,
            lit_type: LitTypeVariant::None,
            local_offset: 0,
            name: "".to_string(),
            size: 0,
            sym_type: SymbolType::Variable,
            func_id: None,
            __use_count: 0
        }
    }

    fn name(&self) -> String {
        self.name.clone()
    }

    fn is_unused(&self) -> bool {
        self.__use_count == 0
    }
}

impl Symbol {
    pub fn new(name: String, lit: LitTypeVariant, sym_type: SymbolType, class: StorageClass) -> Self {
        Self {
            name,
            lit_type: lit,
            sym_type,
            size: 1,
            class,
            local_offset: 0,
            default_value: None,
            __use_count: 0,
            func_id: None
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn create(
        name: String,
        lit_type: LitTypeVariant,
        sym_type: SymbolType,
        size: usize,
        class: StorageClass,
        local_offset: i32,
        default_value: Option<LitType>,
        func_id: usize,
    ) -> Self {
        Self {
            name,
            lit_type,
            sym_type,
            size,
            class,
            local_offset,
            default_value,
            __use_count: 0,  // Ignored or initialized to 0
            func_id: Some(func_id),
        }
    }

    pub fn incr_use(&mut self) {
        self.__use_count += 1;
    }
}