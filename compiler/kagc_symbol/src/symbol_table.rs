// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::{cell::{Cell, RefCell}, collections::HashMap, slice::{Iter, IterMut}};
use crate::{STableLookupKey, Sym, SymId, sym::SymbolTrait};

/// Maximum number of symbols that can be stored in the symbol table.
pub const NSYMBOLS: usize = 1024;

/// A generic symbol table that stores declared symbols of type `T`.
/// 
/// Keeps track of a list of symbols and their declaration order.
#[derive(Clone, Debug)]
pub struct Symtable<T: SymbolTrait + Clone> {
    /// List of declared symbols.
    syms: Vec<T>,

    /// Points to the next free index for symbol insertion.
    counter: usize,
}

impl<T: SymbolTrait + Clone> Default for Symtable<T> {
    fn default() -> Self {
        Self { 
            syms: Default::default(), 
            counter: Default::default() 
        }
    }
}

impl<T: SymbolTrait + Clone> Symtable<T> {
    /// Looks up a symbol by name (immutable reference).
    ///
    /// Returns `Some(&T)` if found, or `None` otherwise.
    pub fn lookup<Key>(&self, key: &Key) -> Option<&T> 
    where Key: STableLookupKey {
        if let Some(name) = key.key_as_str() {
            return self.syms.iter().find(|&sym| sym.name() == name);
        }

        if let Some(index) = key.key_as_int() {
            return self.syms.get(index);
        }
        None
    }

    /// Looks up a symbol by name (mutable reference).
    ///
    /// Returns `Some(&mut T)` if found, or `None` otherwise.
     pub fn lookup_mut<Key>(&mut self, key: &Key) -> Option<&mut T> 
     where Key: STableLookupKey {
        if let Some(name) = key.key_as_str() {
            return self.syms.iter_mut().find(|sym| sym.name() == name);
        }

        if let Some(index) = key.key_as_int() {
            return self.syms.get_mut(index);
        }
        None
    }

    /// Declares a new symbol if it doesn't already exist.
    ///
    /// Returns the position it was inserted at, or `None` if duplicate.
    pub fn declare(&mut self, sym: T) -> Option<usize> {
        let act_pos: usize = self.next();
        if self.get_symbol_pos(&sym.name()).is_some() { 
            return None;
        }
        self.syms.push(sym);
        Some(act_pos - 1)
    }

    /// Declares a symbol at a specific position in the table.
    ///
    /// Panics if the position exceeds `NSYMBOLS`.
    pub fn declare_at(&mut self, pos: usize, sym: T) -> Option<usize> {
        assert!(
            pos < NSYMBOLS,
            "value '{}' out of bounds for range '{}'",
            pos,
            self.counter
        );
        self.syms[pos] = sym;
        Some(pos)
    }

    pub fn get_symbol_pos(&self, name: &str) -> Option<usize> {
        self.syms.iter().position(|sym| sym.name() == name)
    }

    fn next(&mut self) -> usize {
        assert!(self.counter < NSYMBOLS, "Symbol table is full!");
        self.counter += 1;
        self.counter
    }

    pub fn remove_symbol(&mut self, index: usize) -> T {
        assert!(self.counter < NSYMBOLS);
        self.syms.remove(index)
    }

    pub fn iter(&self) -> Iter<'_, T> {
        self.syms.iter()
    }

    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        self.syms.iter_mut()
    }

    pub fn count(&self) -> usize {
        self.syms.len()
    }
}

pub struct SymTable<'tcx> {
    arena: &'tcx typed_arena::Arena<Sym<'tcx>>,
    pub symbols: RefCell<HashMap<&'tcx str, &'tcx Sym<'tcx>>>,
    pub next_id: Cell<usize>,
}

impl<'tcx> SymTable<'tcx> {
    pub fn new(arena: &'tcx typed_arena::Arena<Sym<'tcx>>) -> Self {
        Self {
            arena,
            symbols: RefCell::new(HashMap::new()),
            next_id: Cell::new(0) // begin the count
        }
    }

    pub fn add(&self, sym: Sym<'tcx>) -> Result<&'tcx Sym<'tcx>, &'tcx Sym<'tcx>> {
        let mut syms = self.symbols.borrow_mut();
        if let Some(existing) = syms.get(sym.name) {
            return Err(*existing);
        }
        
        let sym_id = self.next_id.replace(self.next_id.get() + 1);
        sym.id.replace(SymId(sym_id));

        let alloced = self.arena.alloc(sym);
        syms.insert(alloced.name, alloced);
        Ok(alloced)
    }

    pub fn get(&'tcx self, name: &str) -> Option<&'tcx Sym<'tcx>> {
        let syms = self.symbols.borrow();
        let sym = syms.get(name)?;
        Some(sym)
    }
}

#[cfg(test)]
mod tests {
    use kagc_types::LitTypeVariant;

    use crate::{sym::{StorageClass, Symbol, SymbolType}, symbol_table::Symtable};

    #[test]
    fn test_symbol_addition() {
        let mut table: Symtable<Symbol> = Symtable::default();
        matches!(
            table.declare(Symbol::new(
                String::from("number"),
                LitTypeVariant::I32,
                SymbolType::Variable,
                StorageClass::GLOBAL
            )),
            Option::Some(0)
        );
        assert_eq!(
            table.declare(Symbol::new(
                String::from("number2"),
                LitTypeVariant::I32,
                SymbolType::Variable,
                StorageClass::GLOBAL
            )),
            Option::Some(1)
        );
        assert_eq!(
            table.declare(Symbol::new(
                String::from("number3"),
                LitTypeVariant::I32,
                SymbolType::Variable,
                StorageClass::GLOBAL
            )),
            Option::Some(2)
        );
    }

    #[test]
    fn test_find_symbol_index_from_its_name() {
        let mut table: Symtable<Symbol> = Symtable::default();
        table.declare(Symbol::new(
            String::from("number2"),
            LitTypeVariant::I32,
            SymbolType::Variable,
            StorageClass::GLOBAL
        ));
        table.declare(Symbol::new(
            String::from("number3"),
            LitTypeVariant::I32,
            SymbolType::Variable,
            StorageClass::GLOBAL
        ));
        table.declare(Symbol::new(
            String::from("number4"),
            LitTypeVariant::I32,
            SymbolType::Variable,
            StorageClass::GLOBAL
        ));
        assert_eq!(table.get_symbol_pos("number2"), Option::Some(0));
        assert_eq!(table.get_symbol_pos("number3"), Option::Some(1));
        assert_eq!(table.get_symbol_pos("number4"), Option::Some(2));
    }

    #[test]
    fn test_symbol_removal() {
        let mut table: Symtable<Symbol> = Symtable::default();
        table.declare(Symbol::new(
            String::from("number2"),
            LitTypeVariant::I32,
            SymbolType::Variable,
            StorageClass::GLOBAL
        ));
        table.declare(Symbol::new(
            String::from("number3"),
            LitTypeVariant::I32,
            SymbolType::Variable,
            StorageClass::GLOBAL
        ));
        table.declare(Symbol::new(
            String::from("number4"),
            LitTypeVariant::I32,
            SymbolType::Variable,
            StorageClass::GLOBAL
        ));
        assert_eq!(
            table.remove_symbol(0),
            Symbol::new(
                String::from("number2"),
                LitTypeVariant::I32,
                SymbolType::Variable,
                StorageClass::GLOBAL
            )
        );
    }
}