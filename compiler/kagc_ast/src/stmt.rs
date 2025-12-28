// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_symbol::*;
use kagc_types::{record::RecordFieldType, LitType, LitTypeVariant};

use super::Expr;

#[derive(Clone, Debug)]
pub struct FuncDeclStmt {
    /// Function ID
    pub func_id: usize,

    pub name: String,

    pub scope_id: usize,

    pub return_type: LitTypeVariant,

    pub storage_class: StorageClass,

    pub locals: Vec<usize>,

    pub func_param_types: Vec<LitTypeVariant>
}

#[derive(Clone, Debug)]
pub struct ReturnStmt {
    /// ID of the function that this `return` statement is in.
    pub func_id: usize
}

#[derive(Clone, Debug)]
pub struct VarDeclStmt {
    /// Position of the symbol in the symbol table.
    pub symtbl_pos: usize, 

    /// Name of the symbol.
    pub sym_name: String,

    /// Storage class of the symbol.
    pub class: StorageClass,

    pub value_type: LitTypeVariant,

    pub local_offset: usize,

    pub func_id: usize,

    pub symbol_type: SymbolType,

    pub default_value: Option<LitType>,
}

#[derive(Clone, Debug)]
pub struct ArrVarDeclStmt {
    pub symtbl_pos: usize,

    /// Name of the symbol
    pub sym_name: String,

    pub class: StorageClass,
    pub vals: Vec<Expr>
}

#[derive(Clone, Debug)]
pub struct AssignStmt {
    /// Name of the symbol
    pub sym_name: String
}

#[derive(Clone, Debug)]
pub struct FuncCallStmt {
    #[deprecated]
    pub symtbl_pos: usize,

    pub symbol_name: String,

    pub args: Vec<Expr>,

    pub result_type: LitTypeVariant
}

/// Represents an `if` statement and its associated lexical scope.
#[derive(Debug, Clone)]
pub struct IfStmt {
    /// Unique identifier for the scope introduced by the `if` statement.
    pub scope_id: usize,
}

/// A statement type which create its own scope.
#[derive(Clone, Debug)]
pub struct ScopingStmt {
    pub scope_id: usize
}

/// Represents a top-level `import` statement in a source file.
#[derive(Debug, Clone)]
pub struct ImportStmt {
    /// Path to the module being imported.
    pub path: String
}

#[derive(Debug, Clone)]
pub struct RecordDeclStmt {
    pub name: String,
    pub size: usize,
    pub alignment: usize,
    pub fields: Vec<RecordFieldType>
}

#[derive(Debug, Clone)]
pub struct RecordFieldStmt {
    pub name: String,
    pub typ: LitTypeVariant
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Glue,
    If(IfStmt),
    For,
    While,
    Loop,
    Break,
    ArrVarDecl(ArrVarDeclStmt),
    FuncDecl(FuncDeclStmt),
    Return(ReturnStmt),
    Assignment(AssignStmt),
    VarDecl(VarDeclStmt),
    LValue(usize), // usize for symbol table position of this left value
    LValue2 {
        name: String
    },
    FuncCall(FuncCallStmt),
    Import(ImportStmt),
    Record(RecordDeclStmt),
    RecordField(RecordFieldStmt),
    Scoping(ScopingStmt)
}