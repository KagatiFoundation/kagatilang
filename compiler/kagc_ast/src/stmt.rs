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

use kagc_symbol::*;
use kagc_types::LitTypeVariant;

use super::Expr;

#[derive(Clone, Debug)]
pub struct FuncDeclStmt {
    /// Function ID
    pub func_id: usize, 

    pub name: String,

    pub scope_id: usize
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
    pub class: StorageClass
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

/// Represents a top-level `import` statement in a source file.
#[derive(Debug, Clone)]
pub struct ImportStmt {
    /// Path to the module being imported.
    pub path: String
}

#[derive(Debug, Clone)]
pub struct RecordDeclStmt {
    pub name: String,
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
    RecordField(RecordFieldStmt)
}