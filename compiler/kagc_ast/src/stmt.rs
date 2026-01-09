// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_symbol::function::{FuncId, FuncParam};
use kagc_symbol::{StorageClass, SymTy};
use kagc_types::record::RecordFieldType;
use kagc_types::TyKind;

use crate::AstNode;

use super::Expr;

#[derive(Clone, Debug)]
pub struct FuncDeclStmt<'tcx> {
    pub id: FuncId,
    pub name: &'tcx str,
    pub ty: TyKind<'tcx>, // return type
    pub storage_class: StorageClass,
    pub params: Vec<FuncParam<'tcx>>,
    pub param_types: Vec<TyKind<'tcx>>
}

#[derive(Clone, Debug)]
pub struct ReturnStmt {
    /// ID of the function that this `return` statement is in.
    pub func_id: FuncId
}

#[derive(Clone, Debug)]
pub struct VarDeclStmt<'tcx> {
    /// Position of the symbol in the symbol table.
    pub symtbl_pos: usize, 

    /// Name of the symbol.
    pub sym_name: &'tcx str,

    /// Storage class of the symbol.
    pub class: StorageClass,

    pub ty: TyKind<'tcx>,

    pub local_offset: usize,

    pub func_id: usize,

    pub symbol_type: SymTy<'tcx>
}

#[derive(Clone, Debug)]
pub struct ArrVarDeclStmt<'tcx> {
    pub symtbl_pos: usize,

    /// Name of the symbol
    pub sym_name: String,

    pub class: StorageClass,
    pub vals: Vec<Expr<'tcx>>
}

#[derive(Clone, Debug)]
pub struct AssignStmt {
    /// Name of the symbol
    pub sym_name: String
}

#[derive(Clone, Debug)]
pub struct FuncCallStmt<'tcx> {
    #[deprecated]
    pub symtbl_pos: usize,
    pub symbol_name: String,
    pub args: Vec<Expr<'tcx>>,
    pub ty: TyKind<'tcx>
}

/// Represents a top-level `import` statement in a source file.
#[derive(Debug, Clone)]
pub struct ImportStmt<'tcx> {
    /// Path to the module being imported.
    pub path: &'tcx str
}

#[derive(Debug, Clone)]
pub struct RecordDeclStmt<'tcx> {
    pub name: &'tcx str,
    pub size: usize,
    pub alignment: usize,
    pub fields: Vec<RecordFieldType<'tcx>>
}

#[derive(Debug, Clone)]
pub struct RecordFieldStmt<'tcx> {
    pub name: String,
    pub ty: TyKind<'tcx>
}

#[derive(Debug, Clone)]
pub struct BlockStmt<'tcx> {
    pub statements: Vec<AstNode<'tcx>>
}

#[derive(Clone, Debug)]
pub enum Stmt<'tcx> {
    Glue,
    If,
    For,
    While,
    Loop,
    Break,
    ArrVarDecl(ArrVarDeclStmt<'tcx>),
    FuncDecl(FuncDeclStmt<'tcx>),
    Return(ReturnStmt),
    Assignment(AssignStmt),
    VarDecl(VarDeclStmt<'tcx>),
    LValue(usize), // usize for symbol table position of this left value
    LValue2 {
        name: String
    },
    FuncCall(FuncCallStmt<'tcx>),
    Import(ImportStmt<'tcx>),
    Record(RecordDeclStmt<'tcx>),
    RecordField(RecordFieldStmt<'tcx>),
    Scoping,

    /// Block statement
    Block(BlockStmt<'tcx>)
}

impl<'tcx> Stmt<'tcx> {
    pub fn as_block(&self) -> Option<&BlockStmt<'tcx>> {
        match self {
            Stmt::Block(stmt) => Some(stmt),
            _ => None
        }
    }

    pub fn as_block_mut(&mut self) -> Option<&mut BlockStmt<'tcx>> {
        match self {
            Stmt::Block(stmt) => Some(stmt),
            _ => None
        }
    }

    pub fn as_func_decl(&self) -> Option<&FuncDeclStmt<'tcx>> {
        match self {
            Stmt::FuncDecl(stmt) => Some(stmt),
            _ => None
        }
    }

    pub fn as_func_decl_mut(&mut self) -> Option<&mut FuncDeclStmt<'tcx>> {
        match self {
            Stmt::FuncDecl(stmt) => Some(stmt),
            _ => None
        }
    }

    pub fn as_if(&self) -> Option<()> {
        match self {
            Stmt::If => Some(()),
            _ => None
        }
    }

    pub fn as_loop(&self) -> Option<()> {
        match self {
            Stmt::Loop => Some(()),
            _ => None
        }
    }

    pub fn as_record_decl(&self) -> Option<&RecordDeclStmt<'tcx>> {
        match self {
            Stmt::Record(stmt) => Some(stmt),
            _ => None
        }
    }

    pub fn as_record_decl_mut(&mut self) -> Option<&mut RecordDeclStmt<'tcx>> {
        match self {
            Stmt::Record(stmt) => Some(stmt),
            _ => None
        }
    }

    pub fn as_return(&self) -> Option<&ReturnStmt> {
        match self {
            Stmt::Return(stmt) => Some(stmt),
            _ => None
        }
    }

    pub fn as_return_mut(&mut self) -> Option<&mut ReturnStmt> {
        match self {
            Stmt::Return(stmt) => Some(stmt),
            _ => None
        }
    }

    pub fn as_var_decl(&self) -> Option<&VarDeclStmt<'tcx>> {
        match self {
            Stmt::VarDecl(stmt) => Some(stmt),
            _ => None
        }
    }

    pub fn as_var_decl_mut(&mut self) -> Option<&mut VarDeclStmt<'tcx>> {
        match self {
            Stmt::VarDecl(stmt) => Some(stmt),
            _ => None
        }
    }
}