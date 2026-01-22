// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_symbol::function::FuncId;
use kagc_types::TyKind;

use crate::Literal;

use super::{AstOp, AstNode};

/// A binary expression AST node.
#[derive(Clone, Debug)]
pub struct BinExpr<'tcx> {
    pub operation: AstOp,
    pub left: Box<Expr<'tcx>>,
    pub right: Box<Expr<'tcx>>,
    pub ty: TyKind<'tcx>,
}

/// A widen expression AST node.
#[derive(Clone, Debug)]
pub struct WidenExpr<'tcx> {
    pub from: Box<AstNode<'tcx>>,
    pub ty: TyKind<'tcx>
}

/// A identifier expression AST node.
#[derive(Clone, Debug)]
pub struct IdentExpr<'tcx> {
    /// Name of the symbol
    pub sym_name: &'tcx str,

    /// Result type of the symbol
    pub ty: TyKind<'tcx>
}

#[derive(Clone, Debug)]
pub struct LitValExpr<'tcx> {
    pub value: Literal<'tcx>,
    pub ty: TyKind<'tcx>
}

#[derive(Clone, Debug)]
pub struct SubscriptExpr<'tcx> {
    pub index: Box<Expr<'tcx>>, // subscript index expression
    pub symtbl_pos: usize, // position of the symbol inside symbol table that is being subscripted
    pub ty: TyKind<'tcx>
}

/// Represents the original index of a function argument in the source code.
/// This helps preserve argument order during transformations like sorting.
pub type ArgIdx = usize;

/// Represents a function argument, storing both:
/// - The original argument index (`ArgIdx`) to preserve order.
/// - The actual expression (`Expr`) representing the argument.
pub type FuncArg<'tcx> = (ArgIdx, Expr<'tcx>);

#[derive(Clone, Debug)]
pub struct FuncCallExpr<'tcx> {
    /// Name of the called function
    pub symbol_name: &'tcx str,

    pub id: FuncId,

    pub ty: TyKind<'tcx>,
    // args
    pub args: Vec<FuncArg<'tcx>>
}

#[derive(Clone, Debug)]
pub struct RecordFieldAssignExpr<'tcx> {
    pub name: &'tcx str,
    pub value: Box<Expr<'tcx>>,
    pub offset: usize
}

#[derive(Clone, Debug)]
pub struct RecordCreationExpr<'tcx> {
    pub name: &'tcx str,
    pub rec_alias: &'tcx str,
    pub fields: Vec<RecordFieldAssignExpr<'tcx>>,
    pub pool_idx: usize,
}

#[derive(Debug, Clone)]
pub struct RecordFieldAccessExpr<'tcx> {
    pub rec_name: &'tcx str,
    pub rec_alias: &'tcx str,
    pub field_name: &'tcx str, // the field being accessed

    /// Relative stack offset.
    /// 
    /// This feild is filled up by the pass `analyzer`.
    /// This field is assigned the value 0(zero) while initializing.
    pub rel_stack_off: usize,

    pub ty: TyKind<'tcx>
}

#[derive(Clone, Debug)]
pub enum Expr<'tcx> {
    Binary(BinExpr<'tcx>),
    
    Widen(WidenExpr<'tcx>),
    
    Ident(IdentExpr<'tcx>),
    
    LitVal(LitValExpr<'tcx>),
    
    Subscript(SubscriptExpr<'tcx>),
    
    FuncCall(FuncCallExpr<'tcx>),

    RecordCreation(RecordCreationExpr<'tcx>),

    RecordFieldAssign(RecordFieldAssignExpr<'tcx>),

    RecordFieldAccess(RecordFieldAccessExpr<'tcx>),

    /// Null expression
    Null
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Binary,
    Literal,
    Call,
    RecordCreation
}

impl<'tcx> Expr<'tcx> {
    pub fn result_type(&self) -> TyKind<'tcx> {
        match self {
            Expr::Binary(bin_expr) => bin_expr.ty,
            Expr::Widen(widen_expr) => widen_expr.ty,
            Expr::Ident(ident_expr) => ident_expr.ty,
            Expr::LitVal(lit_val_expr) => lit_val_expr.ty,
            Expr::Subscript(subscript_expr) => subscript_expr.ty,
            Expr::FuncCall(func_call_expr) => func_call_expr.ty,
            Expr::RecordFieldAccess(record_field_access_expr) => record_field_access_expr.ty,
            Expr::RecordCreation(_rec) => todo!(),
            Expr::RecordFieldAssign(_record_field_assign_expr) => todo!(),
            Expr::Null => todo!(),
        }
    }

    pub fn kind(&self) -> ExprKind {
        match self {
            Expr::Binary(_) => ExprKind::Binary,
            _ => todo!()
        }
    }

    pub fn as_litval(&self) -> Option<&LitValExpr<'tcx>> {
        match self {
            Expr::LitVal(lit_val_expr) => Some(lit_val_expr),
            _ => None
        }
    }

    pub fn as_binary(&self) -> Option<&BinExpr<'tcx>> {
        match self {
            Expr::Binary(bin) => Some(bin),
            _ => None
        }
    }

    pub fn as_func_call(&self) -> Option<&FuncCallExpr<'tcx>> {
        match self {
            Expr::FuncCall(call) => Some(call),
            _ => None
        }
    }

    pub fn as_func_call_mut(&mut self) -> Option<&mut FuncCallExpr<'tcx>> {
        match self {
            Expr::FuncCall(call) => Some(call),
            _ => None
        }
    }
}