// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_symbol::function::FunctionId;
use kagc_types::*;

use super::{ASTOperation, AST};

/// A binary expression AST node.
#[derive(Clone, Debug)]
pub struct BinExpr {
    pub operation: ASTOperation,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub result_type: LitTypeVariant,
}

/// A widen expression AST node.
#[derive(Clone, Debug)]
pub struct WidenExpr {
    pub from: Box<AST>,
    pub result_type: LitTypeVariant
}

/// A identifier expression AST node.
#[derive(Clone, Debug)]
pub struct IdentExpr {
    /// Name of the symbol
    pub sym_name: String,

    /// Result type of the symbol
    pub result_type: LitTypeVariant
}

#[derive(Clone, Debug)]
pub struct LitValExpr {
    pub value: LitType,
    pub result_type: LitTypeVariant,
}

#[derive(Clone, Debug)]
pub struct SubscriptExpr {
    pub index: Box<Expr>, // subscript index expression
    pub symtbl_pos: usize, // position of the symbol inside symbol table that is being subscripted
    pub result_type: LitTypeVariant
}

/// Represents the original index of a function argument in the source code.
/// This helps preserve argument order during transformations like sorting.
pub type ArgIdx = usize;

/// Represents a function argument, storing both:
/// - The original argument index (`ArgIdx`) to preserve order.
/// - The actual expression (`Expr`) representing the argument.
pub type FuncArg = (ArgIdx, Expr);


#[derive(Clone, Debug)]
pub struct FuncCallExpr {
    /// Name of the called function
    pub symbol_name: String,

    pub id: FunctionId,

    pub result_type: LitTypeVariant, // function return type
    // args
    pub args: Vec<FuncArg>
}

#[derive(Clone, Debug)]
pub struct RecordFieldAssignExpr {
    pub name: String,
    pub value: Box<Expr>,
    pub offset: usize
}

#[derive(Clone, Debug)]
pub struct RecordCreationExpr {
    pub name: String,
    pub rec_alias: String,
    pub fields: Vec<RecordFieldAssignExpr>,
    pub pool_idx: usize
}

#[derive(Debug, Clone)]
pub struct RecordFieldAccessExpr {
    pub rec_name: String,
    pub rec_alias: String,
    // pub field_name: String,

    pub field_chain: Vec<String>,

    /// Relative stack offset.
    /// 
    /// This feild is filled up by the pass `analyzer`.
    /// This field is assigned the value 0(zero) while initializing.
    pub rel_stack_off: usize,

    pub result_type: LitTypeVariant
}

#[derive(Clone, Debug)]
pub enum Expr {
    Binary(BinExpr),
    
    Widen(WidenExpr),
    
    Ident(IdentExpr),
    
    LitVal(LitValExpr),
    
    Subscript(SubscriptExpr),
    
    FuncCall(FuncCallExpr),

    RecordCreation(RecordCreationExpr),

    RecordFieldAssign(RecordFieldAssignExpr),

    RecordFieldAccess(RecordFieldAccessExpr),

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

impl Expr {
    pub fn result_type(&self) -> LitTypeVariant {
        match self {
            Expr::Binary(bin_expr) => bin_expr.result_type.clone(),
            Expr::Widen(widen_expr) => widen_expr.result_type.clone(),
            Expr::Ident(ident_expr) => ident_expr.result_type.clone(),
            Expr::LitVal(lit_val_expr) => lit_val_expr.result_type.clone(),
            Expr::Subscript(subscript_expr) => subscript_expr.result_type.clone(),
            Expr::FuncCall(func_call_expr) => func_call_expr.result_type.clone(),
            Expr::RecordFieldAccess(record_field_access_expr) => record_field_access_expr.result_type.clone(),
            Expr::RecordCreation(record_creation_expr) => LitTypeVariant::Record{name: record_creation_expr.name.clone()},
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

    pub fn as_litval(&self) -> Option<&LitValExpr> {
        match self {
            Expr::LitVal(lit_val_expr) => Some(lit_val_expr),
            _ => None
        }
    }

    pub fn as_binary(&self) -> Option<&BinExpr> {
        match self {
            Expr::Binary(bin) => Some(bin),
            _ => None
        }
    }
}