use std::collections::HashMap;

use kagc_types::*;
use lazy_static::lazy_static;

use super::{ASTOperation, AST};

#[derive(Clone, Debug)]
pub struct BinExpr {
    pub operation: ASTOperation,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub result_type: LitTypeVariant
}

#[derive(Clone, Debug)]
pub struct WidenExpr {
    pub from: Box<AST>,
    pub result_type: LitTypeVariant
}

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
    pub fields: Vec<RecordFieldAssignExpr>
}

#[derive(Debug, Clone)]
pub struct RecordFieldAccessExpr {
    pub rec_name: String,
    pub rec_alias: String,
    pub field_name: String,

    /// Relative stack offset.
    /// 
    /// This feild is filled up by the pass `analyzer`.
    /// This field is assigned the value 0(zero) while initializing.
    pub rel_stack_off: usize 
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

lazy_static! {
    pub static ref TYPE_PRECEDENCE_EXPR: std::collections::HashMap<u8, u8> = {
        let mut typ: std::collections::HashMap<u8, u8> = HashMap::new();
        typ.insert(LitTypeVariant::I64 as u8, 3);
        typ.insert(LitTypeVariant::I32 as u8, 2);
        typ.insert(LitTypeVariant::I16 as u8, 1);
        typ.insert(LitTypeVariant::U8 as u8, 0);
        typ
    };
}

pub trait FromExpr<T> {
    type Error;

    fn from_expr(expr: &Expr) -> Result<T, Self::Error>;
}

impl FromExpr<LitTypeVariant> for LitTypeVariant {
    type Error = ();

    fn from_expr(expr: &Expr) -> Result<LitTypeVariant, ()> {
        match expr {
            Expr::LitVal(lit) => Ok(lit.result_type),
            Expr::Binary(bin) => {
                let left_type: LitTypeVariant = LitTypeVariant::from_expr(&bin.left)?;
                let right_type: LitTypeVariant = LitTypeVariant::from_expr(&bin.right)?;
    
                if left_type == LitTypeVariant::Str || right_type == LitTypeVariant::Str {
                    if bin.operation == ASTOperation::AST_ADD {
                        return Ok(LitTypeVariant::Str);
                    } else {
                        panic!("Type mismatch: {:?} and {:?}", left_type, right_type);
                    }
                }
                if left_type != right_type {
                    let lprec: Option<&u8> = TYPE_PRECEDENCE.get(&(left_type as u8));
                    let rprec: Option<&u8> = TYPE_PRECEDENCE.get(&(right_type as u8));
                    let lp: u8 = if let Some(lp) = lprec {
                        *lp
                    } else {
                        panic!("Type precedence not defined for operation {:?}", left_type);
                    };
                    let rp: u8 = if let Some(rp) = rprec {
                        *rp
                    } else {
                        panic!("Type precedence not defined for operation {:?}", right_type);
                    };
                    if lp > rp {
                        return Ok(left_type);
                    } else {
                        return Ok(right_type);
                    }
                }
                Ok(left_type)
            },
            Expr::Ident(ident) => Ok(ident.result_type),
            Expr::FuncCall(func_call_expr) => Ok(func_call_expr.result_type),
            _ => Err(())
        }
    }
}