// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#![allow(non_camel_case_types)]

use kagc_token::{FromTokenKind, TokenKind};

#[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
pub enum AstOp {
    // below this are relational operators
    EqEq = 0,  // equal equal
    NEq,   // not equal
    LtEq,  // less than or equal to
    GtEq,  // greate than equal to
    GThan, // greater than
    LThan, // less than

    None,     // used as a placeholder
    Add = 10, // an AST node with "+" as the root node
    Subtract, // an AST node with "-" as the root node
    Multiply, // an AST node with "*" as the root node
    Divide,   // an AST node with "/" as the root node
    // end of relational operators

    Str, // string literal node
    IntLit, // a leaf AST node with literal integer value
    Ident,  // a leaf AST node with an identifier name
    LvIdent,
    Assign,
    
    #[deprecated]
    Glue,
    If,
    Else,

    Import,
    
    // Record related operations
    RecDecl,
    RecFieldDecl,
    RecCreate,
    RecFieldAccess,

    While,
    For,
    Block,
    Loop,
    Break,
    Func,
    FuncCall,
    Return,       // return statement AST node
    Widen,        // need to widen the tree
    ArrayAccess, // access array element
    VarDecl,
    ArrayVarDecl,

    /// Null AST
    Null
}

impl FromTokenKind<AstOp> for AstOp {
    fn from_token_kind(tk: TokenKind) -> Option<AstOp> {
        match tk {
            TokenKind::T_PLUS => Some(Self::Add),
            TokenKind::T_MINUS => Some(Self::Subtract),
            TokenKind::T_STAR => Some(Self::Multiply),
            TokenKind::T_SLASH => Some(Self::Divide),
            TokenKind::T_EQEQ => Some(Self::EqEq),
            TokenKind::T_NEQ => Some(Self::NEq),
            TokenKind::T_GTHAN => Some(Self::GThan),
            TokenKind::T_LTHAN => Some(Self::LThan),
            TokenKind::T_GTEQ => Some(Self::GtEq),
            TokenKind::T_LTEQ => Some(Self::LtEq),
            _ => unimplemented!("Not implemented for now!"),
        }
    }
}