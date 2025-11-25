// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use super::{Expr, Stmt};

#[derive(Clone, Debug)]
pub enum ASTKind {
    StmtAST(Stmt),
    ExprAST(Expr),
    Empty
}

impl AsRef<ASTKind> for ASTKind {
    fn as_ref(&self) -> &ASTKind {
        self
    }
}

impl AsMut<ASTKind> for ASTKind {
    fn as_mut(&mut self) -> &mut ASTKind {
        self
    }
}

impl ASTKind {
    /// Checks if the ASTKind is an expression variant.
    pub fn is_expr(&self) -> bool {
        matches!(self, ASTKind::ExprAST(_))
    }

    pub fn expr(self) -> Option<Expr> {
        match self {
            Self::ExprAST(expr) => Some(expr),
            _ => None
        }
    }

    pub fn as_expr(&self) -> Option<&Expr> {
        if let Self::ExprAST(expr) = self {
            Some(expr)
        } 
        else {
            None
        }
    }

    pub fn as_expr_mut(&mut self) -> Option<&mut Expr> {
        if let Self::ExprAST(expr) = self {
            Some(expr)
        } 
        else {
            None
        }
    }

    /// Checks if the ASTKind is a statement variant.
    pub fn is_stmt(&self) -> bool {
        matches!(self, ASTKind::StmtAST(_))
    }

    pub fn stmt(self) -> Option<Stmt> {
        match self {
            Self::StmtAST(stmt) => Some(stmt),
            _ => None
        }
    }

    pub fn as_stmt(&self) -> Option<&Stmt> {
        if let Self::StmtAST(stmt) = self {
            Some(stmt)
        } 
        else {
            None
        }
    }

    pub fn as_stmt_mut(&mut self) -> Option<&mut Stmt> {
        if let Self::StmtAST(stmt) = self {
            Some(stmt)
        } 
        else {
            None
        }
    }

    /// Unwraps the ASTKind, returning the contained statement.
    ///
    /// # Panics
    /// Panics if the ASTKind is not a statement kind.
    #[deprecated]
    pub fn unwrap_stmt(self) -> Stmt {
        match self {
            Self::StmtAST(stmt) => stmt,
            _ => panic!("ASTKind is not a statement kind!")
        }
    }

    /// Unwraps the ASTKind, returning the contained expression.
    ///
    /// # Panics
    /// Panics if the ASTKind is not an expression kind.
    #[deprecated]
    pub fn unwrap_expr(self) -> Expr {
        match self {
            Self::ExprAST(expr) => expr,
            _ => panic!("ASTKind is not an expression kind!")
        }
    }
}