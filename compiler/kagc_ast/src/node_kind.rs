// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use super::{Expr, Stmt};

#[derive(Clone, Debug)]
pub enum NodeKind<'tcx> {
    StmtAST(Stmt<'tcx>),
    ExprAST(Expr<'tcx>),
    Empty
}

impl<'tcx> NodeKind<'tcx> {
    /// Checks if the NodeKind is an expression variant.
    pub fn is_expr(&self) -> bool {
        matches!(self, NodeKind::ExprAST(_))
    }

    pub fn expr(self) -> Option<Expr<'tcx>> {
        match self {
            Self::ExprAST(expr) => Some(expr),
            _ => None
        }
    }

    pub fn as_expr(&self) -> Option<&Expr<'tcx>> {
        if let Self::ExprAST(expr) = self {
            Some(expr)
        } 
        else {
            None
        }
    }

    pub fn as_expr_mut(&mut self) -> Option<&mut Expr<'tcx>> {
        if let Self::ExprAST(expr) = self {
            Some(expr)
        } 
        else {
            None
        }
    }

    /// Checks if the ASTKind is a statement variant.
    pub fn is_stmt(&self) -> bool {
        matches!(self, NodeKind::StmtAST(_))
    }

    pub fn stmt(self) -> Option<Stmt<'tcx>> {
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

    pub fn as_stmt_mut(&mut self) -> Option<&mut Stmt<'tcx>> {
        if let Self::StmtAST(stmt) = self {
            Some(stmt)
        } 
        else {
            None
        }
    }
}