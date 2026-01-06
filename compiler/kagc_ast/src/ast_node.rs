// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_span::span::{HasSpan, SourcePos, Span};
use kagc_types::TyKind;

use crate::{AstOp, BlockStmt, Expr, FuncCallExpr, FuncDeclStmt, Stmt};

use super::NodeKind;

#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(pub usize);

pub const INVALID_NODE_ID: usize = 0xFFFFFFFA;

/// Represents a node in the Abstract Syntax Tree (AST).
///
/// The `AstNode` struct encapsulates various properties of an AST node, 
/// including its kind, operation, child nodes (left, mid, and right), 
/// value, and result type. Each node in the AST corresponds to a specific 
/// operation or expression in the source code.
/// 
/// # Fields
///
/// * `data` - An enum representing the kind of AST node (`AstData`).
/// * `op` - An enum representing the operation performed by the AST 
///   node (`AstOp`).
/// * `left` - A boxed optional child node representing the left subtree of 
///   the current node.
/// * `mid` - A boxed optional child node representing the middle subtree of 
///   the current node.
/// * `right` - A boxed optional child node representing the right subtree of 
///   the current node.
/// * `ty` - The variant of literal type representing the result type of 
///   the AST node (`TyKind`).
#[derive(Clone, Debug)]
pub struct AstNode<'tcx> {
    pub id: NodeId,
    pub data: NodeKind<'tcx>,
    pub op: AstOp,
    pub left: Option<Box<AstNode<'tcx>>>,
    pub mid: Option<Box<AstNode<'tcx>>>,
    pub right: Option<Box<AstNode<'tcx>>>,
    pub ty: Option<TyKind<'tcx>>,
    pub meta: NodeMeta,
}

impl<'tcx> AstNode<'tcx> {
    pub fn empty() -> Self {
        Self {
            id: NodeId(INVALID_NODE_ID),
            data: NodeKind::Empty,
            op: AstOp::None,
            left: None,
            right: None,
            mid: None,
            ty: None,
            meta: NodeMeta::none()
        }
    }

    pub fn new(
        kind: NodeKind<'tcx>, 
        operation: AstOp, 
        left: Option<AstNode<'tcx>>, 
        right: Option<AstNode<'tcx>>, 
        ty: Option<TyKind<'tcx>>,
    ) -> Self {
        Self {
            data: kind,
            op: operation,
            left: left.map(Box::new),
            mid: None,
            right: right.map(Box::new),
            ty,
            meta: NodeMeta::none(),
            id: NodeId(INVALID_NODE_ID)
        }
    }

    pub fn create_leaf(
        kind: NodeKind<'tcx>, 
        operation: AstOp, 
        ty: Option<TyKind<'tcx>>,
        meta: NodeMeta
    ) -> Self {
        Self {
            data: kind,
            op: operation,
            left: None,
            mid: None,
            right: None,
            ty,
            meta,
            id: NodeId(INVALID_NODE_ID)
        }
    }
    
    pub fn with_mid(
        kind: NodeKind<'tcx>, 
        op: AstOp, 
        left: Option<AstNode<'tcx>>, 
        mid: Option<AstNode<'tcx>>, 
        right: Option<AstNode<'tcx>>, 
        ty: Option<TyKind<'tcx>>
    ) -> Self {
        Self {
            data: kind,
            op,
            left: left.map(Box::new),
            mid: mid.map(Box::new),
            right: right.map(Box::new),
            ty,
            meta: NodeMeta::none(),
            id: NodeId(INVALID_NODE_ID)
        }
    }

    pub fn with_meta(
        kind: NodeKind<'tcx>,
        op: AstOp,
        left: Option<AstNode<'tcx>>,
        mid: Option<AstNode<'tcx>>,
        right: Option<AstNode<'tcx>>,
        ty: Option<TyKind<'tcx>>,
        meta: NodeMeta
    ) -> Self {
        Self {
            data: kind,
            op,
            left: left.map(Box::new),
            mid: mid.map(Box::new),
            right: right.map(Box::new),
            ty,
            meta,
            id: NodeId(INVALID_NODE_ID)
        }
    }

    pub fn linearize(&self) -> Vec<&AstNode<'tcx>> {
        let mut output: Vec<&AstNode> = vec![];
        if self.op == AstOp::Glue {
            if let Some(left) = &self.left {
                output.extend(left.linearize());
            }
            if let Some(right) = &self.right {
                output.extend(right.linearize());
            }
        }
        else {
            output.push(self);
        }
        output
    }

    pub fn linearize_mut(&mut self) -> Vec<&mut AstNode<'tcx>> {
        let mut output: Vec<&mut AstNode> = vec![];
        if self.op == AstOp::Glue {
            if let Some(left) = &mut self.left {
                output.extend(left.linearize_mut());
            }

            if let Some(right) = &mut self.right {
                output.extend(right.linearize_mut());
            }
        }
        else {
            output.push(self);
        }
        output
    }

    #[allow(unused_parens)]
    pub fn contains_operation(&self, op: AstOp) -> bool {
        fn check_node_for_operation(node: &Option<Box<AstNode>>, op: AstOp) -> bool {
            if let Some(n) = node {
                if n.op == op {
                    return true;
                }
                return n.children().any(|child| check_node_for_operation(child, op));
            }
            false
        }
        self.children().any(|child| check_node_for_operation(child, op))
    }

    fn children(&self) -> impl Iterator<Item = &Option<Box<AstNode>>> {
        [&self.left, &self.mid, &self.right].into_iter()
    }

    pub fn as_expr(&self) -> Option<&Expr<'tcx>> {
        match &self.data {
            NodeKind::ExprAST(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_expr_mut(&mut self) -> Option<&mut Expr<'tcx>> {
        match &mut self.data {
            NodeKind::ExprAST(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_stmt(&self) -> Option<&Stmt<'tcx>> {
        match &self.data {
            NodeKind::StmtAST(stmt) => Some(stmt),
            _ => None,
        }
    }

    pub fn as_stmt_mut(&mut self) -> Option<&mut Stmt<'tcx>> {
        match &mut self.data {
            NodeKind::StmtAST(stmt) => Some(stmt),
            _ => None,
        }
    }
    
    pub fn expect_func_decl_stmt(&self) -> &FuncDeclStmt<'tcx> {
        let stmt = self.as_stmt().expect("expected stmt");
        stmt.as_func_decl().expect("expected function declaration stmt")
    }

    pub fn expect_func_decl_stmt_mut(&mut self) -> &mut FuncDeclStmt<'tcx> {
        let stmt = self.as_stmt_mut().expect("expected stmt");
        stmt.as_func_decl_mut().expect("expected function declaration stmt")
    } 

    pub fn expect_block_stmt_mut(&mut self) -> &mut BlockStmt<'tcx> {
        let stmt = self.as_stmt_mut().expect("expected stmt");
        stmt.as_block_mut().expect("expected block stmt")
    }

    pub fn expect_if_stmt(&self) {
        let stmt = self.as_stmt().expect("expected stmt");
        stmt.as_if().expect("expected if stmt");
    }

    pub fn expect_func_call_expr_mut(&mut self) -> &mut FuncCallExpr<'tcx> {
        let expr = self.as_expr_mut().expect("expected expression");
        expr.as_func_call_mut().expect("expected function call")
    }
}

#[macro_export]
macro_rules! contains_ops {
    ($ast:expr, $($op:expr),+ $(,)?) => {{
        $(
            if $ast.contains_operation($op) {
                true
            } else
        )+
        { false }
    }};
}

/// `HasSpan` implemented for AST node.
impl HasSpan for AstNode<'_> {
    fn span(&self) -> &Span {
        &self.meta.span
    }
}

/// Metadata associated with an AST node.
///
/// Contains the source code location (`span`) and a list of informational notes
/// (e.g., warnings, hints) relevant to the node.
#[derive(Clone, Debug)]
pub struct NodeMeta {
    pub span:       Span,
    pub notes:      Vec<String>,
    pub gc_alloced: bool
}

impl NodeMeta {
    pub fn none() -> Self {
        Self {
            span: Span::new(
                0, 
                SourcePos{
                    column: 1, 
                    line: 1
                }, 
                SourcePos { 
                    line: 0, 
                    column: 0 
                }
            ),
            notes: vec![],
            gc_alloced: false
        }
    }

    pub fn new(span: Span, notes: Vec<String>) -> Self {
        Self {
            span,
            notes,
            gc_alloced: false
        }
    }

    pub fn add_note(&mut self, note: &str) {
        self.notes.push(note.to_string());
    }
}