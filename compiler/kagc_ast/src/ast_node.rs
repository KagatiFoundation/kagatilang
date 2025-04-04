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

#![allow(non_camel_case_types)]

use kagc_token::{FromTokenKind, Token, TokenKind};
use kagc_types::{BTypeComparable, LitTypeVariant, TypeSized};

use super::ASTKind;

#[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
pub enum ASTOperation {
    // below this are relational operators
    AST_EQEQ = 0,  // equal equal
    AST_NEQ,   // not equal
    AST_LTEQ,  // less than or equal to
    AST_GTEQ,  // greate than equal to
    AST_GTHAN, // greater than
    AST_LTHAN, // less than

    AST_NONE,     // used as a placeholder
    AST_ADD = 10, // an AST node with "+" as the root node
    AST_SUBTRACT, // an AST node with "-" as the root node
    AST_MULTIPLY, // an AST node with "*" as the root node
    AST_DIVIDE,   // an AST node with "/" as the root node
    // end of relational operators
    AST_INTLIT, // a leaf AST node with literal integer value
    AST_IDENT,  // a leaf AST node with an identifier name
    AST_LVIDENT,
    AST_ASSIGN,
    AST_GLUE,
    AST_IF,
    AST_WHILE,
    AST_LOOP,
    AST_BREAK,
    AST_FUNCTION,
    AST_FUNC_CALL,
    AST_RETURN,       // return statement AST node
    AST_ADDR,         // for address-of operator
    AST_DEREF,        // for dereferencing operator
    AST_WIDEN,        // need to widen the tree
    AST_ARRAY_ACCESS, // access array element
    AST_STRLIT, // string literal node
    AST_VAR_DECL,
    AST_ARR_VAR_DECL
}

impl FromTokenKind<ASTOperation> for ASTOperation {
    fn from_token_kind(tk: TokenKind) -> Option<ASTOperation> {
        match tk {
            TokenKind::T_PLUS => Some(Self::AST_ADD),
            TokenKind::T_MINUS => Some(Self::AST_SUBTRACT),
            TokenKind::T_STAR => Some(Self::AST_MULTIPLY),
            TokenKind::T_SLASH => Some(Self::AST_DIVIDE),
            TokenKind::T_EQEQ => Some(Self::AST_EQEQ),
            TokenKind::T_NEQ => Some(Self::AST_NEQ),
            TokenKind::T_GTHAN => Some(Self::AST_GTHAN),
            TokenKind::T_LTHAN => Some(Self::AST_LTHAN),
            TokenKind::T_GTEQ => Some(Self::AST_GTEQ),
            TokenKind::T_LTEQ => Some(Self::AST_LTEQ),
            _ => unimplemented!("Not implemented for now!"),
        }
    }
}

/// Represents a node in the Abstract Syntax Tree (AST).
///
/// The `AST` struct encapsulates various properties of an AST node, 
/// including its kind, operation, child nodes (left, mid, and right), 
/// value, and result type. Each node in the AST corresponds to a specific 
/// operation or expression in the source code.
/// 
/// # Fields
///
/// * `kind` - An enum representing the kind of AST node (`ASTKind`).
/// * `operation` - An enum representing the operation performed by the AST 
///     node (`ASTOperation`).
/// * `left` - A boxed optional child node representing the left subtree of 
///     the current node.
/// * `mid` - A boxed optional child node representing the middle subtree of 
///     the current node.
/// * `right` - A boxed optional child node representing the right subtree of 
///     the current node.
/// * `value` - An optional literal value associated with the AST node (`LitType`).
/// * `result_type` - The variant of literal type representing the result type of 
///     the AST node (`LitTypeVariant`).
///
/// # Examples
///
/// ```
/// // Create a new AST node representing an addition operation
/// let ast_node = AST {
///     kind: ASTKind::Expr(...),
///     operation: ASTOperation::Addition,
///     left: Box::new(Some(left_ast_node)),
///     mid: Box::new(None), // No middle subtree
///     right: Box::new(Some(right_ast_node)),
///     value: None, // No associated literal value
///     result_type: LitTypeVariant::Int, // Result type is integer
/// };
/// ```
#[derive(Clone, Debug)]
pub struct AST {
    pub kind: ASTKind,
    pub operation: ASTOperation,
    pub left: Option<Box<AST>>,
    pub mid: Option<Box<AST>>,
    pub right: Option<Box<AST>>,
    pub result_type: LitTypeVariant,
    pub start_token: Option<Token>,
    pub end_token: Option<Token>
}

impl AST {
    pub fn empty() -> Self {
        Self {
            kind: ASTKind::Empty,
            operation: ASTOperation::AST_NONE,
            left: None,
            right: None,
            mid: None,
            result_type: LitTypeVariant::None,
            start_token: None,
            end_token: None
        }
    }

    pub fn new(
        kind: ASTKind, 
        operation: ASTOperation, 
        left: Option<AST>, 
        right: Option<AST>, 
        result_type: LitTypeVariant
    ) -> Self {
        Self {
            kind,
            operation,
            left: left.map(Box::new),
            mid: None,
            right: right.map(Box::new),
            result_type,
            start_token: None,
            end_token: None
        }
    }

    pub fn create_leaf(
        kind: ASTKind,
        operation: ASTOperation, 
        result_type: LitTypeVariant,
        start_tok: Option<Token>,
        end_tok: Option<Token>
    ) -> Self {
        Self {
            kind,
            operation,
            left: None,
            mid: None,
            right: None,
            result_type,
            start_token: start_tok,
            end_token: end_tok
        }
    }
    
    pub fn with_mid(
        kind: ASTKind, 
        op: ASTOperation, 
        left: Option<AST>, 
        mid: Option<AST>, 
        right: Option<AST>, 
        result_type: LitTypeVariant
    ) -> Self {
        Self {
            kind,
            operation: op,
            left: left.map(Box::new),
            mid: mid.map(Box::new),
            right: right.map(Box::new),
            result_type,
            start_token: None,
            end_token: None
        }
    }

    pub fn linearize(&self) -> Vec<&AST> {
        let mut output: Vec<&AST> = vec![];

        if self.operation == ASTOperation::AST_GLUE {
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

    pub fn linearize_mut(&mut self) -> Vec<&mut AST> {
        let mut output: Vec<&mut AST> = vec![];

        if self.operation == ASTOperation::AST_GLUE {
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
    pub fn contains_operation(&self, op: ASTOperation) -> bool {
        fn check_node_for_operation(node: &Option<Box<AST>>, op: ASTOperation) -> bool {
            if let Some(n) = node {
                if n.operation == op {
                    return true;
                }
                return (
                    check_node_for_operation(&n.left, op) || 
                    check_node_for_operation(&n.right, op)
                );
            }
            false
        }
        check_node_for_operation(&self.left, op) || 
        check_node_for_operation(&self.right, op)
    }
}

impl BTypeComparable for AST {
    fn cmp(&self, other: &AST) -> bool {
        self.result_type == other.result_type
    }
    
    fn variant(&self) -> LitTypeVariant {
        self.result_type
    }
}

impl TypeSized for AST {
    fn type_size(&self) -> usize {
        self.result_type.size()
    }
}

pub fn are_compatible_for_operation<T: BTypeComparable + TypeSized>(
    left: &T, 
    right: &T, 
    op: ASTOperation
) -> (bool, LitTypeVariant) {
    let ltype: LitTypeVariant = left.variant();
    let rtype: LitTypeVariant = right.variant();
    if ltype == rtype {
        return (true, ltype);
    }
    let mut larger_type: LitTypeVariant = ltype;
    let lsize: usize = left.type_size();
    let rsize: usize = right.type_size();
    if rsize > lsize {
        larger_type = rtype;
    }
    match (ltype, rtype) {
        (LitTypeVariant::I32, LitTypeVariant::U8) |
        (LitTypeVariant::U8, LitTypeVariant::I32) | 
        (LitTypeVariant::I64, LitTypeVariant::I32) |
        (LitTypeVariant::I64, LitTypeVariant::U8) | 
        (LitTypeVariant::I32, LitTypeVariant::I64) | 
        (LitTypeVariant::U8, LitTypeVariant::I64) => {
            if matches!(
                op, 
                ASTOperation::AST_ADD 
                | ASTOperation::AST_SUBTRACT 
                | ASTOperation::AST_MULTIPLY 
                | ASTOperation::AST_DIVIDE 
                | ASTOperation::AST_LTHAN
                | ASTOperation::AST_GTHAN
            ) {
                (true, larger_type)
            } else {
                (false, larger_type)
            }
        },
        _ => (false, larger_type)
    }
}