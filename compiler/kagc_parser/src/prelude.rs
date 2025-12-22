// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_ast::{AST, BinExpr, Expr, Stmt};
use kagc_errors::diagnostic::{Diagnostic, DiagnosticBag};
use kagc_lexer::Tokenizer;

use crate::{Parser, session::ParserSession};

/// A type alias representing the result of parsing, which can either
/// be an AST node on successful parsing or a ParseError indicating a
/// parsing failure.
pub type ParseResult = Result<AST, Box<Diagnostic>>;

pub struct ParseOutput<T> {
    pub result: Option<T>,
    pub diagnostics: DiagnosticBag
}

pub fn parse_single_statement(source: &str) -> ParseResult {
    let session = ParserSession::from_string(source);
    let lexer = Tokenizer::new();
    let mut parser = Parser::new(session, lexer);
    parser.parse_single_stmt()
}

pub fn parse_expression(source: &str) -> Option<Expr> {
    let session = ParserSession::from_string(source);
    let lexer = Tokenizer::new();
    let mut parser = Parser::new(session, lexer);
    parser.parse_expression()
}

pub fn parse_statement(source: &str) -> Option<Stmt> {
    let session = ParserSession::from_string(source);
    let lexer = Tokenizer::new();
    let mut parser = Parser::new(session, lexer);
    parser.parse_statement()
}

pub(crate) fn as_expr(ast: &AST) -> &Expr {
    match &ast.kind {
        kagc_ast::ASTKind::ExprAST(expr) => expr,
        _ => panic!("expected expr")
    }
}

pub(crate) fn as_stmt(ast: &AST) -> &Stmt {
    match &ast.kind {
        kagc_ast::ASTKind::StmtAST(stmt) => stmt,
        _ => panic!("expected stmt")
    }
}

pub(crate) fn as_binary(expr: &Expr) -> &BinExpr {
    match expr {
        Expr::Binary(bin) => bin,
        _ => panic!("expected bin expr")
    }
}

pub(crate) fn assert_binary(ast: &Expr) {
    matches!(ast, Expr::Binary(_));
}

#[cfg(test)]
mod parser_prelude_tests {
    use kagc_ast::{Expr, LitValExpr};
    use kagc_types::LitType;

    use crate::prelude::{as_binary, assert_binary, parse_expression};

    fn assert_lit_u8(expr: &Expr, v: u8) {
        match expr {
            Expr::LitVal(LitValExpr { value: LitType::U8(x), .. }) => assert_eq!(*x, v),
            _ => panic!("expected u8 literal"),
        }
    }

    #[test]
    fn test_parse_empty_input() {
        let empty_ast = parse_expression("");
        assert!(empty_ast.is_none());
    }

    #[test]
    fn test_parse_expr_with_simple_binary_expression() {
        let expr_ast = parse_expression("12 + 12");
        assert!(expr_ast.is_some());
        let expr_ast = expr_ast.unwrap();
        assert_binary(&expr_ast);
        
        let bin = as_binary(&expr_ast);
        match (&*bin.left, &*bin.right) {
            (Expr::LitVal(left), Expr::LitVal(right)) => {
                matches!(left, LitValExpr { value: LitType::U8(12), .. });
                matches!(right, LitValExpr { value: LitType::U8(12), .. });
            },
            _ => unreachable!()
        }
    }
}