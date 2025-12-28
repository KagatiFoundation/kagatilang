// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_ast::{AST, Expr, Stmt};
use kagc_errors::diagnostic::Diagnostic;
use kagc_lexer::Tokenizer;

use crate::{Parser, session::ParserSession};

/// A type alias representing the result of parsing, which can either
/// be an AST node on successful parsing or a ParseError indicating a
/// parsing failure.
pub type ParseResult = Result<AST, Box<Diagnostic>>;

pub type ParseOutput = Option<AST>;

pub fn parse_expression(source: &str) -> Option<Expr> {
    let mut session = ParserSession::from_string(source);
    let lexer = Tokenizer::new();
    let mut parser = Parser::new(&mut session, lexer);
    parser.parse_expression()
}

pub fn parse_statement(source: &str) -> Option<Stmt> {
    let mut session = ParserSession::from_string(source);
    let lexer = Tokenizer::new();
    let mut parser = Parser::new(&mut session, lexer);
    let stmt = parser.parse_statement();
    if session.has_errors() {
        session.dump_diagnostics();
    }
    stmt
}

#[cfg(test)]
mod parser_prelude_tests {
    use kagc_ast::{Expr, LitValExpr, Stmt};
    use kagc_types::LitType;

    use crate::prelude::{parse_expression, parse_statement};

    #[test]
    fn test_parse_empty_input() {
        let empty_ast = parse_expression("");
        assert!(empty_ast.is_none());
        let empty_ast = parse_expression("    ");
        assert!(empty_ast.is_none());
        let empty_ast = parse_expression("\n\t");
        assert!(empty_ast.is_none());
        let empty_ast = parse_expression("\t\n");
        assert!(empty_ast.is_none());
        let empty_ast = parse_expression("\t\t\t\t\t\t\t\t\t\t\t\t\t\t");
        assert!(empty_ast.is_none());
        let empty_ast = parse_expression("\n\n\n\n\n\n\n\n");
        assert!(empty_ast.is_none());
    }

    #[test]
    fn test_parse_expr_with_simple_binary_expression() {
        let expr_ast = parse_expression("12 + 12");
        assert!(expr_ast.is_some(), "couldn't parse an expression");
        let expr_ast = expr_ast.unwrap();
        
        let bin = expr_ast.as_binary();
        assert!(bin.is_some(), "expr is not binary");
        let bin = bin.unwrap();
        match (&*bin.left, &*bin.right) {
            (Expr::LitVal(left), Expr::LitVal(right)) => {
                assert!(matches!(left, LitValExpr { value: LitType::U8(12), .. }));
                assert!(matches!(right, LitValExpr { value: LitType::U8(12), .. }));
            },
            _ => unreachable!()
        }
    }

    #[test]
    fn test_parse_empty_for_loop_statement() {
        let ast = parse_statement("for a in 12 { }");
        assert!(ast.is_some());
        let for_stmt = ast.unwrap();
        assert!(matches!(for_stmt, Stmt::For));
    }

    #[test]
    fn test_block_stmt_parsing() {
        let Some(block_ast) = parse_statement("{ let a = 12; }") else {
            panic!("cannot parse");
        };
        let Stmt::Block(block_stmt) = block_ast else {
            panic!("expected block stmt, found {:#?}", block_ast);
        };
        assert!(block_stmt.statements.len() == 1);
    }

    #[test]
    fn test_nested_block_stmt_parsing() {
        let Some(block_ast) = parse_statement("{{ let a = 12 + 12; }}") else {
            panic!("cannot parse");
        };
        let Stmt::Block(block_stmt) = block_ast else {
            panic!("expected block stmt, found {:#?}", block_ast);
        };
        assert!(block_stmt.statements.len() == 1);

        let Some(nested_block_ast) = block_stmt.statements.first() else {
            panic!("nested block not found");
        };
        let Some(nested_block_stmt) = nested_block_ast.as_stmt() else {
            panic!("expected a nested block stmt, found {:#?}", nested_block_ast);
        };
        let Stmt::Block(block) = nested_block_stmt else {
            panic!("expected a statement inside the nested block stmt, found {:#?}", nested_block_stmt);
        };
        assert!(block.statements.len() == 1);
    }
}