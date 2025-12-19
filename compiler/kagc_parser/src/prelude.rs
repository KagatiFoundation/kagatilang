// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_ast::AST;
use kagc_errors::diagnostic::Diagnostic;
use kagc_lexer::Tokenizer;

use crate::{Parser, session::ParserSession};

/// A type alias representing the result of parsing, which can either
/// be an AST node on successful parsing or a ParseError indicating a
/// parsing failure.
pub type ParseResult = Result<AST, Box<Diagnostic>>;

pub fn parse_single_statement(source: &str) -> ParseResult {
    let session = ParserSession::from_string(source);
    let lexer = Tokenizer::new();
    let mut parser = Parser::new(session, lexer);
    parser.parse_single_stmt()
}

pub fn parse_expression(source: &str) -> ParseResult {
    let session = ParserSession::from_string(source);
    let lexer = Tokenizer::new();
    let mut parser = Parser::new(session, lexer);
    parser.parse_expression()
}

#[cfg(test)]
mod parser_prelude_tests {
    use crate::prelude::parse_expression;

    #[test]
    fn parse_expr_with() {
        let asts = parse_expression("12 + 12");
        assert!(asts.is_ok());
    }
}