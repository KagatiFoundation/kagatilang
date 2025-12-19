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
    let asts = parser.parse();
    Ok(asts[0].clone())
}

#[cfg(test)]
mod parser_prelude_tests {
    use crate::prelude::parse_single_statement;

    #[test]
    fn test_length_of_parse_single_statement() {
        let asts = parse_single_statement("let a = 12;");
        assert!(asts.is_ok());
        println!("{:#?}", asts.ok().unwrap());
    }
}