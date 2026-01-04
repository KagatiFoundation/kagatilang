// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_ast::Expr;
use kagc_errors::diagnostic::DiagnosticBag;
use kagc_lexer::Tokenizer;
use kagc_ctx::StringInterner;

use crate::Parser;
use crate::options::ParserOptions;

pub fn parse_expression<'tcx>(
    source: &'tcx str, 
    diagnostics: &'tcx DiagnosticBag, 
    str_interner: &'tcx StringInterner<'tcx>,
) -> Option<Expr<'tcx>> {
    let mut lexer = Tokenizer::new(
        diagnostics, 
        str_interner
    );
    let tokens = lexer.tokenize(source);

    let mut parser = Parser::new(
        ParserOptions { }, 
        diagnostics, 
        tokens
    );
    let expr = parser.parse_expression();

    if diagnostics.has_errors() {
        panic!("{:#?}", diagnostics);
    }
    expr
}

pub fn parse_statement<'tcx>(
    source: &'tcx str, 
    diagnostics: &'tcx DiagnosticBag, 
    str_interner: &'tcx StringInterner<'tcx>,
) -> Option<Expr<'tcx>> {
    let mut lexer = Tokenizer::new(
        diagnostics, 
        str_interner
    );
    let tokens = lexer.tokenize(source);

    let mut parser = Parser::new(
        ParserOptions { }, 
        diagnostics, 
        tokens
    );
    let expr = parser.parse_expression();

    if diagnostics.has_errors() {
        panic!("{:#?}", diagnostics);
    }
    expr
}