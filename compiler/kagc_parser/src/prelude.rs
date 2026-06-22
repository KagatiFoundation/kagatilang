// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_ast::{Expr, Stmt};
use kagc_errors::diagnostic::DiagnosticBag;
use kagc_token::Token;

use crate::Parser;
use crate::options::ParserOptions;

pub fn parse_expr<'tcx>(
    options: ParserOptions,
    diagnostics: &'tcx DiagnosticBag, 
    tokens: Vec<Token<'tcx>>,
) -> Option<Expr<'tcx>> {
    let mut parser = Parser::new(
        options, 
        diagnostics, 
        tokens
    );
    
    parser.parse_expression()
}

pub fn parse_stmt<'tcx>(
    options: ParserOptions,
    diagnostics: &'tcx DiagnosticBag, 
    tokens: Vec<Token<'tcx>>,
) -> Option<Stmt<'tcx>> {
    let mut parser = Parser::new(
        options, 
        diagnostics, 
        tokens
    );
    
    parser.parse_statement()
}