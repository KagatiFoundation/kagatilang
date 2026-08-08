// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_span::span::{SourcePos, Span};

use super::TokenKind;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct TokenPos {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Token<'tcx> {
    pub kind: TokenKind,
    pub lexeme: &'tcx str,
	pub span: Span
}

impl<'tcx> Token<'tcx> {
    pub fn new(kind: TokenKind, lexeme: &'tcx str, span: Span) -> Token<'tcx> {
        Token { kind, lexeme, span }
    }

    // to mark something as erronous token
    pub fn uninit() -> Token<'static> {
        Token {
            kind: TokenKind::T_NONE,
            lexeme: "",
            span: Span::new(
				0,
				SourcePos {
					line: 0, column: 0, offset: 0
				},
				SourcePos { 
					line: 0, column: 0, offset: 0
				}
			)
        }
    }
}