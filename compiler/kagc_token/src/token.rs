// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

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
    pub pos: TokenPos,
}

impl<'tcx> Token<'tcx> {
    pub fn new(kind: TokenKind, lexeme: &'tcx str, pos: TokenPos) -> Token<'tcx> {
        Token { kind, lexeme, pos }
    }

    // to mark something as erronous token
    pub fn none() -> Token<'static> {
        Token {
            kind: TokenKind::T_NONE,
            lexeme: "",
            pos: TokenPos { line: 0, column: 0 }
        }
    }
}