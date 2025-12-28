// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_lexer::Tokenizer;

use crate::{Parser, session::ParserSession};

#[derive(Default, Debug)]
pub struct ParserBuilder<'pb> {
    session: Option<&'pb mut ParserSession>,
    lexer: Option<Tokenizer>
}

impl<'pb> ParserBuilder<'pb> {
    pub fn new() -> Self {
        Self {
            session: None,
            lexer: None
        }
    }

    pub fn session(mut self, sess: &'pb mut ParserSession) -> Self {
        self.session = Some(sess);
        self
    }

    pub fn lexer(mut self, lexer: Tokenizer) -> Self {
        self.lexer = Some(lexer);
        self
    }

    pub fn build(self) -> Parser<'pb> {
        if self.lexer.is_none() {
            panic!("Lexer is required to build a parser!");
        }
        if self.session.is_none() {
            panic!("Session is required to build a parser!");
        }
        Parser::new(self.session.unwrap(), self.lexer.unwrap())
    }
}