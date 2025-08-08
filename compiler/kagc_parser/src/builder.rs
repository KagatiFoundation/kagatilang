use std::cell::RefCell;
use std::rc::Rc;

use kagc_comp_unit::CompilationUnit;
use kagc_ctx::CompilerCtx;
use kagc_token::Token;

use crate::{Parser, SharedParserCtx};

#[derive(Default, Debug)]
pub struct ParserBuilder {
    ctx: Option<Rc<RefCell<CompilerCtx>>>,
    shared_pctx: Option<Rc<RefCell<SharedParserCtx>>>,
    tokens: Option<Rc<Vec<Token>>>,
    current_token: Option<Token>
}

impl ParserBuilder {
    pub fn new() -> Self {
        Self {
            ctx: None,
            shared_pctx: None,
            tokens: None,
            current_token: None
        }
    }

    pub fn context(mut self, ctx: Rc<RefCell<CompilerCtx>>) -> Self {
        self.ctx = Some(ctx);
        self
    }

    pub fn shared_context(mut self, ctx: Rc<RefCell<SharedParserCtx>>) -> Self {
        self.shared_pctx = Some(ctx);
        self
    }

    pub fn compile_unit(mut self, unit: &mut CompilationUnit) -> Self {
         if let Some(tokens) = &unit.tokens {
            self.tokens = Some(tokens.clone());
            self.current_token = Some(tokens[0].clone());
            self
        }
        else {
            panic!("No tokens provided!");
        }
    }

    pub fn build(self) -> Parser {
        if self.shared_pctx.is_none() {
            panic!("Shared parser context is required to build a parser!");
        }
        if self.ctx.is_none() {
            panic!("Context is required to build a parser!");
        }
        if self.tokens.is_none() {
            panic!("Tokens is required to build a parser!");
        }
        Parser::new(
            self.ctx.unwrap(), 
            self.shared_pctx.unwrap(),
            self.tokens.unwrap(),
            self.current_token.unwrap()
        )
    }
}