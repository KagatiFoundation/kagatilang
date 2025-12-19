// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::RefCell;
use std::rc::Rc;

use kagc_comp_unit::source::SourceFile;
use kagc_errors::diagnostic::DiagnosticBag;
use kagc_scope::ctx::ScopeCtx;
use kagc_scope::ctx::builder::ScopeCtxBuilder;

use crate::options::ParserOptions;
use crate::source_map::{FileId, SourceMap};

#[derive(Debug, Clone)]
pub struct ParserSession {
    pub file_id: FileId,
    pub diagnostics: DiagnosticBag,
    pub options: ParserOptions,
    pub sources: Rc<RefCell<SourceMap>>,
    pub scope: Rc<RefCell<ScopeCtx>>
}

impl ParserSession {
    pub fn from_string(source: &str) -> Self {
        let mut sources = SourceMap::default();
        let id = sources.add_virtual_file("test", source);
        Self {
            file_id: id,
            diagnostics: DiagnosticBag::default(),
            options: ParserOptions {  },
            sources: Rc::new(RefCell::new(sources)),
            scope: Rc::new(RefCell::new(ScopeCtxBuilder::new().build()))
        }
    }

    pub fn from_file(path: &str, scope: Rc<RefCell<ScopeCtx>>) -> std::io::Result<Self> {
        let mut sources = SourceMap::default();
        let file = SourceFile::from_file(path)?;
        let file_id = sources.add_file(file);
        Ok(Self {
            file_id,
            diagnostics: DiagnosticBag::default(),
            options: ParserOptions { },
            sources: Rc::new(RefCell::new(sources)),
            scope
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::session::ParserSession;

    #[test]
    fn test_session_creation() {
        let a = ParserSession::from_string("let a = 12;");
        println!("{a:#?}");
    }
}