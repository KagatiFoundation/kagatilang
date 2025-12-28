// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::RefCell;
use std::rc::Rc;

use kagc_comp_unit::source_map::{FileId, SourceMap};
use kagc_comp_unit::source::SourceFile;
use kagc_errors::diagnostic::DiagnosticBag;
use kagc_scope::ctx::ScopeCtx;
use kagc_scope::ctx::builder::ScopeCtxBuilder;

use crate::options::ParserOptions;

#[derive(Debug, Clone)]
pub struct ParserSession {
    pub file_id: FileId,
    pub diagnostics: DiagnosticBag,
    pub options: ParserOptions,
    pub files: Rc<RefCell<SourceMap>>,
    pub scope: Rc<RefCell<ScopeCtx>>
}

impl ParserSession {
    pub fn from_string(source: &str) -> Self {
        let mut sources = SourceMap::default();
        let file_id = sources.insert(SourceFile::from_string("test", source)).unwrap();
        Self {
            file_id,
            diagnostics: DiagnosticBag::default(),
            options: ParserOptions {  },
            files: Rc::new(RefCell::new(sources)),
            scope: Rc::new(RefCell::new(ScopeCtxBuilder::new().build()))
        }
    }

    pub fn from_file(
        path: &str, 
        scope: Rc<RefCell<ScopeCtx>>,
        files: Rc<RefCell<SourceMap>>
    ) -> std::io::Result<Self> {
        let file = SourceFile::from_file(path)?;
        Ok(Self::from_source_file(file, scope, files))
    }

    pub fn from_source_file(
        file: SourceFile, 
        scope: Rc<RefCell<ScopeCtx>>,
        files: Rc<RefCell<SourceMap>>
    ) -> Self {
        let file_id = files.borrow_mut().insert(file).unwrap();
        Self {
            file_id,
            diagnostics: DiagnosticBag::default(),
            options: ParserOptions {  },
            files,
            scope
        }
    }

    pub(crate) fn dump_diagnostics(&self) {
        self.diagnostics.report_all(&self.files.borrow());
    }

    pub(crate) fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
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