// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::RefCell;

use kagc_comp_unit::source_map::{FileId, SourceMap};
use kagc_span::span::{SourcePos, Span};
use kagc_token::Token;

use crate::code::ErrCode;
use crate::terminal::*;

#[derive(Debug, Clone)]
pub enum Severity {
    Error,
    Warning,
    Help,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub code:               Option<ErrCode>,
    pub severity:           Severity,
    pub primary_span:       Span,
    pub secondary_spans:    Vec<(Span, String)>,
    pub message:            String,
    pub notes:              Vec<String>,
}

impl Diagnostic {
    pub fn from_single_token(tok: &Token, file: FileId, msg: &str, severity: Severity) -> Self {
        let start_pos = tok.pos;
        let lexeme = tok.lexeme;
        let total_span = Span::new(
            file.0, 
            SourcePos {
                line: start_pos.line,
                column: start_pos.column
            },
            SourcePos {
                line: start_pos.line,
                column: start_pos.column + lexeme.len()
            }
        );
        Self {
            code: None,
            message: msg.to_string(),
            severity,
            primary_span: total_span,
            secondary_spans: vec![],
            notes: vec![]
        }
    }

    pub fn report(&self, source_map: &SourceMap) {
        let source_file = source_map.get(FileId(self.primary_span.file_id))
            .expect("File not found in pool");

        // split the file content into lines
        let source_lines: Vec<&str> = source_file.content.lines().collect();
        let line_num = self.primary_span.start.line;   // 1-based
        let col_num = self.primary_span.start.column;  // 0-based
        let span_len = self.primary_span.end.column - col_num; // length of the token

        // print severity and message
        eprintln!("{ANSI_COLOR_RED}{:?}{ANSI_COLOR_RESET}: {}", self.severity, self.message);

        // print file path with line and column
        eprintln!(" --> {}:{}:{}", source_file.meta.abs_path.to_str().unwrap(), line_num, col_num + 1);

        // separator
        eprintln!("  |");

        // print the source line
        let source_line = source_lines.get(line_num - 1).unwrap_or(&"");
        eprintln!("{: >4} | {}", line_num, source_line);

        // print caret repeated to match token length
        let caret_line = " ".repeat(col_num) + &"^".repeat(span_len.max(1));
        eprintln!("     |{}", caret_line);
    }
}

#[derive(Debug, Default, Clone)]
pub struct DiagnosticBag {
    diagnostics: RefCell<Vec<Diagnostic>>,
}

impl DiagnosticBag {
    pub fn push(&self, diag: Diagnostic) {
        self.diagnostics.borrow_mut().push(diag);
    }

    pub fn extend(&self, other: DiagnosticBag) {
        self.diagnostics.borrow_mut().extend(other.diagnostics.borrow().clone());
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.borrow().iter().any(|d| matches!(d.severity, Severity::Error))
    }

    pub fn report_all(&self, source_map: &SourceMap) {
        for diag in self.diagnostics.borrow().iter() {
            diag.report(source_map);
        }
    }

    pub fn clear(&self) {
        self.diagnostics.borrow_mut().clear();
    }
}