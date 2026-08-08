// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::cell::RefCell;

use kagc_comp_unit::source_map::{FileId, SourceMap};
use kagc_span::span::Span;
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
    pub fn from_single_token(tok: &Token, msg: &str, severity: Severity) -> Self {
        Self {
            code: None,
            message: msg.to_string(),
            severity,
            primary_span: tok.span,
            secondary_spans: vec![],
            notes: vec![]
        }
    }

	pub fn missing_token(
		tok: &Token,
		msg: &str,
		severity: Severity
	) -> Self {
		Self {
			code: None,
			message: msg.to_string(),
			severity,
			primary_span: tok.span,
			secondary_spans: vec![],
			notes: vec![]
		}
	}

    pub fn report(&self, source_map: &SourceMap) {
        let source_file = source_map.get(FileId(self.primary_span.file_id))
            .expect("File not found in pool");

        // split the file content into lines
        let source_lines: Vec<&str> = source_file.content.lines().collect();
        let line_num = self.primary_span.start.line;   // 0-based
        let col_num = self.primary_span.start.column;  // 0-based

        // let span_len = self.primary_span.end.column - self.primary_span.start.column; // length of the token

		let span_len = if self.primary_span.start.line == self.primary_span.end.line {
        self.primary_span
            .end
            .column
            .saturating_sub(self.primary_span.start.column)
            .max(1)
    } else {
        1
    };

        // print severity and message
        eprintln!("{ANSI_COLOR_RED}{:?}{ANSI_COLOR_RESET}: {}", self.severity, self.message);

        // print file path with line and column
        eprintln!(" --> {}:{}:{}", source_file.meta.abs_path.to_str().unwrap(), line_num + 1, col_num);

        // separator
        eprintln!("  |");

        // print the source line
        let source_line = source_lines.get(line_num).unwrap_or(&"");
		let expanded = Self::expand_tabs(source_line);

        eprintln!("{: >4} | {}", line_num + 1, expanded);

        // print caret repeated to match token length
		let display_col = Diagnostic::display_column(source_line, col_num);

        let caret_line = " ".repeat(display_col) + &"^".repeat(span_len.max(1));
        eprintln!("     |{}", caret_line);
    }

	fn display_column(line: &str, source_column: usize) -> usize {
    	let mut display_col = 0;

    	for (source_col, ch) in line.chars().enumerate() {
        	if source_col >= source_column {
            	break;
        	}

        	if ch == '\t' {
            	display_col += TAB_WIDTH - (display_col % TAB_WIDTH);
        	} else {
            	display_col += 1;
        	}
    	}

    	display_col
	}

	fn expand_tabs(line: &str) -> String {
		let mut out = String::new();
		let mut col = 0;

		for ch in line.chars() {
			if ch == '\t' {
				let spaces = TAB_WIDTH - (col % TAB_WIDTH);
				out.extend(std::iter::repeat_n(' ', spaces));
				col += spaces;
			} else {
				out.push(ch);
				col += 1;
			}
		}

		out
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