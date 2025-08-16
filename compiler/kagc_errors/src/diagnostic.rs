/*
MIT License

Copyright (c) 2023 Kagati Foundation

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

use kagc_comp_unit::{
    file_pool::{
        FilePool, 
        FilePoolIdx
    }, 
    CompilationUnit
};
use kagc_span::span::{SourcePos, Span};
use kagc_token::Token;

#[derive(Debug)]
pub enum Severity {
    Error,
    Warning,
    Help,
}

#[derive(Debug)]
pub struct Diagnostic {
    pub code: Option<String>,
    pub severity: Severity,
    pub primary_span: Span,
    pub secondary_spans: Vec<(Span, String)>,
    pub message: String,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn from_single_token(tok: &Token, file: FilePoolIdx, msg: &str, severity: Severity) -> Self {
        let start_pos = tok.pos;
        let lexeme = tok.lexeme.clone();
        let total_span = Span::new(
            file, 
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

    pub fn report(&self, file_pool: &FilePool, unit: &CompilationUnit) {
        let file_meta = file_pool.get(self.primary_span.file_id)
            .expect("File not found in pool");

        let source_lines: Vec<&str> = unit.source.content.lines().collect();
        let line_num = self.primary_span.start.line;
        let col_num = self.primary_span.start.column;

        eprintln!("{:#?}: {}", self.severity, self.message);
        eprintln!(
            " --> {}:{}:{}",
            file_meta.abs_path,
            line_num + 1,
            col_num + 1
        );
        eprintln!("  |");
        eprintln!("{: >3} | {}", line_num + 1, source_lines[line_num]);
        eprintln!("    | {:>width$}^", "", width = col_num);
    }
}

#[derive(Debug, Default)]
pub struct DiagnosticBag {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticBag {
    pub fn push(&mut self, diag: Diagnostic) {
        self.diagnostics.push(diag);
    }

    pub fn extend(&mut self, other: DiagnosticBag) {
        self.diagnostics.extend(other.diagnostics);
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| matches!(d.severity, Severity::Error))
    }

    pub fn report_all(&self, file_pool: &FilePool, unit: &CompilationUnit) {
        for diag in &self.diagnostics {
            diag.report(file_pool, unit);
        }
    }

    pub fn clear(&mut self) {
        self.diagnostics.clear();
    }
}