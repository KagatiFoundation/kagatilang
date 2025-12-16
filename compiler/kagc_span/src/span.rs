// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourcePos {
    pub line: usize,
    pub column: usize,
}

/// Represents a continuous range in a source file.
/// Both start and end are inclusive positions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// File identifier 
    pub file_id: usize,

    /// Starting position (line and column).
    pub start: SourcePos,

    /// Ending position (line and column).
    pub end: SourcePos,
}

impl Span {
    pub fn new(file: usize, start: SourcePos, end: SourcePos) -> Self {
        Self {
            file_id: file,
            start,
            end
        }
    }
}

/// A trait for types that provide access to a source code span.
///
/// Implementors of this trait can return a reference to their associated `Span`,
/// which represents the location in the source code corresponding to the item.
pub trait HasSpan {
    fn span(&self) -> &Span;
}
