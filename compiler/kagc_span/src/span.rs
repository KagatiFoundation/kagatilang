// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourcePos {
    pub line: usize,
    pub column: usize,
	pub offset: usize,
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
	pub fn uninit() -> Self {
		Self {
			file_id: 0xFFFFFFFF,
			start: SourcePos { line: 0, column: 0, offset: 0 },
			end: SourcePos { line: 0, column: 0, offset: 0 }
		}
	}

    pub fn new(file: usize, start: SourcePos, end: SourcePos) -> Self {
        Self {
            file_id: file,
            start,
            end
        }
    }

	pub fn covering(&self, other: &Span) -> Self {
		assert_eq!(self.file_id, other.file_id);

		Self {
			file_id: self.file_id,
			start: self.start,
			end: other.end
		}
	}

	pub fn source<'a>(&self, source: &'a str) -> &'a str {
		&source[self.start.offset..self.end.offset]
	}
}

/// A trait for types that provide access to a source code span.
///
/// Implementors of this trait can return a reference to their associated `Span`,
/// which represents the location in the source code corresponding to the item.
pub trait HasSpan {
    fn span(&self) -> &Span;
}
