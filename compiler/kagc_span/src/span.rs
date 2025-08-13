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
