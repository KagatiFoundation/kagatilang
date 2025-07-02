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

use std::fmt;

use kagc_ast::ASTOperation;
use kagc_token::Token;
use kagc_types::LitTypeVariant;

#[derive(Debug)]
pub enum SAReturnTypeError {
    ExpectedNoReturnValue {
        found: LitTypeVariant
    },
    TypeMismatch {
        expected: LitTypeVariant,
        found: LitTypeVariant
    }
}

#[derive(Debug)]
pub enum SATypeError {
    AssignmentTypeMismatch {
        expected: LitTypeVariant, 
        found: LitTypeVariant
    },

    IncompatibleTypes {
        a: LitTypeVariant, 
        b: LitTypeVariant,
        operation: ASTOperation
    },

    TypeMismatch {
        expected: LitTypeVariant, 
        found: LitTypeVariant
    },

    ReturnType(SAReturnTypeError),

    NonCallable {
        sym_name: String
    }
}

#[derive(Debug)]
pub enum SAError {
    TypeError(SATypeError),

    ArrayLengthError { 
        expected: usize, 
        found: usize 
    },

    SymbolAlreadyDefined {
        sym_name: String,
        token: Token
    },

    UndefinedSymbol { 
        sym_name: String, 
        token: Token
    },

    ArgLengthMismatch {
        expected: usize,
        found: usize
    },

    None
}

impl fmt::Display for SATypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AssignmentTypeMismatch { expected, found } => {
                write!(f, "Assignment type mismatch: expected type `{}`, found `{}`.", expected, found)
            }
            Self::IncompatibleTypes { a, b, operation } => {
                write!(f, "Incompatible types `{}` and `{}` for operation `{:?}`.", a, b, operation)
            }
            Self::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: `{}` is not compatible with `{}`.", expected, found)
            },
            Self::ReturnType(ret_type_err) => {
                match ret_type_err {
                    SAReturnTypeError::ExpectedNoReturnValue { found } => {
                        write!(f, "Type mismatch: expected no return value but found `{}`.", found)
                    },
                    SAReturnTypeError::TypeMismatch { expected, found } => {
                        write!(f, "Type mismatch: expected return type `{}` but found `{}`.", expected, found)
                    }
                }
            },
            Self::NonCallable{ sym_name} => {
                write!(f, "'{}' is not a function.", sym_name)
            }
        }
    }
}

impl fmt::Display for SAError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SAError::TypeError(type_error) => write!(f, "Type Error: {type_error}"),
            SAError::ArrayLengthError { expected, found } => {
                write!(f, "Array length mismatch: expected `{expected}`, found `{found}`.")
            },
            SAError::UndefinedSymbol{ sym_name, token} => {
                panic!();
                write!(f, "{}:{}: compile error: Undefined symbol '{sym_name}'", token.pos.line, token.pos.column)
            },
            SAError::SymbolAlreadyDefined{ sym_name, token} => {
                write!(f, "{}:{}: compile error: Symbol already defined '{sym_name}'", token.pos.line, token.pos.column)
            },
            SAError::ArgLengthMismatch { expected, found } => {
                write!(f, "compile error: Argument length mismatch: expected '{}' but found '{}'", expected, found)
            },
            SAError::None => write!(f, "No error."),
        }
    }
}

impl SAError {
    pub fn dump(&self) {
        eprintln!("{self}");
        std::process::exit(1);
    }
}