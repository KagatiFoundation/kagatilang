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

use kagc_token::{Token, TokenKind, TokenPos};

/// Trait representing a basic error in the compiler. Any struct 
/// implementing this trait should provide a mechanism to report 
/// the error.
pub trait BError {
    /// Reports the error by printing it to the console.
    fn report(&self);
}

/// Struct containing common information about an error in the 
/// compiler. This includes the token that caused the error, a 
/// static message describing the error, and a reference to the 
/// source file where the error occurred.
#[derive(Clone, Debug)]
pub struct BErrorInfo {
    /// The token that caused the error.
    pub token: Token,

    /// A static message describing the error.
    pub message: String,

    /// Name of the source file where error has occured.
    pub source_file: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BTypeErr {
    TypesMismatch { expected: String, found: String },
    AssignmentTypeMismatch { var_type: String, assigned_type: String },
    ReturnTypeMismatch { expected: String, found: String },
    IncompatibleTypes { first_type: String, second_type: String, operator: String },
    InitializerNotAConstant { lexeme: String }
}

#[derive(PartialEq, Clone, Debug)]
pub enum BErrType {
    UndefinedSymbol,
    UnexpectedToken {
        expected: Vec<TokenKind>,
        found: Token
    },
    NonSubscriptable,
    NonCallable,
    SymbolAlreadyDefined,
    MissingReturnType,
    InvalidReturnType,
    InvalidRecordFieldType,
    TypeError(BTypeErr),
    None
}

#[derive(Clone, Debug)]
pub struct BErr {
    err_type: BErrType,
    info: Option<BErrorInfo>,
}

impl BErr {
    pub fn new(err_type: BErrType, source_file: String, err_token: Token) -> Self {
        if err_type == BErrType::None {
            return BErr {
                err_type: BErrType::None,
                info: None
            };
        }
        let message: String = match err_type {
            BErrType::UndefinedSymbol => "Undefined symbol".to_string(),
            
            BErrType::NonSubscriptable => "Identifier is not subscriptable".to_string(),
            
            BErrType::NonCallable => "Identifier is not callable".to_string(),
            
            BErrType::SymbolAlreadyDefined => "Symbol already defined".to_string(),
            
            BErrType::MissingReturnType => "Missing return type".to_string(),
            
            BErrType::InvalidReturnType => "Invalid return type for a function".to_string(),

            BErrType::InvalidRecordFieldType => "Invalid type for record field".to_string(),

            BErrType::TypeError(ref type_error) => match type_error {
                BTypeErr::TypesMismatch { expected, found } => format!("Type mismatch: expected {}, found {}", expected, found),
                BTypeErr::AssignmentTypeMismatch { var_type, assigned_type } => format!("Cannot assign a value of type '{}' to a variable of type '{}'", assigned_type, var_type),
                BTypeErr::ReturnTypeMismatch { expected, found } => format!("Return type mismatch: expected {}, found {}", expected, found),
                BTypeErr::IncompatibleTypes { first_type, second_type, operator } => format!("Incompatible types '{}' and '{}' for operator '{}'", first_type, second_type, operator),
                BTypeErr::InitializerNotAConstant { lexeme } => format!("Initializer is not a constant '{}'", lexeme)
            },
            _ => "".to_string()
        };
        BErr {
            err_type,
            info: Some(BErrorInfo {
                message,
                source_file,
                token: err_token,
            }),
        }
    }

    pub fn none() -> Self {
        BErr {
            err_type: BErrType::None,
            info: None
        }
    }

    pub fn undefined_symbol(source_file: String, err_token: Token) -> BErr {
        BErr::new(BErrType::UndefinedSymbol, source_file, err_token)
    }

    pub fn unexpected_token(source_file: String, expected: Vec<TokenKind>, found: Token) -> BErr {
        let mut expect_token_msg: String = String::from("");
        if expected.len() > 1 {
            expect_token_msg.push_str(" one of ");
        }
        for expect in &expected {
            expect_token_msg.push_str(&format!("'{}'", expect.as_str()));
        }

        BErr {
            err_type: BErrType::UnexpectedToken { 
                expected: expected.clone(), 
                found: found.clone() 
            },
            info: Some(
                BErrorInfo { 
                    token: found.clone(), 
                    message: format!("Unexpected token: expected {} but found '{}'", expect_token_msg, found.kind.as_str()), 
                    source_file
                }
            )
        }
    }

    pub fn nonsubsriptable_ident(source_file: String, err_token: Token) -> BErr {
        BErr::new(BErrType::NonSubscriptable, source_file, err_token)
    }

    pub fn noncallable_ident(source_file: String, err_token: Token) -> BErr {
        BErr::new(BErrType::NonCallable, source_file, err_token)
    }
    
    pub fn symbol_already_defined(source_file: String, err_token: Token) -> BErr {
        BErr::new(BErrType::SymbolAlreadyDefined, source_file, err_token)
    }

    pub fn is_ignorable(&self) -> bool {
        matches!(self.err_type, BErrType::None)
    }

    pub fn report(&self) {
        if let Some(info) = &self.info {
            println!(
                "{}:{}:{}: compile error: {}",
                info.source_file,
                info.token.pos.line,
                info.token.pos.column,
                info.message
            );
        }
    }
    
    pub fn fatal(&self) {
        if self.info.is_some() {
            self.report();
            std::process::exit(1);
        }
    }
}

pub fn bichara_error(msg: &str) {
    println!("error: {}", msg);
    std::process::exit(1);
}

pub fn error(pos: TokenPos, msg: &str) {
    panic!("Error: {}:{} {}", pos.line, pos.column, msg);
}

pub fn report_errornous_token(tok: &Token, msg: &str) {
    println!("Error: {}:{} {}", tok.pos.line, tok.pos.column, msg);
}

pub fn report_errornous_token_and_exit(tok: &Token, msg: &str, code: i32) {
    report_errornous_token(tok, msg);
    std::process::exit(code);
}

pub fn warning(pos: TokenPos, msg: &str) {
    panic!("warning: {}:{} {}", pos.line, pos.column, msg);
}

pub fn report_unexpected_token(token: &Token, hint: Option<&str>) {
    panic!(
        "Error: unexpected token '{}' at {}:{}. {}",
        token.lexeme,
        token.pos.line,
        token.pos.column,
        hint.unwrap_or_default()
    );
}