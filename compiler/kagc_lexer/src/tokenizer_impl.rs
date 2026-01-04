// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;
use std::str::FromStr;

use kagc_errors::code::ErrCode;
use kagc_errors::diagnostic::{Diagnostic, DiagnosticBag, Severity};
use kagc_span::span::{SourcePos, Span};
use kagc_token::{Token, TokenKind, TokenPos};

extern crate lazy_static;
use kagc_ctx::StringInterner;
use lazy_static::lazy_static;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenKind> = {
        let mut _keys: HashMap<&'static str, TokenKind> = HashMap::new();
        _keys.insert("foreach", TokenKind::KW_FOR);
        _keys.insert("while", TokenKind::KW_WHILE);
        _keys.insert("for", TokenKind::KW_FOR);
        _keys.insert("loop", TokenKind::KW_LOOP);
        _keys.insert("integer", TokenKind::KW_INT);
        _keys.insert("float", TokenKind::KW_FLOAT);
        _keys.insert("double", TokenKind::KW_DOUBLE);
        _keys.insert("char", TokenKind::KW_CHAR,);
        _keys.insert("void", TokenKind::KW_VOID,);
        _keys.insert("const", TokenKind::KW_CONST,);
        _keys.insert("return", TokenKind::KW_RETURN,);
        _keys.insert("break", TokenKind::KW_BREAK,);
        _keys.insert("continue", TokenKind::KW_CONTINUE);
        _keys.insert("if", TokenKind::KW_IF);
        _keys.insert("else", TokenKind::KW_ELSE);
        _keys.insert("long", TokenKind::KW_LONG);
        _keys.insert("short", TokenKind::KW_SHORT);
        _keys.insert("extern", TokenKind::KW_EXTERN);
        _keys.insert("let", TokenKind::KW_LET);
        _keys.insert("def", TokenKind::KW_DEF);
        _keys.insert("string", TokenKind::KW_STR);
        _keys.insert("in", TokenKind::KW_IN);
        _keys.insert("import", TokenKind::KW_IMPORT);
        _keys.insert("null", TokenKind::KW_NULL);
        _keys.insert("record", TokenKind::KW_RECORD);
        _keys.insert("priv", TokenKind::KW_PRIV);
        _keys.insert("pub", TokenKind::KW_PUB);
        _keys
    };
}

pub struct Tokenizer<'t, 'tcx> {
    line: usize,
    curr_char: char, // current char
    next_char_pos: usize, // position from the start
    col_counter: usize, // column counter
    source: &'tcx str,
    diagnostics: &'t DiagnosticBag,
    str_interner: &'tcx StringInterner<'tcx>
}

impl<'t, 'tcx> Tokenizer<'t, 'tcx> {
    pub fn new(diags: &'t DiagnosticBag, str_interner: &'tcx StringInterner<'tcx>) -> Self {
        Self {
            line: 1,
            curr_char: ' ', // space 
            next_char_pos: 0,
            col_counter: 1,
            source: "",
            diagnostics: diags,
            str_interner
        }
    }

    pub fn tokenize(&mut self, input: &'tcx str) -> Vec<Token<'tcx>> {
        self.source = input;
        let mut tokens: Vec<Token> = Vec::new();
        self.advance_to_next_char_pos();
        loop {
            if let Some(token) = self.get_token() {
                if token.kind == TokenKind::T_EOF {
                    tokens.push(token);
                    break;
                }
                if token.kind != TokenKind::T_NONE {
                    tokens.push(token);
                } 
            }
        }
        tokens
    }

    fn get_token(&mut self) -> Option<Token<'tcx>> {
        let mut token: Token = Token::new(TokenKind::T_NONE, "", TokenPos{line: 0, column: 0});
        let col: usize = self.col_counter - 1;
        let line: usize = self.line;
        let token_pos: TokenPos = TokenPos { line, column: col };
        match self.curr_char {
            '+' => {
                token.kind = TokenKind::T_PLUS;
                self.advance_to_next_char_pos();
                match self.curr_char {
                    '+' => {
                        token.kind = TokenKind::T_INCR;
                        self.advance_to_next_char_pos();
                    },
                    '=' => {
                        token.kind = TokenKind::T_PLUSEQ;
                        self.advance_to_next_char_pos();
                    }
                    _ => {}
                }
            },
            '-' => {
                token.kind = TokenKind::T_MINUS;
                self.advance_to_next_char_pos();
                match self.curr_char {
                    '-' => {
                        token.kind = TokenKind::T_DECR;
                        self.advance_to_next_char_pos();
                    },
                    '=' => {
                        token.kind = TokenKind::T_MINUSEQ;
                        self.advance_to_next_char_pos();
                    }
                    '>' => {
                        token.kind = TokenKind::T_ARROW;
                        self.advance_to_next_char_pos();
                    }
                    _ => {}
                }
            },
            '*' => {
                token.kind = TokenKind::T_STAR;
                self.advance_to_next_char_pos();
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_STAREQ;
                    self.advance_to_next_char_pos();
                }
            },
            '/' => {
                self.advance_to_next_char_pos();
                if self.curr_char == '/' {
                    self.advance_to_next_line(); 
                    return None;
                }
                token.kind = TokenKind::T_SLASH;
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_SLASHEQ;
                    self.advance_to_next_char_pos();
                }
            },
            '!' => {
                token.kind = TokenKind::T_BANG;
                self.advance_to_next_char_pos();
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_NEQ;
                    self.advance_to_next_char_pos();
                }
            },
            '%' => {
                token.kind = TokenKind::T_PERCENT;
                self.advance_to_next_char_pos();
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_PERCENTEQ;
                    self.advance_to_next_char_pos();
                }
            },
            '^' => {
                token.kind = TokenKind::T_CARET;
                self.advance_to_next_char_pos();
                if self.curr_char == '=' {
                    token.kind = TokenKind::T_CARETEQ;
                    self.advance_to_next_char_pos();
                }
            },
            '>' => {
                token.kind = TokenKind::T_GTHAN;
                self.advance_to_next_char_pos();
                match self.curr_char {
                    '>' => {
                        token.kind = TokenKind::T_RSHIFT;
                        self.advance_to_next_char_pos();
                        if self.curr_char == '=' {
                            self.advance_to_next_char_pos();
                            token.kind = TokenKind::T_RSHIFTEQ;
                        }
                    },
                    '=' => token.kind = TokenKind::T_GTEQ,
                    _ => {},
                }
            },
            '<' => {
                token.kind = TokenKind::T_LTHAN;
                self.advance_to_next_char_pos();
                match self.curr_char {
                    '<' => {
                        token.kind = TokenKind::T_LSHIFT;
                        self.advance_to_next_char_pos();
                        if self.curr_char == '=' {
                            self.advance_to_next_char_pos();
                            token.kind = TokenKind::T_LSHIFTEQ;
                        }
                    }
                    '=' => token.kind = TokenKind::T_LTEQ,
                    _ => {}
                }
            },
            '|' => {
                token.kind = TokenKind::T_PIPE;
                self.advance_to_next_char_pos();
                match self.curr_char {
                    '|' => {
                        token.kind = TokenKind::T_OR;
                        self.advance_to_next_char_pos();
                    },
                    '=' => {
                        self.advance_to_next_char_pos();
                        token.kind = TokenKind::T_PIPEEQ;
                    },
                    _ => {}
                }
            },
            '&' => {
                token.kind = TokenKind::T_AMPERSAND;
                self.advance_to_next_char_pos();
                match self.curr_char {
                    '&' => {
                        token.kind = TokenKind::T_AND;
                        self.advance_to_next_char_pos();
                    },
                    '=' => {
                        self.advance_to_next_char_pos();
                        token.kind = TokenKind::T_AMPERSANDEQ;
                    },
                    _ => {}
                }
            },
            '~' => {
                token.kind = TokenKind::L_TILDE;
                self.advance_to_next_char_pos();
                if self.curr_char == '=' {
                    self.advance_to_next_char_pos();
                    token.kind = TokenKind::T_TILDEEQ;
                }
            },
            '=' => {
                token.kind = TokenKind::T_EQUAL;
                self.advance_to_next_char_pos();
                if self.curr_char == '=' {
                    self.advance_to_next_char_pos();
                    token.kind = TokenKind::T_EQEQ;
                }
            },
            '0'..='9' => return self.parse_number_from(token_pos), 
            '_' | 'a'..='z' | 'A'..='Z' => {
                let __start: usize = self.next_char_pos - 1;
                let mut __end: usize = __start;
                while self.curr_char.is_alphanumeric() || self.curr_char == '_' {
                    self.advance_to_next_char_pos();
                    __end += 1;
                }
                token.kind = TokenKind::T_IDENTIFIER;
                let name: &str = &self.source[__start..__end];
                let keyword: Option<&TokenKind> = KEYWORDS.get(name);
                if let Some(key) = keyword {
                    token.kind = *key;
                } 
                token.lexeme = self.str_interner.intern(name);
            },
            '"' => {
                self.advance_to_next_char_pos(); // skip '"'
                let start: usize = self.next_char_pos - 1;
                let mut end: usize = start;
                while self.curr_char != '"' && !self.is_at_end() {
                    self.advance_to_next_char_pos();
                    end += 1;
                }
                if self.is_at_end() {
                    self.advance_to_next_char_pos();
                    let diag = Diagnostic {
                        code: Some(ErrCode::SYN1001),
                        severity: Severity::Error,
                        primary_span: Span::new(
                            0, 
                            SourcePos { line: token_pos.line, column: token_pos.column }, 
                            SourcePos { line: token_pos.line, column: token_pos.column }
                        ),
                        secondary_spans: Vec::with_capacity(0),
                        message: "unterminated string".to_string(),
                        notes: Vec::with_capacity(0)
                    };
                    self.diagnostics.push(diag);
                    return None;
                }
                self.advance_to_next_char_pos();
                let str_val: &str = &self.source[start..end];
                token.kind = TokenKind::T_STRING;
                token.lexeme = self.str_interner.intern(str_val)
            },
            '(' | ')' | '{' | '}' | '[' | ']' | '#' | '.' | '?' | ':' | ',' | ';' => {
                token.kind = TokenKind::from_str(self.curr_char.to_string().as_str()).unwrap();
                self.advance_to_next_char_pos();
            },
            ' ' | '\n' | '\t' => self.advance_to_next_char_pos(),
            '\0' => token.kind = TokenKind::T_EOF,
            _ => {}
        }
        token.pos = TokenPos{ line, column: col };
        Some(token)
    }

    fn parse_number_from(&mut self, pos: TokenPos) -> Option<Token<'tcx>> {
        let mut token: Token = Token::new(TokenKind::T_NONE, "", pos);
        token.kind = TokenKind::T_INT_NUM;
        let mut __start: usize = self.next_char_pos - 1;
        let mut __end: usize = __start;
        let mut period_detected: bool = false;
        while self.curr_char.is_ascii_digit() {
            self.advance_to_next_char_pos();
            __end += 1;
        }
        if self.curr_char == '.' {
            self.advance_to_next_char_pos(); // skip '.'
            if self.curr_char.is_ascii_digit() {
                period_detected = true;
                __end += 1;
                while self.curr_char.is_ascii_digit() {
                    self.advance_to_next_char_pos();
                    __end += 1;
                }
            } else {
                let diag = Diagnostic {
                    code: Some(ErrCode::SYN1001),
                    severity: Severity::Error,
                    primary_span: Span::new(
                        0, 
                        SourcePos { line: pos.line, column: pos.column }, 
                        SourcePos { line: pos.line, column: pos.column }
                    ),
                    secondary_spans: Vec::with_capacity(0),
                    message: "invalid numeric type".to_string(),
                    notes: Vec::with_capacity(0)
                };
                self.diagnostics.push(diag);
                return None;
            }
        }
        // This check is incorrect. REWRITE THIS!!!
        let invalid_num_end: bool = self.curr_char.is_alphabetic() || self.curr_char == '_';
        if invalid_num_end {
            while self.curr_char.is_alphanumeric() || self.curr_char == '_' {
                self.advance_to_next_char_pos();
            }
            let diag = Diagnostic {
                code: Some(ErrCode::SYN1001),
                severity: Severity::Error,
                primary_span: Span::new(
                    0, 
                    SourcePos { line: pos.line, column: pos.column }, 
                    SourcePos { line: pos.line, column: pos.column }
                ),
                secondary_spans: Vec::with_capacity(0),
                message: "invalid numeric type".to_string(),
                notes: Vec::with_capacity(0)
            };
            self.diagnostics.push(diag);
            return None;
        }
        let number: &str = &self.source[__start..__end];
        if period_detected {
            token.kind = TokenKind::T_DOUBLE_NUM;
        } else {
            let _value: i64 = number.parse::<i64>().unwrap();
            token.kind = if (0..256).contains(&_value) { TokenKind::T_CHAR } 
            else if ((i32::MAX as i64)..i64::MAX).contains(&_value) { TokenKind::T_LONG_NUM }
            else { TokenKind::T_INT_NUM }
        }
        token.lexeme = self.str_interner.intern(number);
        Some(token)
    }

    fn is_at_end(&self) -> bool {
        self.next_char_pos >= self.source.len()
    }

    fn advance_to_next_char_pos(&mut self) {
        #[allow(clippy::comparison_chain)]
        if self.next_char_pos < self.source.len() {
            self.curr_char = self.source.as_bytes()[self.next_char_pos] as char;
            if self.curr_char == '\n' {
                self.line += 1;
                self.col_counter = 0;
            }
            self.next_char_pos += 1;
            self.col_counter += 1;
        }
        else {
            self.curr_char = '\0';
        }
    }

    fn advance_to_next_line(&mut self) {
        if self.next_char_pos < self.source.len() {
            while self.curr_char != '\n' {
                self.advance_to_next_char_pos();
            }
        }
        self.line += 1;
        self.col_counter = 0;
        self.advance_to_next_char_pos();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_var_decl_tokenization() {
        let a = typed_arena::Arena::<String>::new();
        let d = DiagnosticBag::default();
        let s = StringInterner::new(&a);
        let mut tok: Tokenizer = Tokenizer::new(&d, &s);
        let tokens: Vec<Token> = tok.tokenize("let a: integer = 23;");
        assert!(tokens.len() == 8);
        assert_eq!(tokens[0].kind, TokenKind::KW_LET);
        assert_eq!(tokens[1].kind, TokenKind::T_IDENTIFIER);
        assert_eq!(tokens[2].kind, TokenKind::T_COLON);
        assert_eq!(tokens[3].kind, TokenKind::KW_INT);
        assert_eq!(tokens[4].kind, TokenKind::T_EQUAL);
        assert_eq!(tokens[5].kind, TokenKind::T_CHAR);
        assert_eq!(tokens[6].kind, TokenKind::T_SEMICOLON);
        assert_eq!(tokens[7].kind, TokenKind::T_EOF);
    }
    
    #[test]
    fn test_should_report_invalid_numeric_value_error3() {
        let a = typed_arena::Arena::<String>::new();
        let d = DiagnosticBag::default();
        let s = StringInterner::new(&a);
        let mut tok: Tokenizer = Tokenizer::new(&d, &s);
        let tokens: Vec<Token> = tok.tokenize(".9999");
        assert_eq!(tokens[0].kind, TokenKind::T_DOT);
        assert_eq!(tokens[1].kind, TokenKind::T_INT_NUM);
    }
    
    #[test]
    fn test_int_var_decl_len_correct() {
        let a = typed_arena::Arena::<String>::new();
        let d = DiagnosticBag::default();
        let s = StringInterner::new(&a);
        let mut tok: Tokenizer = Tokenizer::new(&d, &s);
        let tokens: Vec<Token> = tok.tokenize("let a = 43343;");
        assert!(tokens.len() == 6);
        assert_eq!(tokens[3].lexeme.len(), 5);
    }
    
    #[test]
    fn test_float_var_decl_len_correct() {
        let a = typed_arena::Arena::<String>::new();
        let d = DiagnosticBag::default();
        let s = StringInterner::new(&a);
        let mut tok: Tokenizer = Tokenizer::new(&d, &s);
        let tokens: Vec<Token> = tok.tokenize("let a = 34.343");
        assert!(tokens.len() == 5);
        assert_eq!(tokens[3].lexeme, "34.343");
        assert_eq!(tokens[3].lexeme.len(), 6);
    }
    
    #[test]
    #[should_panic]
    fn test_float_var_decl_len_correct2() {
        let a = typed_arena::Arena::<String>::new();
        let d = DiagnosticBag::default();
        let s = StringInterner::new(&a);
        let mut tok: Tokenizer = Tokenizer::new(&d, &s);
        let tokens: Vec<Token> = tok.tokenize("let a = 3443.44ff");
        assert!(tokens.len() == 6);
    }

    #[test]
    fn test_char_ptr_var_decl_tokenization() {
        let a = typed_arena::Arena::<String>::new();
        let d = DiagnosticBag::default();
        let s = StringInterner::new(&a);
        let mut tok: Tokenizer = Tokenizer::new(&d, &s);
        let tokens: Vec<Token> = tok.tokenize("let name = \"ram\";");
        assert!(tokens.len() == 6);
        assert_eq!(tokens[0].kind, TokenKind::KW_LET);
        assert_eq!(tokens[1].kind, TokenKind::T_IDENTIFIER);
        assert_eq!(tokens[2].kind, TokenKind::T_EQUAL);
        assert_eq!(tokens[3].kind, TokenKind::T_STRING);
        assert_eq!(tokens[4].kind, TokenKind::T_SEMICOLON);
        assert_eq!(tokens[5].kind, TokenKind::T_EOF);
        assert_eq!(tokens[1].lexeme, "name"); // give identifier
        assert_eq!(tokens[3].lexeme, "ram"); // give string
    }

    #[test]
    fn test_func_decl_tokenization() {
        let a = typed_arena::Arena::<String>::new();
        let d = DiagnosticBag::default();
        let s = StringInterner::new(&a);
        let mut tok: Tokenizer = Tokenizer::new(&d, &s);
        let tokens: Vec<Token> = tok.tokenize("def main() -> void { return 0; }");
        assert!(tokens.len() == 12);
        assert_eq!(tokens[1].kind, TokenKind::T_IDENTIFIER);
        assert_eq!(tokens[1].lexeme, "main");
        assert_eq!(tokens[7].lexeme, "return");
    }

    #[test]
    fn test_empty_func_decl_tokenization() {
        let a = typed_arena::Arena::<String>::new();
        let d = DiagnosticBag::default();
        let s = StringInterner::new(&a);
        let mut tok: Tokenizer = Tokenizer::new(&d, &s);
        let tokens: Vec<Token> = tok.tokenize("def main() -> void {  }");
        assert!(tokens.len() == 9);
        assert_eq!(tokens[1].kind, TokenKind::T_IDENTIFIER);
        assert_eq!(tokens[1].lexeme, "main");
        assert_eq!(tokens[6].lexeme, "{");
        assert_eq!(tokens[7].lexeme, "}");
    }

    #[test]
    fn test_empty_source() {
        let a = typed_arena::Arena::<String>::new();
        let d = DiagnosticBag::default();
        let s = StringInterner::new(&a);
        let mut tok: Tokenizer = Tokenizer::new(&d, &s);
        let tokens: Vec<Token> = tok.tokenize("");
        assert_eq!(tokens.len(), 1); // only T_EOF is present
        assert_eq!(tokens[0].kind, TokenKind::T_EOF); // only T_EOF is present
    }

    #[test]
    fn test_only_whitespace_source() {
        let a = typed_arena::Arena::<String>::new();
        let d = DiagnosticBag::default();
        let s = StringInterner::new(&a);
        let mut tok: Tokenizer = Tokenizer::new(&d, &s);
        let tokens: Vec<Token> = tok.tokenize("            ");
        assert_eq!(tokens.len(), 1); // only T_EOF is present
        assert_eq!(tokens[0].kind, TokenKind::T_EOF); // only EOF is present
    }

    #[test]
    fn test_if_else_statement() {
        let a = typed_arena::Arena::<String>::new();
        let d = DiagnosticBag::default();
        let s = StringInterner::new(&a);
        let mut tok: Tokenizer = Tokenizer::new(&d, &s);
        let tokens: Vec<Token> = tok.tokenize("if (4 > 5) { } else { }");
        assert_eq!(tokens.len(), 12); // including T_EOF
        assert_eq!(tokens[0].kind, TokenKind::KW_IF);
        assert_eq!(tokens[8].kind, TokenKind::KW_ELSE);
    }
}