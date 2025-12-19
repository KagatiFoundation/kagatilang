// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use core::panic;
use std::collections::HashMap;
use std::rc::Rc;

use kagc_ast::record::*;
use kagc_ast::*;
use kagc_comp_unit::file_pool::FilePoolIdx;
use kagc_errors::diagnostic::Diagnostic;
use kagc_errors::diagnostic::Severity;
use kagc_lexer::Tokenizer;
use kagc_scope::scope::ScopeType;
use kagc_span::span::SourcePos;
use kagc_span::span::Span;
use kagc_symbol::{function::*, *};
use kagc_token::*;
use kagc_types::record::RecordFieldType;
use kagc_types::LitType;
use kagc_types::LitTypeVariant;

use crate::prelude::ParseResult;
use crate::session::ParserSession;

type TokenMatch<'a> = Result<&'a Token, Box<Diagnostic>>;

/// Represents an invalid function ID.
///
/// This constant is used to indicate that a function ID is not valid or
/// not set, serving as a sentinel value during parsing and code generation
/// to detect error states and invalid contexts.
const INVALID_FUNC_ID: usize = 0xFFFFFFFF;

pub type StringLabel = usize;

/// Represents a parser for converting tokens into an
/// abstract syntax tree (AST).
#[derive(Debug, Clone)]
pub struct Parser {
    /// Tokens that are going to be parsed.
    tokens: Rc<Vec<Token>>,

    /// Counter which points to the current token index.
    current: usize,

    /// Current token being parsed. (```current_token = self.tokens[self.current]```)
    current_token: Token,

    /// ID of a function that is presently being parsed. This field's
    /// value is ```INVALID_FUNC_ID``` if the parser is not inside a
    /// function.
    current_function_id: usize,

    /// Name of the function that is currently being parsed.
    current_function_name: Option<String>,

    /// Position of next local symbol.
    next_local_sym_pos: usize,

    // shared_pctx: Rc<RefCell<SharedParserCtx>>,

    /// Label generator that is going to be used by string literals only.
    _str_label_: usize,

    var_offsets: HashMap<String, usize>,

    current_file: FilePoolIdx,

    sess: ParserSession,

    lexer: Tokenizer
}

#[cfg(test)]
mod tests {
    use kagc_lexer::Tokenizer;

    use crate::{Parser, session::ParserSession};

    #[test]
    fn test_sth() {
        let s = ParserSession::from_string("let a = 12;");
        let l = Tokenizer::new();
        let mut p = Parser::new(s, l);
        let tokens = p.tokenize_input_stream();
        println!("{tokens:#?}");
    }
}

impl Parser {
    /// Internal parser constructor.
    /// Use `ParserBuilder` instead.
    pub fn new(sess: ParserSession, lexer: Tokenizer) -> Self {
        Self {
            tokens: Rc::new(vec![]),
            current: 0,
            current_token: Token::none(),
            current_function_id: INVALID_FUNC_ID,
            current_function_name: None,
            next_local_sym_pos: 0,
            _str_label_: 0,
            var_offsets: HashMap::new(),
            current_file: sess.file_id.0,
            sess,
            lexer
        }
    }

    pub fn tokenize_input_stream(&mut self) -> Rc<Vec<Token>> {
        let current_file_id = self.sess.file_id;
        let files = self.sess.sources.borrow();
        if let Some(source_file) = files.files.get(&current_file_id) {
            let tokens = self.lexer.tokenize(source_file.content.clone());
            self.tokens = Rc::new(tokens);
            let first_token = self.tokens[0].clone();
            self.current_token = first_token;
            self.current_function_id = current_file_id.0;
            self.tokens.clone()
        }
        else {
            panic!()
        }
    }

    pub fn parse(&mut self) -> Vec<AST> {
        if self.tokens.is_empty() {
            self.tokenize_input_stream();
        }

        let mut nodes: Vec<AST> = vec![];
        loop {
            if self.current_token.kind == TokenKind::T_EOF {
                break;
            }
            let stmt_parse_result: ParseResult = self.parse_single_stmt();
            if let Ok(stmt) = stmt_parse_result {
                nodes.push(stmt);
            } 
            else if let Err(parse_error) = stmt_parse_result {
                self
                    .sess
                    .diagnostics
                    .push(*parse_error);
                break;
            }
        }
        nodes
    }

    /// Parses a single statement based on the current token.
    ///
    /// Delegates parsing to specific functions depending on the token kind:
    /// - Handles variable declarations (global/local), assignments, control
    ///   flow statements (if, while, for), function definitions, and return
    ///   statements.
    ///
    /// - If the token is a compound statement, parses it recursively.
    ///
    /// - If the token is not recognized, attempts to parse an expression followed
    ///   by a semicolon.
    ///
    /// - Returns a `ParseResult` representing the parsed statement or an error
    ///   if parsing fails.
    fn parse_single_stmt(&mut self) -> ParseResult {
        let curr_tok_kind: TokenKind = self.current_token.kind;
        let result = match curr_tok_kind {
            TokenKind::KW_DEF => self.parse_function_stmt(),

            TokenKind::KW_LET => self.parse_var_decl_stmt(),

            TokenKind::T_IDENTIFIER => self.assign_stmt_or_func_call(),
            
            TokenKind::KW_IF => self.parse_if_stmt(),
            
            TokenKind::KW_WHILE => self.parse_while_stmt(),
            
            TokenKind::KW_FOR => self.parse_for_stmt(),
            
            TokenKind::T_LBRACE => self.parse_compound_stmt(),
            
            TokenKind::KW_RETURN => self.parse_return_stmt(),
            
            TokenKind::KW_LOOP => self.parse_loop_stmt(),
            
            TokenKind::KW_BREAK => self.parse_break_stmt(),

            TokenKind::KW_IMPORT => self.parse_import_stmt(),

            TokenKind::KW_RECORD => self.parse_record_decl_stmt(),
            
            _ => {
                let diag = Diagnostic::from_single_token(
                    &self.current_token, 
                    self.current_file, 
                    "unexpected token", 
                    Severity::Error
                );
                Err(Box::new(diag))
            }
        };

        if result.is_ok() {
            match curr_tok_kind {
                TokenKind::KW_LET
                | TokenKind::KW_RETURN
                | TokenKind::KW_BREAK
                | TokenKind::T_IDENTIFIER => {
                    _ = self.token_match(TokenKind::T_SEMICOLON)?;
                },
                _ => ()
            }
        }
        result
    }

    // parse compound statement(statement starting with '{' and ending with '}')
    fn parse_compound_stmt(&mut self) -> ParseResult {
        _ = self.token_match(TokenKind::T_LBRACE)?;
        let mut left: Option<AST> = None;
        let mut stmt_count: i32 = 0;
        loop {
            if self.current_token.kind == TokenKind::T_RBRACE {
                _ = self.token_match(TokenKind::T_RBRACE)?; // match and ignore '}'
                break;
            }
            let tree_result: ParseResult = self.parse_single_stmt();
            if let Err(parse_error) = tree_result {
                return Err(parse_error);
            } 
            else if let Ok(parse_res) = tree_result {
                if left.is_none() {
                    left = Some(parse_res);
                } else {
                    left = Some(AST::new(
                        ASTKind::StmtAST(Stmt::Glue),
                        ASTOperation::AST_GLUE,
                        left.clone(),
                        Some(parse_res),
                        LitTypeVariant::None,
                    ));
                }
            }
            // increment the statement count only when we succesffully parse a statement
            stmt_count += 1;
        }
        if stmt_count == 0 {
            return Ok(AST::empty());
        } 
        Ok(left.unwrap())
    }

    fn parse_import_stmt(&mut self) -> ParseResult {
        let start_tok = self.token_match(TokenKind::KW_IMPORT)?.pos; // match 'import' keyword
        let module_path_tok: Token = self.token_match(TokenKind::T_STRING)?.clone();
        let end_tok = self.token_match(TokenKind::T_SEMICOLON)?.pos; // match ';'

        let meta = NodeMeta::new(
            Span::new(
                self.current_file,
                SourcePos { 
                    line: start_tok.line, 
                    column: start_tok.column 
                },
                SourcePos { 
                    line: end_tok.line, 
                    column: end_tok.column 
                }
            ),
            vec![]
        );
        Ok(
            AST::create_leaf(
                ASTKind::StmtAST(
                    Stmt::Import(
                        ImportStmt { 
                            path: module_path_tok.lexeme.clone() 
                        }
                    )
                ), 
                ASTOperation::AST_IMPORT,
                LitTypeVariant::None,
                meta
            )
        )
    }

    fn parse_record_decl_stmt(&mut self) -> ParseResult {
        let start_tok = self.token_match(TokenKind::KW_RECORD)?.pos; // match 'record' keyword
        // expect name of the record
        let id_token = self.token_match(TokenKind::T_IDENTIFIER)?.clone();
        _ = self.token_match(TokenKind::T_LBRACE); // match '{'
        let mut rec_fields = vec![];

        while self.current_token.kind != TokenKind::T_RBRACE {
            rec_fields.push(self.parse_record_field_decl_stmt()?);
        }
        
        let end_tok = self.token_match(TokenKind::T_RBRACE)?.pos; // match '}'
        let meta = NodeMeta::new(
            Span::new(
                self.current_file,
                SourcePos { 
                    line: start_tok.line, 
                    column: start_tok.column 
                },
                SourcePos { 
                    line: end_tok.line, 
                    column: end_tok.column 
                }
            ),
            vec![]
        );
        Ok(
            AST::create_leaf(
                ASTKind::StmtAST(
                    Stmt::Record(
                        RecordDeclStmt { 
                            name: id_token.lexeme.clone(),
                            size: 0,
                            alignment: 0,
                            fields: rec_fields.into_iter().enumerate().map(|(idx, field)| {
                                RecordFieldType {
                                    name: field.name.clone(),
                                    typ: field.typ,
                                    rel_stack_off: idx
                                }
                            }).collect::<Vec<RecordFieldType>>() 
                        }
                    )
                ), 
                ASTOperation::AST_RECORD_DECL,
                LitTypeVariant::Record { name: id_token.lexeme.clone() },
                meta
            )
        )
    }

    fn parse_record_field_decl_stmt(&mut self) -> Result<RecordField, Box<Diagnostic>> {
        let id_token = self.token_match(TokenKind::T_IDENTIFIER)?.clone();
        _ = self.token_match(TokenKind::T_COLON); // match ':'

        let id_type = self.parse_id_type()?;
        if id_type == LitTypeVariant::Null {
            let diag = Diagnostic::from_single_token(
                &self.current_token, 
                self.current_file, 
                "invalid type for record field", 
                Severity::Error
            );
            return Err(Box::new(diag));
        }

        self.skip_to_next_token(); // skip id type

        // check if default value has been assigned
        if self.current_token.kind == TokenKind::T_EQUAL {
            todo!("Default value is not supported right now in record field!");
        }

        _ = self.token_match(TokenKind::T_SEMICOLON); // match ';'

        Ok(
            RecordField { 
                typ: id_type, 
                name: id_token.lexeme.clone(), 
                default_value: None 
            }
        )
    }

    // parsing a function declaration and definition
    // supports multiple parameters
    fn parse_function_stmt(&mut self) -> ParseResult {
        // reset local offset counter to 0
        // self.local_offset = 0;

        // Creating a new scope for function declaration
        let func_scope_id: usize = self.sess
            .scope
            .borrow_mut()
            .enter_new_scope(ScopeType::Function);

        // match and ignore function declaration keyword 'def'
        _ = self.token_match(TokenKind::KW_DEF)?;

        /* Storage class of the function that is being parsed.
          * By default, it is set to 'GLOBAL'.
          */
        let mut func_storage_class: StorageClass = StorageClass::GLOBAL;

        // 'def' keyword could be followed by the 'extern' keyword, 
        // symbolizing the external definition of the function's body.
        if self.current_token.kind == TokenKind::KW_EXTERN {
            _ = self.token_match(TokenKind::KW_EXTERN)?;
            func_storage_class = StorageClass::EXTERN;
        }

        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER)?.clone();
        let func_name_start_pos = SourcePos {
            column: id_token.pos.column,
            line: id_token.pos.line
        };
        let func_name_end_pos = SourcePos {
            column: id_token.pos.column + id_token.lexeme.len(),
            line: id_token.pos.line
        };
        _ = self.token_match(TokenKind::T_LPAREN)?;

        let mut func_param_types: Vec<LitTypeVariant> = vec![];
        let mut func_locals = vec![];

        if self.current_token.kind != TokenKind::T_RPAREN {
            loop {
                if let Ok(param) = self.parse_parameter() {
                    let mut sym: Symbol = Symbol::create(
                        param.name.clone(), 
                        param.lit_type.clone(), 
                        SymbolType::Variable, 
                        param.lit_type.size(), 
                        StorageClass::PARAM, 
                        param.offset,
                        None,
                        self.current_function_id,
                    );

                    if let LitTypeVariant::Record{ name } = &sym.lit_type {
                        sym.sym_type = SymbolType::Record { 
                            name: name.clone()
                        };
                    }

                    func_param_types.push(sym.lit_type.clone());
                    let local_pos = self.add_symbol_local(sym);
                    func_locals.push(local_pos);
                }

                let is_tok_comma: bool = self.current_token.kind == TokenKind::T_COMMA;
                let is_tok_rparen: bool = self.current_token.kind == TokenKind::T_RPAREN;

                if !is_tok_comma && !is_tok_rparen {
                    let diag = Diagnostic::from_single_token(
                        &self.current_token, 
                        self.current_file, 
                        "unexpected token", 
                        Severity::Error
                    );
                    return Err(Box::new(diag));
                } 
                else if is_tok_rparen {
                    break;
                } 
                else {
                    self.token_match(TokenKind::T_COMMA)?;
                }
            } 
        }

        // function's return type
        self.token_match(TokenKind::T_RPAREN)?;
        let func_return_type: LitTypeVariant = self.parse_fn_ret_type()?;
        self.skip_to_next_token();

        // If this is not an extern function, ensure that the next token 
        // is a left brace ('{') before starting the function body. This 
        // prevents the parser from entering the "local" state prematurely 
        // if the function signature is invalid.
        if func_storage_class != StorageClass::EXTERN {
            _ = self.token_match_no_advance(TokenKind::T_LBRACE)?;
        }

        // And of course the function name as well :)
        self.current_function_name = Some(id_token.lexeme.clone());
        let mut function_body: Option<AST> = None;

        // create function body
        if func_storage_class != StorageClass::EXTERN {
            let function_body_res = self.parse_compound_stmt()?;
            function_body = Some(function_body_res);
        } 
        else {
            _ = self.token_match(TokenKind::T_SEMICOLON)?;
        }

        let temp_func_id: usize = self.current_function_id;
        self.current_function_id = INVALID_FUNC_ID; 

        // create a new FunctionInfo
        self.sess.scope.borrow_mut().exit_scope();

        // reset offset counter after parsing function
        // let local_offset = self.local_offset;
        // self.local_offset = 0;
        
        /*
        Stack offset calculation:
         'x29' and 'x30' has to be preserved. Thus, the extra 15 bytes has to 
         be allocated for them.
         Also the x0-x3 has to be preserved if they are used during function calls. 
         So, allocate extra 32 bytes for them as well.
         */
        // let stack_offset: i32 = (local_offset + 15 + 32) & !15;

        // Return AST for function declaration
        Ok(AST::with_meta(
            ASTKind::StmtAST(Stmt::FuncDecl(FuncDeclStmt {
                func_id: temp_func_id,
                stack_off_: 0,
                name: id_token.lexeme.clone(),
                scope_id: func_scope_id,
                return_type: func_return_type.clone(),
                storage_class: func_storage_class,
                locals: func_locals,
                func_param_types
            })),
            ASTOperation::AST_FUNCTION,
            function_body,
            None,
            None,
            func_return_type,
            NodeMeta::new(
                Span::new(
                    self.current_file, 
                    func_name_start_pos, 
                    func_name_end_pos
                ), 
                Vec::with_capacity(0)
            )
        ))
    }

    // parse function's return type
    fn parse_fn_ret_type(&mut self) -> Result<LitTypeVariant, Box<Diagnostic>> {
        _ = self.token_match(TokenKind::T_ARROW)?;
        let func_ret_type: LitTypeVariant = self.parse_id_type()?;

        if func_ret_type == LitTypeVariant::Null {
            let diag = Diagnostic::from_single_token(
                &self.current_token, 
                self.current_file, 
                "invalid return type", 
                Severity::Error
            );
            return Err(Box::new(diag));
        }

        Ok(func_ret_type)
    }

    fn parse_parameter(&mut self) -> Result<FuncParam, Box<Diagnostic>> {
        let param_name: Token = self.token_match(TokenKind::T_IDENTIFIER)?.clone();
        let _ = self.token_match(TokenKind::T_COLON)?;
        let param_type: LitTypeVariant = self.parse_id_type()?;
        // let param_loc_off: i32 = self.gen_next_local_offset() as i32;
        self.skip_to_next_token();
        Ok(FuncParam {
            lit_type: param_type,
            name: param_name.lexeme,
            offset: 0
        })
    }

    fn parse_return_stmt(&mut self) -> ParseResult {
        // check whether parser's parsing a function or not
        let inside_func = self.sess.scope.borrow().inside_function();
        if !inside_func {
            let diag = Diagnostic::from_single_token(
                &self.current_token, 
                self.current_file, 
                "unexpected token", 
                Severity::Error
            );
            return Err(Box::new(diag));
        }

        let ret_tok = self.token_match(TokenKind::KW_RETURN)?;
        let pos = SourcePos {
            line: ret_tok.pos.line,
            column: ret_tok.pos.column
        };

        if self.current_token.kind == TokenKind::T_SEMICOLON {
            let meta = NodeMeta::new(
                Span::new(
                    self.current_file,
                    pos,
                    pos
                ),
                vec![]
            );
            Ok(
                AST::create_leaf(
                    ASTKind::StmtAST(
                        Stmt::Return(
                            ReturnStmt {
                                func_id: self.current_function_id,
                            }
                        )
                    ),
                    ASTOperation::AST_RETURN,
                    LitTypeVariant::None,
                    meta
                )
            )
        }
        else {
            let return_expr: AST = self.parse_record_or_expr(None)?;
            let meta = NodeMeta::new(
                Span::new(
                    self.current_file,
                    pos,
                    return_expr.meta.span.end
                ),
                vec![]
            );
            let return_ast = AST {
                kind: ASTKind::StmtAST(
                    Stmt::Return(
                        ReturnStmt {
                            func_id: self.current_function_id,
                        }
                    )
                ),
                left: Some(Box::new(return_expr)),
                right: None,
                mid: None,
                operation: ASTOperation::AST_RETURN,
                result_type: LitTypeVariant::None,
                meta
            };
            Ok(return_ast)
        }
    }

    fn parse_while_stmt(&mut self) -> ParseResult {
        let cond_ast = self.parse_conditional_stmt(TokenKind::KW_WHILE)?;
        let while_body = self.parse_single_stmt()?;
        Ok(
            AST::new(
                ASTKind::StmtAST(Stmt::While),
                ASTOperation::AST_WHILE,
                Some(cond_ast),
                Some(while_body),
                LitTypeVariant::None,
            )
        )
    }

    fn parse_loop_stmt(&mut self) -> ParseResult {
        _ = self.token_match(TokenKind::KW_LOOP)?; // match and ignore 'loop'
        let loop_body: AST = self.parse_compound_stmt()?;
        Ok(AST::new(
            ASTKind::StmtAST(Stmt::Loop),
            ASTOperation::AST_LOOP,
            Some(loop_body),
            None,
            LitTypeVariant::None,
        ))
    }

    fn parse_break_stmt(&mut self) -> ParseResult {
        _ = self.token_match(TokenKind::KW_BREAK)?; // match and ignore 'break'
        Ok(
            AST::new(
                ASTKind::StmtAST(Stmt::Break),
                ASTOperation::AST_BREAK,
                None,
                None,
                LitTypeVariant::None
            )
        )
    }

    fn parse_for_stmt(&mut self) -> ParseResult {
        _ = self.token_match(TokenKind::KW_FOR)?; // match and ignore the keyword 'for'
        _ = self.token_match(TokenKind::T_LPAREN)?; // match and ignore '('
        let pre_stmt: AST = self.parse_single_stmt()?; // initialization statement
                                                       // _ = self.token_match(TokenKind::T_SEMICOLON);
        let cond_ast = self.parse_equality(); // conditional section of for loop
        if let Ok(_icast) = &cond_ast {
            if (_icast.operation < ASTOperation::AST_EQEQ)
                || (_icast.operation > ASTOperation::AST_LTHAN)
            {
                // if operation kind is not "relational operation"
                panic!("Please provide conditional expression for 'for'");
            }
        }
        _ = self.token_match(TokenKind::T_SEMICOLON)?; // expect semicolon
        let incr_ast = self.parse_single_stmt();

        _ = self.token_match(TokenKind::T_RPAREN)?; // match and ignore ')'
        let for_body = self.parse_single_stmt();

        let mut tree: AST = AST::new(
            ASTKind::StmtAST(Stmt::Glue),
            ASTOperation::AST_GLUE,
            for_body.ok(),
            incr_ast.ok(),
            LitTypeVariant::None,
        );
        tree = AST::new(
            ASTKind::StmtAST(Stmt::While),
            ASTOperation::AST_WHILE,
            cond_ast.ok(),
            Some(tree),
            LitTypeVariant::None,
        );
        Ok(AST::new(
            ASTKind::StmtAST(Stmt::Glue),
            ASTOperation::AST_GLUE,
            Some(pre_stmt),
            Some(tree),
            LitTypeVariant::None,
        ))
    }

    fn parse_if_stmt(&mut self) -> ParseResult {
        let cond_ast = self.parse_conditional_stmt(TokenKind::KW_IF)?;

        let if_scope = self.sess
            .scope
            .borrow_mut()
            .enter_new_scope(ScopeType::If); // enter if's scope

        let if_true_ast = self.parse_single_stmt()?;
        self.sess.scope.borrow_mut().exit_scope(); // exit

        let mut if_false_ast = None;
        if self.current_token.kind == TokenKind::KW_ELSE {
            let else_scope = self.sess
                .scope
                .borrow_mut()
                .enter_new_scope(ScopeType::If);

            self.skip_to_next_token(); // skip 'else'

            let else_block = self.parse_single_stmt()?;
            if_false_ast = Some(
                AST::new(
                    ASTKind::StmtAST(Stmt::Scoping(ScopingStmt { scope_id: else_scope })),
                    ASTOperation::AST_ELSE,
                    Some(else_block),
                    None,
                    LitTypeVariant::None
                )
            );          
            self.sess.scope.borrow_mut().exit_scope();
        }
        Ok(AST::with_mid(
            ASTKind::StmtAST(Stmt::If(IfStmt { scope_id: if_scope })),
            ASTOperation::AST_IF,
            Some(cond_ast),
            Some(if_true_ast),
            if_false_ast,
            LitTypeVariant::None,
        ))
    }

    // parses tokens that are in the form '(expression [< | > | >= | <= | == | !=] expression)'
    fn parse_conditional_stmt(&mut self, kind: TokenKind) -> ParseResult {
        _ = self.token_match(kind)?;
        _ = self.token_match(TokenKind::T_LPAREN)?; // match and ignore '('

        let cond_ast = self.parse_equality();

        if let Ok(_icast) = &cond_ast {
            if (_icast.operation < ASTOperation::AST_EQEQ) || (_icast.operation > ASTOperation::AST_LTHAN) {
                // if operation kind is not "relational operation"
                panic!("'{:?}' is not allowed in {kind:?}'s condition.", _icast.operation);
            }
        }

        _ = self.token_match(TokenKind::T_RPAREN)?; // match and ignore ')'
        cond_ast
    }

    /// Parses a variable declaration statement.
    ///
    /// This function processes the tokens and constructs an abstract syntax tree (AST)
    /// node representing a variable declaration statement. It handles parsing the variable
    /// type, name, and optionally, the initial value assignment.
    ///
    /// # Returns
    ///
    /// A `ParseResult` containing either the AST node for the variable declaration statement
    /// or a `ParseError` if the parsing fails.
    fn parse_var_decl_stmt(&mut self) -> ParseResult {
        // consume 'let'
        _ = self.token_match(TokenKind::KW_LET)?;

        // Being "inside" a function means that we are currently parsing a function's body.
        let inside_func: bool = self.sess.scope.borrow().live_scope_id() != 0;

        // Track the storage class for this variable.
        let mut var_class: StorageClass = StorageClass::GLOBAL;

        if inside_func {
            var_class = StorageClass::LOCAL;
        }

        // Track the type of this variable.
        //
        // The variable might not have any initial value.
        // Thus it is 'null' (or none) by default.
        let mut var_type: LitTypeVariant = LitTypeVariant::None;

        // symbol's type
        let mut sym_type = SymbolType::Variable;

        // Name of the variable.
        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER)?.clone();

        // self.var_offsets.insert(id_token.lexeme.clone(), self.local_offset as usize);

        // Parser may encounter a colon after the identifier name.
        // This means the type of this variable has been defined
        // by the user.
        if self.current_token.kind == TokenKind::T_COLON {
            _ = self.token_match(TokenKind::T_COLON)?;
            var_type = self.parse_id_type()?;

            // if the declared variable is a record
            if let LitTypeVariant::Record { name: rec_name } = &var_type {
                sym_type = SymbolType::Record { name: rec_name.clone() };
            }
            else {
                self.skip_to_next_token();
            }
        }

        // Stores the RHS value of this variable (if defined)
        let mut assignment_parse_res: Option<ParseResult> = None;

        // Checking whether variable is assigned at the time of its declaration.
        // If identifier name is followed by an equal sign, then it is assigned 
        // at the time of declaration.
        if self.current_token.kind == TokenKind::T_EQUAL {
            _ = self.token_match(TokenKind::T_EQUAL)?; // match and ignore '=' sign
            assignment_parse_res = Some(self.parse_record_or_expr(Some(&id_token.lexeme)));
        }

        // Default value contains compile-time evaluated expression's result.
        let mut default_value: Option<LitType> = None;

        // if there is some error during expression parsing
        if let Some(Err(parse_err)) = assignment_parse_res {
            return Err(parse_err);
        } 
        else if let Some(Ok(expr_ast)) = &assignment_parse_res {
            if let ASTKind::ExprAST(Expr::RecordCreation(record_create)) = &expr_ast.kind {
                sym_type = SymbolType::Record { 
                    name: record_create.name.clone() 
                };
            }

            if var_type == LitTypeVariant::Str {
                let str_const_label = 0;
                default_value = Some(
                    LitType::I32(
                        str_const_label as i32
                    )
                );
            }
        }

        let local_off = 0;

        if let Some(assign_ast_node_res) = assignment_parse_res {
            Ok(AST::new(
                ASTKind::StmtAST(Stmt::VarDecl(VarDeclStmt {
                    symtbl_pos: 0xFFFFFFFF, // this value will be set by the resolver
                    symbol_type: sym_type,
                    class: var_class,
                    sym_name: id_token.lexeme.clone(),
                    value_type: var_type.clone(),
                    local_offset: local_off,
                    func_id: if inside_func { self.current_function_id } else { 0xFFFFFFFF },
                    default_value
                })),
                ASTOperation::AST_VAR_DECL,
                Some(assign_ast_node_res?),
                None,
                var_type,
            ))
        } 
        else {
            panic!("Variable declared without any assignment!")
        }
    }

    /// Parses the current token as a literal type keyword and returns the 
    /// corresponding `LitTypeVariant`.
    ///
    /// Returns an error if the token does not represent a valid data type keyword.
    fn parse_id_type(&mut self) -> Result<LitTypeVariant, Box<Diagnostic>> {
        let current_tok: TokenKind = self.current_token.kind;
        match current_tok {
            TokenKind::KW_INT => Ok(LitTypeVariant::I32),
            TokenKind::KW_CHAR => Ok(LitTypeVariant::U8),
            TokenKind::KW_STR => Ok(LitTypeVariant::RawStr),
            TokenKind::KW_LONG => Ok(LitTypeVariant::I64),
            TokenKind::KW_VOID => Ok(LitTypeVariant::Void),
            TokenKind::KW_NULL => Ok(LitTypeVariant::Null),
            TokenKind::T_IDENTIFIER => Ok(
                LitTypeVariant::Record{ 
                    name: self.current_token.lexeme.to_string() 
                }
            ),
            _ => {
                let diag = Diagnostic::from_single_token(
                    &self.current_token, 
                    self.current_file, 
                    "unexpected token", 
                    Severity::Error
                );
                Err(Box::new(diag))
            }
        }
    }

    // TODO: Write comments
    fn assign_stmt_or_func_call(&mut self) -> ParseResult {
        let id_token = self.token_match(TokenKind::T_IDENTIFIER)?.clone();
        let tok_kind_after_id_tok: TokenKind = self.current_token.kind;
        if tok_kind_after_id_tok != TokenKind::T_LPAREN {
            self.parse_assignment_stmt(id_token)
        } else {
            self.parse_func_call_expr(&id_token.lexeme, &id_token)
        }
    }

    fn parse_assignment_stmt(&mut self, id_token: Token) -> ParseResult {
        _ = self.token_match(TokenKind::T_EQUAL)?;

        let bin_expr_ast_node: AST = self.parse_record_or_expr(None)?;

        // _ = self.token_match(TokenKind::T_SEMICOLON)?;

        let lvalueid: AST = AST::create_leaf(
            ASTKind::StmtAST(
                Stmt::LValue2 { 
                    name: id_token.lexeme.clone() 
                }
            ),
            ASTOperation::AST_LVIDENT,
            LitTypeVariant::None,
            NodeMeta::none()
        );

        Ok(AST::new(
            ASTKind::StmtAST(
                Stmt::Assignment(
                    AssignStmt {
                        sym_name: id_token.lexeme.clone()
                    }
                )
            ),
            ASTOperation::AST_ASSIGN,
            Some(lvalueid),
            Some(bin_expr_ast_node),
            LitTypeVariant::Void,
        ))
    }

    fn parse_record_or_expr(&mut self, rec_alias: Option<&str>) -> ParseResult {
        if self.current_token.kind == TokenKind::T_IDENTIFIER {
            if let Some(next) = self.tokens.get(self.current + 1) {
                if next.kind == TokenKind::T_LBRACE {
                    if let Some(ra) = rec_alias {
                        return self.parse_record_creation(ra);
                    }
                    else {
                        panic!("Fatal Error: Record's alias not provided! Aborting...")
                    }
                }
            }
        }
        self.parse_equality()
    }

    fn parse_record_creation(&mut self, rec_alias: &str) -> ParseResult {
        let span_start = self.current_token.pos;

        let id_token = self.token_match(TokenKind::T_IDENTIFIER)?.clone();
        _ = self.token_match(TokenKind::T_LBRACE)?;

        let mut fields = vec![];

        let mut field_off = 0;
        while self.current_token.kind != TokenKind::T_RBRACE {
            fields.push(self.parse_record_field_assignment(field_off)?);
        
            match self.current_token.kind {
                TokenKind::T_COMMA => {
                    self.token_match(TokenKind::T_COMMA)?; // match ','
                }
                TokenKind::T_RBRACE => {
                    self.token_match(TokenKind::T_RBRACE)?; // match '}'
                    break;
                }
                _ => {
                    let diag = Diagnostic::from_single_token(
                        &self.current_token, 
                        self.current_file, 
                        "unexpected token", 
                        Severity::Error
                    );
                    return Err(Box::new(diag));
                }
            }

            // make space for next field
            field_off += 1;
        }

        let span_end = self.tokens[self.current - 1].pos;
        let meta = NodeMeta::new(
            Span::new(
                self.current_file, 
                SourcePos { line: 
                    span_start.line, 
                    column: span_start.column 
                }, 
                SourcePos { 
                    line: span_end.line, 
                    column: span_end.column 
                }
            ),
            vec![]
        );

        Ok(
            AST::create_leaf(
                ASTKind::ExprAST(
                    Expr::RecordCreation(
                        RecordCreationExpr { 
                            name: id_token.lexeme.clone(), 
                            fields,
                            pool_idx: 0xFFFFFFFF, // this value will be set by the Resolver
                            rec_alias: rec_alias.to_string()
                        }
                    )
                ),
                ASTOperation::AST_RECORD_CREATE, 
                LitTypeVariant::Record { name: id_token.lexeme.clone() }, 
                meta
            )
        )
    }

    fn parse_record_field_assignment(&mut self, field_off: usize) -> Result<RecordFieldAssignExpr, Box<Diagnostic>> {
        let id_token = self.token_match(TokenKind::T_IDENTIFIER)?.clone();
        _ = self.token_match(TokenKind::T_EQUAL); // parse '='
        let field_val = self.parse_record_or_expr(None)?;

        if let ASTKind::ExprAST(expr) = field_val.kind {
            return Ok(
                RecordFieldAssignExpr { 
                    name: id_token.lexeme.clone(), 
                    value: Box::new(expr),
                    offset: field_off
                }
            );
        }

        panic!()
    }

    fn parse_equality(&mut self) -> ParseResult {
        let left = self.parse_comparision()?;
        self.try_parsing_binary(left, vec![TokenKind::T_EQEQ, TokenKind::T_NEQ])
    }

    fn parse_comparision(&mut self) -> ParseResult {
        let left = self.parse_addition()?;
        self.try_parsing_binary(
            left,
            vec![
                TokenKind::T_GTHAN,
                TokenKind::T_LTHAN,
                TokenKind::T_GTEQ,
                TokenKind::T_LTEQ,
            ],
        )
    }

    fn parse_addition(&mut self) -> ParseResult {
        let left = self.parse_factor()?;
        self.try_parsing_binary(left, vec![TokenKind::T_PLUS, TokenKind::T_MINUS])
    }

    fn parse_factor(&mut self) -> ParseResult {
        let left = self.parse_primary()?;
        self.try_parsing_binary(left, vec![TokenKind::T_SLASH, TokenKind::T_STAR])
    }

    fn try_parsing_binary(&mut self, left: AST, tokens: Vec<TokenKind>) -> ParseResult {
        let current_token_kind = self.current_token.kind;

        if !tokens.contains(&current_token_kind) {
            return Ok(left);
        }

        // start of the expression             
        let span_start = left.meta.span;

        self.skip_to_next_token(); // skip the operator

        let ast_op = ASTOperation::from_token_kind(current_token_kind).unwrap(); // DANGEROUS unwrap CALL
        let right = self.parse_equality()?;
        let left_expr = left.kind.expr().unwrap();
        let right_expr = right.kind.expr().unwrap();

        // end of the expression
        let span_end = right.meta.span;

        let combined_span = Span {
            file_id: self.current_file,
            start: span_start.start,
            end: span_end.end
        };

        Ok(
            AST::create_leaf(
                ASTKind::ExprAST(
                    Expr::Binary(BinExpr {
                        operation: ast_op,
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                        result_type: LitTypeVariant::None,
                    })
                ),
                ast_op,
                LitTypeVariant::None,
                NodeMeta::new(
                    combined_span,
                    vec![]
                )
            )
        )
    }

    fn parse_primary(&mut self) -> ParseResult {
        let current_token = self.current_token.clone();

        let start_pos = SourcePos {
            line: current_token.pos.line,
            column: current_token.pos.column
        };
        let single_token_meta = NodeMeta::new(
            Span::new(
                self.current_file,
                start_pos,
                SourcePos { 
                    line: start_pos.line, 
                    column: start_pos.column + current_token.lexeme.len()
                }
            ),
            vec![]
        );

        // go ahead
        self.skip_to_next_token();

        match current_token.kind {
            TokenKind::T_INT_NUM => {
                Ok(
                    Parser::create_expr_ast(
                        LitType::I32(current_token.lexeme.parse::<i32>().unwrap()),
                        ASTOperation::AST_INTLIT,
                        single_token_meta
                    )
                )
            },
            TokenKind::T_CHAR => {
                Ok(
                    Parser::create_expr_ast(
                        LitType::U8(current_token.lexeme.parse::<u8>().unwrap()),
                        ASTOperation::AST_INTLIT,
                        single_token_meta
                    )
                )
            },
            TokenKind::T_LONG_NUM => {
                Ok(
                    Parser::create_expr_ast(
                        LitType::I64(current_token.lexeme.parse::<i64>().unwrap()),
                        ASTOperation::AST_INTLIT,
                        single_token_meta
                    )
                )
            },
            TokenKind::T_FLOAT_NUM | TokenKind::T_DOUBLE_NUM => {
               Ok(
                    Parser::create_expr_ast(
                        LitType::F64(current_token.lexeme.parse::<f64>().unwrap()),
                        ASTOperation::AST_INTLIT,
                        single_token_meta
                    )
                ) 
            },
            TokenKind::T_STRING => { 
                Ok(AST::create_leaf(
                    ASTKind::ExprAST(
                        Expr::LitVal(
                            LitValExpr {
                                value: LitType::RawStr(current_token.lexeme.clone()),
                                result_type: LitTypeVariant::RawStr,
                            }
                        )
                    ),
                    ASTOperation::AST_STRLIT,
                    LitTypeVariant::RawStr,
                    single_token_meta
                ))
            }
            TokenKind::T_IDENTIFIER => {
                // Identifiers in a global variable declaration expression are not allowed.
                if self.is_scope_global() {
                    let diag = Diagnostic::from_single_token(
                        &self.current_token, 
                        self.current_file, 
                        "initializer not a constant",
                        Severity::Error
                    );
                    return Err(Box::new(diag));
                }
                
                let symbol_name: String = current_token.lexeme.clone();
                let curr_tok_kind: TokenKind = self.current_token.kind;

                if curr_tok_kind == TokenKind::T_LPAREN {
                    self.parse_func_call_expr(&symbol_name, &current_token)
                } 
                else if curr_tok_kind == TokenKind::T_DOT {
                    self.parse_record_field_access_expr(&symbol_name, &current_token)
                }
                else {
                    Ok(AST::create_leaf(
                        ASTKind::ExprAST(
                            Expr::Ident(IdentExpr {
                                result_type: LitTypeVariant::None, // We don't care about the type of this symbol, yet!
                                sym_name: symbol_name
                            }
                        )),
                        ASTOperation::AST_IDENT,
                        LitTypeVariant::None, // type will be identified at the semantic analysis phases if the symbol is defined
                        single_token_meta
                    ))
                }
            }
            TokenKind::T_LPAREN => {
                // group expression: e.g: (a * (b + c)))
                let group_expr = self.parse_record_or_expr(None)?;
                // Group expression terminates with ')'. Match and ignore ')'.
                let _ = self.token_match(TokenKind::T_RPAREN)?;
                Ok(group_expr)
            },

            // null type
            TokenKind::KW_NULL => {
                Ok(
                    AST::create_leaf(
                        ASTKind::ExprAST(Expr::Null), 
                        ASTOperation::AST_NULL, 
                        LitTypeVariant::Null, 
                        single_token_meta
                    )
                )
            },
            _ => {
                let diag = Diagnostic::from_single_token(
                    &self.tokens[self.current - 2], 
                    self.current_file, 
                    "unexpected token",
                    Severity::Error
                );
                Err(Box::new(diag))
            }
        }
    }

    fn create_expr_ast(value: LitType, operation: ASTOperation, meta: NodeMeta) -> AST {
        AST::create_leaf(
            ASTKind::ExprAST(
                Expr::LitVal(
                    LitValExpr {
                        value: value.clone(),
                        result_type: value.variant(),
                    }
                )
            ),
            operation,
            value.variant(),
            meta
        )
    }

    fn parse_record_field_access_expr(&mut self, rec_alias: &str, start_token: &Token) -> ParseResult {
        let mut field_chain = vec![];
        let mut end_pos = SourcePos {
            line: start_token.pos.line,
            column: start_token.pos.column
        };

        while self.current_token.kind == TokenKind::T_DOT {
            _ = self.token_match(TokenKind::T_DOT); // match '.'
            let access = self.token_match(TokenKind::T_IDENTIFIER)?;
            end_pos.line = access.pos.line;
            end_pos.column = access.pos.column;
            field_chain.push(access.lexeme.clone());
        }

        let meta = NodeMeta::new(
            Span::new(
                self.current_file,
                SourcePos { 
                    line: start_token.pos.line, 
                    column: start_token.pos.column 
                },
                end_pos
            ),
            vec![]
        );

        let field_off = if let Some(rec_base_off) = self.var_offsets.get(rec_alias) {
            *rec_base_off
        }
        else {
            0
        };

        Ok(
            AST::create_leaf(
                ASTKind::ExprAST(
                    Expr::RecordFieldAccess(
                        RecordFieldAccessExpr { 
                            rec_name: "".to_string(), // name will be set by the semantic analyser
                            rec_alias: rec_alias.to_string(), 
                            field_chain,
                            rel_stack_off: field_off,
                            result_type: LitTypeVariant::None // will be determined by the semantic analyzer
                        }
                    )
                ),
                ASTOperation::AST_RECORD_FIELD_ACCESS, 
                LitTypeVariant::None, 
                meta
            )
        )
    }

    fn parse_func_call_expr(&mut self, called_symbol: &str, start_token: &Token) -> ParseResult {
        _ = self.token_match(TokenKind::T_LPAREN)?;

        let curr_token_kind: TokenKind = self.current_token.kind;
        let mut func_args: Vec<FuncArg> = vec![];

        if curr_token_kind != TokenKind::T_RPAREN {
            let mut arg_pos: usize = 0;

            loop {
                let argu: AST = self.parse_record_or_expr(None)?;
                func_args.push((arg_pos, argu.kind.expr().unwrap()));

                let is_tok_comma: bool = self.current_token.kind == TokenKind::T_COMMA;
                let is_tok_rparen: bool = self.current_token.kind == TokenKind::T_RPAREN;

                if !is_tok_comma && !is_tok_rparen {
                    let diag = Diagnostic::from_single_token(
                        &self.current_token, 
                        self.current_file, 
                        "unexpected token",
                        Severity::Error
                    );
                    return Err(Box::new(diag));
                } 
                else if is_tok_rparen {
                    break;
                } 
                else {
                    _ = self.token_match(TokenKind::T_COMMA)?;
                }
                arg_pos += 1;
            }
        }

        let end_token = self.token_match(TokenKind::T_RPAREN)?;

        let start_pos = SourcePos {
            line: start_token.pos.line,
            column: start_token.pos.column
        };
        let end_pos = SourcePos {
            line: end_token.pos.line,
            column: end_token.pos.column
        };

        Ok(AST::create_leaf(
            ASTKind::ExprAST(
                Expr::FuncCall(
                    FuncCallExpr {
                        result_type: LitTypeVariant::None,
                        symbol_name: called_symbol.to_string(),
                        args: func_args,
                        id: 0xFFFFFFFF // resolver sets this value
                    }
                )
            ),
            ASTOperation::AST_FUNC_CALL,
            LitTypeVariant::None,
            NodeMeta::new(
                Span::new(
                    self.current_file,
                    start_pos,
                    end_pos
               ),
               vec![]
            )
        ))
    }

    /// Adds the symbol to the current scope.
    fn add_symbol_local(&mut self, sym: Symbol) -> usize {
        if let Some(insert_pos) = self.sess.scope.borrow_mut().declare(sym.clone()) {
            self.next_local_sym_pos += 1;
            insert_pos
        }
        else {
            panic!("Symbol addition failed!");
        }
    }

    fn is_scope_global(&self) -> bool {
        self.sess.scope.borrow().live_scope_id() == 0
    }

    fn token_match_no_advance(&mut self, kind: TokenKind) -> TokenMatch {
        if kind != self.current_token.kind {
            let diag = Diagnostic::from_single_token(
                &self.current_token, 
                self.current_file, 
                "unexpected token",
                Severity::Error
            );
            return Err(Box::new(diag));
        }
        Ok(&self.tokens[self.current - 1])
    }

    fn token_match(&mut self, kind: TokenKind) -> TokenMatch {
        if kind != self.current_token.kind {
            let diag = Diagnostic::from_single_token(
                &self.current_token, 
                self.current_file, 
                "unexpected token",
                Severity::Error
            );
            return Err(Box::new(diag));
        }

        self.skip_to_next_token();
        Ok(&self.tokens[self.current - 1])
    }

    fn _skip_past(&mut self, kind: TokenKind) {
        loop {
            if self.current_token.kind == kind || self.current_token.kind == TokenKind::T_EOF {
                break;
            }
            self.skip_to_next_token();
        }
        self.skip_to_next_token();
    }

    fn skip_to_next_token(&mut self) {
        self.current += 1;
        if self.current >= self.tokens.len() {
            return;
        }
        self.current_token = self.tokens[self.current].clone();
    }
}