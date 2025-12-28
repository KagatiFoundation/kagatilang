// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use core::panic;
use std::rc::Rc;

use kagc_ast::record::*;
use kagc_ast::*;
use kagc_comp_unit::source_map::FileId;
use kagc_errors::diagnostic::Diagnostic;
use kagc_errors::diagnostic::DiagnosticBag;
use kagc_errors::diagnostic::Severity;
use kagc_lexer::Tokenizer;
use kagc_scope::scope::ScopeType;
use kagc_span::span::SourcePos;
use kagc_span::span::Span;
use kagc_symbol::function::*;
use kagc_symbol::*;
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
#[derive(Debug)]
pub struct Parser<'p> {
    /// Tokens that are going to be parsed.
    tokens: Rc<Vec<Token>>,

    /// Counter which points to the current token index.
    current: usize,

    /// ID of a function that is presently being parsed. This field's
    /// value is ```INVALID_FUNC_ID``` if the parser is not inside a
    /// function.
    current_function_id: usize,

    /// Name of the function that is currently being parsed.
    current_function_name: Option<String>,

    /// Position of next local symbol.
    next_local_sym_pos: usize,

    /// Label generator that is going to be used by string literals only.
    _str_label_: usize,

    current_file: FileId,

    sess: &'p mut ParserSession,

    lexer: Tokenizer
}

impl<'p> Parser<'p> {
    /// Internal parser constructor.
    /// Use `ParserBuilder` instead.
    pub fn new(sess: &'p mut ParserSession, lexer: Tokenizer) -> Self {
        let mut p = Self {
            tokens: Rc::new(vec![]),
            current: 0,
            current_function_id: INVALID_FUNC_ID,
            current_function_name: None,
            next_local_sym_pos: 0,
            _str_label_: 0,
            current_file: sess.file_id,
            sess,
            lexer
        };
        p.tokenize_input_stream();
        p
    }

    pub(crate) fn tokenize_input_stream(&mut self) {
        let current_file_id = self.sess.file_id;
        let files = self.sess.files.borrow();
        if let Some(source_file) = files.get(current_file_id) {
            self.tokens = Rc::new(self.lexer.tokenize(source_file.content.clone()));
            self.current_function_id = current_file_id.0;
        }
        else {
            panic!("couldn't tokenize")
        }
    }

    pub fn diagnostics(&self) -> &DiagnosticBag {
        &self.sess.diagnostics
    }

    pub fn tokens(&self) -> Rc<Vec<Token>> {
        self.tokens.clone()
    }

    pub(crate) fn parse_expression(&mut self) -> Option<Expr> {
        if self.tokens.is_empty() { // lazily tokenize the input
            self.tokenize_input_stream();
        }
        match self.parse_record_or_expr(None) {
            Ok(ast) => ast.kind.expr(),
            Err(diag) => {
                self.sess.diagnostics.push(*diag);
                None
            }
        }
    }

    pub(crate) fn parse_statement(&mut self) -> Option<Stmt> {
        if self.tokens.is_empty() { // lazily tokenize the input
            self.tokenize_input_stream();
        }
        match self.parse_single_stmt() {
            Ok(ast) => ast.kind.stmt(),
            Err(diag) => {
                self.sess.diagnostics.push(*diag);
                None
            }
        }
    }

    pub fn parse(&mut self) -> Vec<AST> {
        if self.tokens.is_empty() { // lazily tokenize the input
            self.tokenize_input_stream();
        }

        let mut nodes: Vec<AST> = vec![];
        loop {
            if self.peek().kind == TokenKind::T_EOF {
                break;
            }
            match self.parse_single_stmt() {
                Ok(stmt) => nodes.push(stmt),
                Err(diag) => {
                    self.sess.diagnostics.push(*diag);
                    break;
                }
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
    pub(crate) fn parse_single_stmt(&mut self) -> ParseResult {
        let curr_tok_kind = self.peek().kind;
        match curr_tok_kind {
            TokenKind::KW_LET => {
                let ast = self.parse_var_decl_stmt()?;
                self.expect_semicolon();
                Ok(ast)
            }
            TokenKind::KW_RETURN => {
                let ast = self.parse_return_stmt()?;
                self.expect_semicolon();
                Ok(ast)
            }
            TokenKind::KW_BREAK => {
                let ast = self.parse_break_stmt()?;
                self.expect_semicolon();
                Ok(ast)
            }
            TokenKind::T_IDENTIFIER => {
                let ast = self.assign_stmt_or_func_call()?;
                self.expect_semicolon();
                Ok(ast)
            }
            TokenKind::KW_DEF => self.parse_function_stmt(),
            TokenKind::KW_IF => self.parse_if_stmt(),
            TokenKind::KW_WHILE => self.parse_while_stmt(),
            TokenKind::KW_FOR => self.parse_for_stmt(),
            TokenKind::T_LBRACE => self.parse_compound_stmt(),
            TokenKind::KW_LOOP => self.parse_loop_stmt(),
            TokenKind::KW_IMPORT => self.parse_import_stmt(),
            TokenKind::KW_RECORD => self.parse_record_decl_stmt(),
            _ => {
                Err(Box::new(
                    Diagnostic::from_single_token(
                        self.peek(), 
                        self.current_file, 
                        &format!("unexpected token {:#?}", curr_tok_kind),
                        Severity::Error
                    )
                ))
            }
        }
    }

    fn expect_semicolon(&mut self) {
        _ = self.consume(TokenKind::T_SEMICOLON, "expected a ';'");
    }

    // parse compound statement(statement starting with '{' and ending with '}')
    fn parse_compound_stmt(&mut self) -> ParseResult {
        _ = self.consume(TokenKind::T_LBRACE, "'{' expected")?;

        let mut left: Option<AST> = None;
        let mut stmt_count: i32 = 0;
        loop {
            if self.peek().kind == TokenKind::T_RBRACE {
                _ = self.consume(TokenKind::T_RBRACE, "'}' expected")?;
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
        let start_tok = self.consume(TokenKind::KW_IMPORT, "'import' expected")?.pos; // match 'import' keyword
        let module_path_tok: Token = self.consume(TokenKind::T_STRING, "expected a string")?.clone();
        let end_tok = self.consume(TokenKind::T_SEMICOLON, "';' expected")?.pos; // match ';'

        let meta = NodeMeta::new(
            Span::new(
                self.current_file.0,
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
        let start_tok = self.consume(TokenKind::KW_RECORD, "expected keyword 'record'")?.pos; // match 'record' keyword
        // expect name of the record
        let id_token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?.clone();
        _ = self.consume(TokenKind::T_LBRACE, "expected '{'"); // match '{'
        let mut rec_fields = vec![];

        while self.peek().kind != TokenKind::T_RBRACE {
            rec_fields.push(self.parse_record_field_decl_stmt()?);
        }
        
        let end_tok = self.consume(TokenKind::T_RBRACE, "expected '}'")?.pos; // match '}'
        let meta = NodeMeta::new(
            Span::new(
                self.current_file.0,
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
        let id_token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?.clone();
        _ = self.consume(TokenKind::T_COLON, "expected a ':'"); // match ':'

        let id_type = self.parse_id_type()?;
        if id_type == LitTypeVariant::Null {
            let diag = Diagnostic::from_single_token(
                self.peek(), 
                self.current_file, 
                "invalid type for record field", 
                Severity::Error
            );
            return Err(Box::new(diag));
        }

        self.skip_to_next_token(); // skip id type

        // check if default value has been assigned
        if self.peek().kind == TokenKind::T_EQUAL {
            todo!("Default value is not supported right now in record field!");
        }

        _ = self.consume(TokenKind::T_SEMICOLON, "expected ';'"); // match ';'

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
        let func_scope_id: usize = self.sess
            .scope
            .borrow_mut()
            .enter_new_scope(ScopeType::Function);

        // match and ignore function declaration keyword 'def'
        _ = self.consume(TokenKind::KW_DEF, "expected keyword 'def'")?;

        /* Storage class of the function that is being parsed.
          * By default, it is set to 'GLOBAL'.
          */
        let mut func_storage_class: StorageClass = StorageClass::GLOBAL;

        // 'def' keyword could be followed by the 'extern' keyword, 
        // symbolizing the external definition of the function's body.
        if self.peek().kind == TokenKind::KW_EXTERN {
            _ = self.consume(TokenKind::KW_EXTERN, "expected keyword 'extern'")?;
            func_storage_class = StorageClass::EXTERN;
        }

        let id_token: Token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?.clone();
        let func_name_start_pos = SourcePos {
            column: id_token.pos.column,
            line: id_token.pos.line
        };
        let func_name_end_pos = SourcePos {
            column: id_token.pos.column + id_token.lexeme.len(),
            line: id_token.pos.line
        };
        _ = self.consume(TokenKind::T_LPAREN, "expected '('")?;

        let mut func_param_types: Vec<LitTypeVariant> = vec![];
        let mut func_locals = vec![];

        if self.peek().kind != TokenKind::T_RPAREN {
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

                let is_tok_comma: bool = self.peek().kind == TokenKind::T_COMMA;
                let is_tok_rparen: bool = self.peek().kind == TokenKind::T_RPAREN;

                if !is_tok_comma && !is_tok_rparen {
                    let diag = Diagnostic::from_single_token(
                        self.peek(), 
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
                    self.consume(TokenKind::T_COMMA, "expected ','")?;
                }
            } 
        }

        // function's return type
        self.consume(TokenKind::T_RPAREN, "expected ')'")?;
        let func_return_type: LitTypeVariant = self.parse_fn_ret_type()?;
        self.skip_to_next_token();

        // If this is not an extern function, ensure that the next token 
        // is a left brace ('{') before starting the function body. This 
        // prevents the parser from entering the "local" state prematurely 
        // if the function signature is invalid.
        if func_storage_class != StorageClass::EXTERN {
            _ = self.consume_no_advance(TokenKind::T_LBRACE)?;
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
            _ = self.consume(TokenKind::T_SEMICOLON, "expected ';'")?;
        }

        let temp_func_id: usize = self.current_function_id;
        self.current_function_id = INVALID_FUNC_ID; 

        // create a new FunctionInfo
        self.sess.scope.borrow_mut().exit_scope();

        // Return AST for function declaration
        Ok(AST::with_meta(
            ASTKind::StmtAST(Stmt::FuncDecl(FuncDeclStmt {
                func_id: temp_func_id,
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
                    self.current_file.0, 
                    func_name_start_pos, 
                    func_name_end_pos
                ), 
                Vec::with_capacity(0)
            )
        ))
    }

    // parse function's return type
    fn parse_fn_ret_type(&mut self) -> Result<LitTypeVariant, Box<Diagnostic>> {
        _ = self.consume(TokenKind::T_ARROW, "expected '->'")?;
        let func_ret_type: LitTypeVariant = self.parse_id_type()?;

        if func_ret_type == LitTypeVariant::Null {
            let diag = Diagnostic::from_single_token(
                self.peek(), 
                self.current_file, 
                "invalid return type", 
                Severity::Error
            );
            return Err(Box::new(diag));
        }

        Ok(func_ret_type)
    }

    fn parse_parameter(&mut self) -> Result<FuncParam, Box<Diagnostic>> {
        let param_name: Token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?.clone();
        let _ = self.consume(TokenKind::T_COLON, "expected ':'")?;
        let param_type: LitTypeVariant = self.parse_id_type()?;
        let param_loc_off: i32 = self.gen_next_local_offset() as i32;
        self.skip_to_next_token();
        Ok(FuncParam {
            lit_type: param_type,
            name: param_name.lexeme,
            offset: param_loc_off
        })
    }

    fn gen_next_local_offset(&mut self) -> usize {
        let idx = self.next_local_sym_pos;
        self.next_local_sym_pos += 1;
        idx
    }

    fn parse_return_stmt(&mut self) -> ParseResult {
        // check whether parser's parsing a function or not
        let inside_func = self.sess.scope.borrow().inside_function();
        if !inside_func {
            let diag = Diagnostic::from_single_token(
                self.peek(), 
                self.current_file, 
                "unexpected token", 
                Severity::Error
            );
            return Err(Box::new(diag));
        }

        let ret_tok = self.consume(TokenKind::KW_RETURN, "expected keyword 'return'")?;
        let pos = SourcePos {
            line: ret_tok.pos.line,
            column: ret_tok.pos.column
        };

        if self.peek().kind == TokenKind::T_SEMICOLON {
            let meta = NodeMeta::new(
                Span::new(
                    self.current_file.0,
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
                    self.current_file.0,
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
        _ = self.consume(TokenKind::KW_LOOP, "expected the keyword 'loop'")?; // match and ignore 'loop'
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
        _ = self.consume(TokenKind::KW_BREAK, "expected the keyword 'break'")?; // match and ignore 'break'
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
        assert!(self.peek().kind == TokenKind::KW_FOR, "cannot parse a for statement");
        self.consume(TokenKind::KW_FOR, "'for' expected")?;
        let id_ast = self.parse_identifier()?;

        self.consume(TokenKind::KW_IN, "'in' expected")?;

        let expr_ast = self.parse_record_or_expr(None)?;
        let body_ast = self.parse_compound_stmt()?;
        Ok(AST::with_mid(
            ASTKind::StmtAST(Stmt::Glue),
            ASTOperation::AST_GLUE,
            Some(id_ast),
            Some(expr_ast),
            Some(body_ast),
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
        if self.peek().kind == TokenKind::KW_ELSE {
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
        _ = self.consume(kind, &format!("expected the keyword '{k}'", k = kind.as_str()))?;
        _ = self.consume(TokenKind::T_LPAREN, "expected a '('")?; // match and ignore '('

        let cond_ast = self.parse_equality();

        if let Ok(_icast) = &cond_ast {
            if (_icast.operation < ASTOperation::AST_EQEQ) || (_icast.operation > ASTOperation::AST_LTHAN) {
                // if operation kind is not "relational operation"
                panic!("'{:?}' is not allowed in {kind:?}'s condition.", _icast.operation);
            }
        }

        _ = self.consume(TokenKind::T_RPAREN, "expected a ')'")?; // match and ignore ')'
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
        _ = self.consume(TokenKind::KW_LET, "expected the keyword 'let'")?;

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
        let id_token: Token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?.clone();

        // self.var_offsets.insert(id_token.lexeme.clone(), self.local_offset as usize);

        // Parser may encounter a colon after the identifier name.
        // This means the type of this variable has been defined
        // by the user.
        if self.peek().kind == TokenKind::T_COLON {
            _ = self.consume(TokenKind::T_COLON, "expected a ':'")?;
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
        if self.peek().kind == TokenKind::T_EQUAL {
            _ = self.consume(TokenKind::T_EQUAL, "expected a '='")?; // match and ignore '=' sign
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
                default_value = Some(LitType::I32(self.next_str_label() as i32));
            }
        }

        let local_off = if inside_func {
            if let SymbolType::Record { .. } = sym_type {
                0
            } 
            else {
                self.gen_next_local_offset()
            }
        }
        else {
            0
        };

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

    fn next_str_label(&mut self) -> usize {
        let l = self._str_label_;
        self._str_label_ += 1;
        l
    }

    /// Parses the current token as a literal type keyword and returns the 
    /// corresponding `LitTypeVariant`.
    ///
    /// Returns an error if the token does not represent a valid data type keyword.
    fn parse_id_type(&mut self) -> Result<LitTypeVariant, Box<Diagnostic>> {
        let current_tok: TokenKind = self.peek().kind;
        match current_tok {
            TokenKind::KW_INT => Ok(LitTypeVariant::I32),
            TokenKind::KW_CHAR => Ok(LitTypeVariant::U8),
            TokenKind::KW_STR => Ok(LitTypeVariant::RawStr),
            TokenKind::KW_LONG => Ok(LitTypeVariant::I64),
            TokenKind::KW_VOID => Ok(LitTypeVariant::Void),
            TokenKind::KW_NULL => Ok(LitTypeVariant::Null),
            TokenKind::T_IDENTIFIER => Ok(
                LitTypeVariant::Record{ 
                    name: self.peek().lexeme.to_string() 
                }
            ),
            _ => {
                let diag = Diagnostic::from_single_token(
                    self.peek(), 
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
        let id_token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?.clone();
        let tok_kind_after_id_tok: TokenKind = self.peek().kind;
        if tok_kind_after_id_tok != TokenKind::T_LPAREN {
            self.parse_assignment_stmt(id_token)
        } else {
            self.parse_func_call_expr(&id_token.lexeme, &id_token)
        }
    }

    fn parse_assignment_stmt(&mut self, id_token: Token) -> ParseResult {
        _ = self.consume(TokenKind::T_EQUAL, "expected an '='")?;

        let bin_expr_ast_node: AST = self.parse_record_or_expr(None)?;

        // _ = self.consume(TokenKind::T_SEMICOLON)?;

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
        // peek ahead to see if this is a Record initialization: Identifier { ...
        if self.check(TokenKind::T_IDENTIFIER) && self.look_ahead(1).kind == TokenKind::T_LBRACE {
            if let Some(ra) = rec_alias {
                return self.parse_record_creation(ra);
            } else {
                unreachable!();
            }
        }
        self.parse_equality()
    }

    fn parse_record_creation(&mut self, rec_alias: &str) -> ParseResult {
        let span_start = self.peek().pos;

        let id_token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?.clone();
        _ = self.consume(TokenKind::T_LBRACE, "expected '{")?;

        let mut fields = vec![];

        let mut field_off = 0;
        while self.peek().kind != TokenKind::T_RBRACE {
            fields.push(self.parse_record_field_assignment(field_off)?);
        
            match self.peek().kind {
                TokenKind::T_COMMA => {
                    self.consume(TokenKind::T_COMMA, "expected a ','")?; // match ','
                }
                TokenKind::T_RBRACE => {
                    self.consume(TokenKind::T_RBRACE, "expected a '}'")?; // match '}'
                    break;
                }
                _ => {
                    let diag = Diagnostic::from_single_token(
                        self.peek(), 
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
                self.current_file.0, 
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
        let id_token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?.clone();
        _ = self.consume(TokenKind::T_EQUAL, "expected an '='"); // parse '='
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
        let current_token_kind = self.peek().kind;
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
            file_id: self.current_file.0,
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
        let file_id = self.current_file.0;
        let current_token = self.advance().clone();

        let start_pos = SourcePos {
            line: current_token.pos.line,
            column: current_token.pos.column
        };
        let single_token_meta = NodeMeta::new(
            Span::new(
                file_id,
                start_pos,
                SourcePos { 
                    line: start_pos.line, 
                    column: start_pos.column + current_token.lexeme.len()
                }
            ),
            vec![]
        );

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
                        self.peek(), 
                        self.current_file, 
                        "initializer not a constant",
                        Severity::Error
                    );
                    return Err(Box::new(diag));
                }
                
                let symbol_name: String = current_token.lexeme.clone();
                let curr_tok_kind: TokenKind = self.peek().kind;

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
                let _ = self.consume(TokenKind::T_RPAREN, "expected a ')'")?;
                Ok(group_expr)
            },

            // null type
            TokenKind::KW_NULL => {
                Ok(AST::create_leaf(
                    ASTKind::ExprAST(Expr::Null), 
                    ASTOperation::AST_NULL, 
                    LitTypeVariant::Null, 
                    single_token_meta
                ))
            },
            _ => {
                Err(Box::new(
                    Diagnostic::from_single_token(
                        self.peek(), 
                        self.current_file, 
                        "unexpected token",
                        Severity::Error
                    )
                ))
            }
        }
    }

    fn parse_identifier(&mut self) -> ParseResult {
        let file_id = self.current_file.0;
        let id = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?;
        let meta = NodeMeta::new(
            Span::new(
                file_id,
                SourcePos { 
                    line: id.pos.line, 
                    column: id.pos.column
                }, 
                SourcePos {
                    line: id.pos.line,
                    column: id.pos.column
                }
            ), 
            Vec::with_capacity(0)
        );
        Ok(AST::create_leaf(
            ASTKind::ExprAST(
                Expr::Ident(
                    IdentExpr { 
                        sym_name: id.lexeme.clone(), 
                        result_type: LitTypeVariant::None 
                    }
                )
            ),
            ASTOperation::AST_IDENT,
            LitTypeVariant::None,
            meta
        ))
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

        while self.peek().kind == TokenKind::T_DOT {
            _ = self.consume(TokenKind::T_DOT, "expected a '.'"); // match '.'
            let access = self.consume(TokenKind::T_IDENTIFIER, "expected an identifer")?;
            end_pos.line = access.pos.line;
            end_pos.column = access.pos.column;
            field_chain.push(access.lexeme.clone());
        }

        let meta = NodeMeta::new(
            Span::new(
                self.current_file.0,
                SourcePos { 
                    line: start_token.pos.line, 
                    column: start_token.pos.column 
                },
                end_pos
            ),
            vec![]
        );

        Ok(
            AST::create_leaf(
                ASTKind::ExprAST(
                    Expr::RecordFieldAccess(
                        RecordFieldAccessExpr { 
                            rec_name: "".to_string(), // name will be set by the semantic analyser
                            rec_alias: rec_alias.to_string(), 
                            field_chain,
                            rel_stack_off: 0xFFFFFFFF, // resolver resolves this
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
        _ = self.consume(TokenKind::T_LPAREN, "expected a '('")?;

        let curr_token_kind: TokenKind = self.peek().kind;
        let mut func_args: Vec<FuncArg> = vec![];

        if curr_token_kind != TokenKind::T_RPAREN {
            let mut arg_pos: usize = 0;

            loop {
                let argu: AST = self.parse_record_or_expr(None)?;
                func_args.push((arg_pos, argu.kind.expr().unwrap()));

                let is_tok_comma: bool = self.peek().kind == TokenKind::T_COMMA;
                let is_tok_rparen: bool = self.peek().kind == TokenKind::T_RPAREN;

                if !is_tok_comma && !is_tok_rparen {
                    let diag = Diagnostic::from_single_token(
                        self.peek(), 
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
                    _ = self.consume(TokenKind::T_COMMA, "expected a ','")?;
                }
                arg_pos += 1;
            }
        }

        let end_token = self.consume(TokenKind::T_RPAREN, "expected a ')'")?;

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
                    self.current_file.0,
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

    fn consume_no_advance(&mut self, kind: TokenKind) -> TokenMatch {
        if kind != self.peek().kind {
            let diag = Diagnostic::from_single_token(
                self.peek(), 
                self.current_file, 
                "unexpected token",
                Severity::Error
            );
            return Err(Box::new(diag));
        }
        Ok(&self.tokens[self.current - 1])
    }

    fn skip_to_next_token(&mut self) {
        self.current += 1;
    }

    /// Look at the current token without consuming it.
    fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap_or_else(|| self.tokens.last().unwrap())
    }

    /// Look ahead N tokens
    fn look_ahead(&self, distance: usize) -> &Token {
        self.tokens.get(self.current + distance).unwrap_or_else(|| self.tokens.last().unwrap())
    }

    /// Check if the current token matches a kind
    fn check(&self, kind: TokenKind) -> bool {
        self.peek().kind == kind
    }

    fn consume(&mut self, kind: TokenKind, msg: &str) -> Result<&Token, Box<Diagnostic>> {
        let tok = self.peek();
        if tok.kind == kind {
            if kind == TokenKind::T_EOF && self.current != self.tokens.len() - 1 {
                unreachable!("EOF not at end of token stream");
            }
            Ok(self.advance())
        } else {
            Err(Box::new(Diagnostic::from_single_token(
                tok,
                self.current_file,
                msg,
                Severity::Error,
            )))
        }
    }

    fn advance(&mut self) -> &Token {
        if self.current < self.tokens.len() {
            let tok = &self.tokens[self.current];
            self.current += 1;
            tok
        } else {
            self.tokens.last().unwrap()
        }
    }
}

#[cfg(test)]
mod tests {
    use kagc_lexer::Tokenizer;
    use kagc_token::TokenKind;

    use crate::{Parser, session::ParserSession};

    #[test]
    fn test_token_advance_logic_holds_correct() {
        let mut session = ParserSession::from_string("let a = 12 + 12;");
        let lexer = Tokenizer::new();
        let mut parser = Parser::new(&mut session, lexer);
        assert!(parser.advance().kind == TokenKind::KW_LET);
        assert!(parser.advance().kind == TokenKind::T_IDENTIFIER);
        assert!(parser.advance().kind == TokenKind::T_EQUAL);
        assert!(parser.advance().kind == TokenKind::T_CHAR);
        assert!(parser.advance().kind == TokenKind::T_PLUS);
        assert!(parser.advance().kind == TokenKind::T_CHAR);
        assert!(parser.advance().kind == TokenKind::T_SEMICOLON);
        assert!(parser.advance().kind == TokenKind::T_EOF);
    }

    #[test]
    fn test_advance_is_idempotent_after_eof() {
        let mut session = ParserSession::from_string("let a = 12 + 12;");
        let lexer = Tokenizer::new();
        let mut parser = Parser::new(&mut session, lexer);

        while parser.advance().kind != TokenKind::T_EOF {}

        assert_eq!(parser.advance().kind, TokenKind::T_EOF);
        assert_eq!(parser.advance().kind, TokenKind::T_EOF);
    }
}