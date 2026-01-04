// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use core::panic;
use std::cell::Cell;

use kagc_ast::record::*;
use kagc_ast::*;
use kagc_comp_unit::source_map::FileId;
use kagc_errors::diagnostic::Diagnostic;
use kagc_errors::diagnostic::DiagnosticBag;
use kagc_errors::diagnostic::Severity;
use kagc_span::span::SourcePos;
use kagc_span::span::Span;
use kagc_symbol::function::*;
use kagc_symbol::*;
use kagc_token::*;
use kagc_types::TyKind;
use kagc_types::record::RecordFieldType;
use kagc_types::LitValue;

use crate::options::ParserOptions;

pub(crate) type ParseOutput<'tcx> = Option<AST<'tcx>>;

/// Represents an invalid function ID.
///
/// This constant is used to indicate that a function ID is not valid or
/// not set, serving as a sentinel value during parsing and code generation
/// to detect error states and invalid contexts.
const INVALID_FUNC_ID: usize = 0xFFFFFFFF;

pub type StringLabel = usize;

/// Represents a parser for converting tokens into abstract syntax trees (ASTs).
pub struct Parser<'p, 'tcx> where 'tcx: 'p {
    /// Tokens that are going to be parsed.
    tokens: Vec<Token<'tcx>>,

    /// Counter which points to the current token index.
    current: usize,

    /// ID of a function that is presently being parsed. This field's
    /// value is ```INVALID_FUNC_ID``` if the parser is not inside a
    /// function.
    current_function_id: usize,

    /// Position of next local symbol.
    next_local_sym_pos: usize,

    /// Label generator that is going to be used by string literals only.
    _str_label_: usize,

    current_file: FileId,

    pub diagnostics: &'p DiagnosticBag,
    pub options: ParserOptions,
}

impl<'p, 'tcx> Parser<'p, 'tcx> where 'tcx: 'p {
    pub fn new(
        options: ParserOptions,
        diags: &'p DiagnosticBag,
        tokens: Vec<Token<'tcx>>
    ) -> Self {
        Self {
            tokens,
            current: 0,
            current_function_id: INVALID_FUNC_ID,
            next_local_sym_pos: 0,
            _str_label_: 0,
            current_file: FileId(0),
            options,
            diagnostics: diags,
        }
    }

    pub fn parse_expression(&mut self) -> Option<Expr<'tcx>> {
        if self.tokens.is_empty() {
            return None;
        }
        match self.parse_record_or_expr(None) {
            Some(ast) => ast.kind.expr(),
            None => None
        }
    }

    pub fn parse_statement(&mut self) -> Option<Stmt<'tcx>> {
        if self.tokens.is_empty() {
            return None;
        }
        match self.parse_single_stmt() {
            Some(ast) => ast.kind.stmt(),
            None => None
        }
    }

    pub fn parse(&mut self) -> Vec<AST<'tcx>> {
        if self.tokens.is_empty() {
            return Vec::with_capacity(0);
        }

        let mut nodes: Vec<AST> = vec![];
        loop {
            let peek_kind = self.peek().kind;
            if peek_kind == TokenKind::T_EOF {
                break;
            }
            match self.parse_single_stmt() {
                Some(stmt) => nodes.push(stmt),
                None => break
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
    pub(crate) fn parse_single_stmt(&mut self) -> ParseOutput<'tcx> {
        let curr_tok_kind = self.peek().kind;
        match curr_tok_kind {
            TokenKind::KW_LET => self.parse_var_decl_stmt(),
            TokenKind::KW_RETURN => self.parse_return_stmt(),
            TokenKind::KW_BREAK => self.parse_break_stmt(),
            TokenKind::T_IDENTIFIER => self.assign_stmt_or_func_call(),
            TokenKind::KW_DEF => self.parse_function_stmt(),
            TokenKind::KW_IF => self.parse_if_stmt(),
            TokenKind::KW_WHILE => self.parse_while_stmt(),
            TokenKind::KW_FOR => self.parse_for_stmt(),
            TokenKind::T_LBRACE => self.parse_compound_stmt(),
            TokenKind::KW_LOOP => self.parse_loop_stmt(),
            TokenKind::KW_IMPORT => self.parse_import_stmt(),
            TokenKind::KW_RECORD => self.parse_record_decl_stmt(),
            _ => {
                self.report_unexpected_token();
                None
            }
        }
    }

    fn expect_semicolon(&mut self) {
        _ = self.consume(TokenKind::T_SEMICOLON, "expected a ';'");
    }

    // parse compound statement(statement starting with '{' and ending with '}')
    fn parse_compound_stmt(&mut self) -> ParseOutput<'tcx> {
        self.consume(TokenKind::T_LBRACE, "'{' expected")?;
        let mut statements = vec![];
        loop {
            if self.peek().kind == TokenKind::T_RBRACE {
                self.consume(TokenKind::T_RBRACE, "'}' expected")?;
                break;
            }
            if let Some(statement) = self.parse_single_stmt() {
                statements.push(statement);
            }
        }
        if statements.is_empty() {
            return Some(AST::empty());
        }
        let block_stmt_ast = Stmt::Block(BlockStmt { statements });
        Some(
            AST::create_leaf(
                ASTKind::StmtAST(block_stmt_ast),
                ASTOperation::AST_BLOCK,
                None,
                NodeMeta::none()
            )
        )
    }

    fn parse_import_stmt(&mut self) -> ParseOutput<'tcx> {
        let start_tok = self.consume(TokenKind::KW_IMPORT, "'import' expected")?.pos;
        let module_path_tok: Token = self.consume(TokenKind::T_STRING, "expected a string")?;
        let end_tok = self.consume(TokenKind::T_SEMICOLON, "';' expected")?.pos;

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
        Some(
            AST::create_leaf(
                ASTKind::StmtAST(
                    Stmt::Import(
                        ImportStmt { path: module_path_tok.lexeme }
                    )
                ), 
                ASTOperation::AST_IMPORT,
                None,
                meta
            )
        )
    }

    fn parse_record_decl_stmt(&mut self) -> ParseOutput<'tcx> {
        let start_tok = self.consume(TokenKind::KW_RECORD, "'record' expected")?.pos;
        // expect name of the record
        let id_token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?;
        _ = self.consume(TokenKind::T_LBRACE, "'{' expected");
        let mut rec_fields = vec![];

        while self.peek().kind != TokenKind::T_RBRACE {
            rec_fields.push(self.parse_record_field_decl_stmt()?);
        }
        
        let end_tok = self.consume(TokenKind::T_RBRACE, "'}' expected")?.pos;
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
        Some(
            AST::create_leaf(
                ASTKind::StmtAST(
                    Stmt::Record(
                        RecordDeclStmt { 
                            name: id_token.lexeme,
                            size: 0,
                            alignment: 0,
                            fields: rec_fields.into_iter().enumerate().map(|(idx, field)| {
                                RecordFieldType {
                                    name: field.name,
                                    ty: field.typ,
                                    rel_stack_off: idx
                                }
                            }).collect::<Vec<RecordFieldType>>() 
                        }
                    )
                ), 
                ASTOperation::AST_RECORD_DECL,
                None,
                // LitTypeVariant::Record { name: id_token.lexeme },
                meta
            )
        )
    }

    fn parse_record_field_decl_stmt(&mut self) -> Option<RecordField<'tcx>> {
        let id_token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?.lexeme;
        _ = self.consume(TokenKind::T_COLON, "':' expected");

        let id_type = self.parse_id_type()?;
        if id_type == TyKind::Null{
            self.diagnostics.push(
                Diagnostic::from_single_token(
                    &self.peek(), 
                    self.current_file, 
                    "invalid type for record field", 
                    Severity::Error
                )
            );
            return None;
        }

        self.advance();
        _ = self.consume(TokenKind::T_SEMICOLON, "';' expected");
        Some(
            RecordField { 
                typ: id_type, 
                name: id_token, 
                default_value: None 
            }
        )
    }

    // parsing a function declaration and definition
    // supports multiple parameters
    fn parse_function_stmt(&mut self) -> ParseOutput<'tcx> {
        // match and ignore function declaration keyword 'def'
        _ = self.consume(TokenKind::KW_DEF, "'def' expected")?;

        /* Storage class of the function that is being parsed.
          * By default, it is set to 'GLOBAL'.
          */
        let mut func_storage_class: StorageClass = StorageClass::GLOBAL;

        // 'def' keyword could be followed by the 'extern' keyword, 
        // symbolizing the external definition of the function's body.
        if self.peek().kind == TokenKind::KW_EXTERN {
            _ = self.consume(TokenKind::KW_EXTERN, "'extern' expected")?;
            func_storage_class = StorageClass::EXTERN;
        }

        let id_token: Token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?;
        let func_name_start_pos = SourcePos {
            column: id_token.pos.column,
            line: id_token.pos.line
        };
        let func_name_end_pos = SourcePos {
            column: id_token.pos.column + id_token.lexeme.len(),
            line: id_token.pos.line
        };
        _ = self.consume(TokenKind::T_LPAREN, "'(' expected")?;

        let mut func_param_types = vec![];
        let mut func_locals = vec![];

        if self.peek().kind != TokenKind::T_RPAREN {
            loop {
                if let Some(param) = self.parse_parameter() {
                    let mut sym = Sym::new(
                        param.name, 
                        param.ty, 
                        SymTy::Variable, 
                        StorageClass::PARAM, 
                        FuncId(self.current_function_id)
                    );
                    if let TyKind::Record{ name } = sym.ty.get() {
                        sym.sym_ty = Cell::new(SymTy::Record { name });
                    }

                    func_param_types.push(sym.ty.get());
                    let local_pos = 0; // self.add_symbol_local(sym);
                    func_locals.push(local_pos);
                }

                let is_tok_comma: bool = self.peek().kind == TokenKind::T_COMMA;
                let is_tok_rparen: bool = self.peek().kind == TokenKind::T_RPAREN;

                if !is_tok_comma && !is_tok_rparen {
                    self.report_unexpected_token();
                    return None;
                } 
                else if is_tok_rparen {
                    break;
                } 
                else {
                    self.consume(TokenKind::T_COMMA, "',' expected")?;
                }
            } 
        }

        // function's return type
        self.consume(TokenKind::T_RPAREN, "')' expected")?;
        let func_return_type = self.parse_fn_ret_type()?;
        self.advance();

        let mut function_body: Option<AST> = None;

        // create function body
        if func_storage_class != StorageClass::EXTERN {
            let function_body_res = self.parse_compound_stmt()?;
            function_body = Some(function_body_res);
        } 
        else {
            _ = self.consume(TokenKind::T_SEMICOLON, "';' expected")?;
        }

        let temp_func_id = self.current_function_id;
        self.current_function_id = INVALID_FUNC_ID; 

        // Return AST for function declaration
        Some(AST::with_meta(
            ASTKind::StmtAST(Stmt::FuncDecl(FuncDeclStmt {
                id: FuncId(temp_func_id),
                name: id_token.lexeme,
                ty: func_return_type,
                storage_class: func_storage_class,
                locals: func_locals,
                func_param_types
            })),
            ASTOperation::AST_FUNCTION,
            function_body,
            None,
            None,
            Some(func_return_type),
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
    fn parse_fn_ret_type(&mut self) -> Option<TyKind<'tcx>> {
        _ = self.consume(TokenKind::T_ARROW, "'->' expected")?;
        let func_ret_type = self.parse_id_type()?;

        if func_ret_type == TyKind::Null {
            self.diagnostics.push(
                Diagnostic::from_single_token(
                    &self.peek(), 
                    self.current_file, 
                    "invalid return type", 
                    Severity::Error
                )
            );
            return None;
        }
        Some(func_ret_type)
    }

    fn parse_parameter(&mut self) -> Option<FuncParam<'tcx>> {
        let param_name = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?;
        let _ = self.consume(TokenKind::T_COLON, "':' expected")?;
        let param_type = self.parse_id_type()?;
        let param_loc_off: i32 = self.gen_next_local_offset() as i32;
        _ = self.advance();
        Some(
            FuncParam {
                ty: param_type,
                name: param_name.lexeme,
                offset: param_loc_off
            }
        )
    }

    fn gen_next_local_offset(&mut self) -> usize {
        let idx = self.next_local_sym_pos;
        self.next_local_sym_pos += 1;
        idx
    }

    fn parse_return_stmt(&mut self) -> ParseOutput<'tcx> {
        // check whether parser's parsing a function or not
        let inside_func = false;
        if !inside_func {
            self.report_unexpected_token();
            return None;
        }

        let ret_tok = self.consume(TokenKind::KW_RETURN, "'return' expected")?;
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
            Some(
                AST::create_leaf(
                    ASTKind::StmtAST(
                        Stmt::Return(
                            ReturnStmt {
                                func_id: FuncId(self.current_function_id),
                            }
                        )
                    ),
                    ASTOperation::AST_RETURN,
                    None,
                    meta
                )
            )
        }
        else {
            let return_expr = self.parse_record_or_expr(None)?;
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
                            func_id: FuncId(self.current_function_id),
                        }
                    )
                ),
                left: Some(Box::new(return_expr)),
                right: None,
                mid: None,
                operation: ASTOperation::AST_RETURN,
                ty: None,
                meta
            };
            Some(return_ast)
        }
    }

    fn parse_while_stmt(&mut self) -> ParseOutput<'tcx> {
        let cond_ast = self.parse_conditional_stmt(TokenKind::KW_WHILE)?;
        let while_body = self.parse_single_stmt()?;
        Some(
            AST::new(
                ASTKind::StmtAST(Stmt::While),
                ASTOperation::AST_WHILE,
                Some(cond_ast),
                Some(while_body),
                None
            )
        )
    }

    fn parse_loop_stmt(&mut self) -> ParseOutput<'tcx> {
        self.consume(TokenKind::KW_LOOP, "expected the keyword 'loop'")?;
        let loop_body: AST = self.parse_compound_stmt()?;
        Some(AST::new(
            ASTKind::StmtAST(Stmt::Loop),
            ASTOperation::AST_LOOP,
            Some(loop_body),
            None,
            None,
        ))
    }

    fn parse_break_stmt(&mut self) -> ParseOutput<'tcx> {
        self.consume(TokenKind::KW_BREAK, "expected the keyword 'break'")?;
        Some(
            AST::new(
                ASTKind::StmtAST(Stmt::Break),
                ASTOperation::AST_BREAK,
                None,
                None,
                None
            )
        )
    }

    fn parse_for_stmt(&mut self) -> ParseOutput<'tcx> {
        assert!(self.peek().kind == TokenKind::KW_FOR, "cannot parse a for statement");
        self.consume(TokenKind::KW_FOR, "'for' expected")?;

        let id_ast = self.parse_identifier()?;
        self.consume(TokenKind::KW_IN, "'in' expected")?;

        let expr_ast = self.parse_record_or_expr(None)?;
        let body_ast = self.parse_compound_stmt()?;
        Some(AST::with_mid(
            ASTKind::StmtAST(Stmt::For),
            ASTOperation::AST_FOR,
            Some(id_ast),
            Some(expr_ast),
            Some(body_ast),
            None
        ))
    }

    fn parse_if_stmt(&mut self) -> ParseOutput<'tcx> {
        let cond_ast = self.parse_conditional_stmt(TokenKind::KW_IF)?;
        let if_true_ast = self.parse_single_stmt()?;

        let mut if_false_ast = None;
        if self.peek().kind == TokenKind::KW_ELSE {
            self.advance(); // skip 'else'

            let else_block = self.parse_compound_stmt()?;
            if_false_ast = Some(
                AST::new(
                    ASTKind::StmtAST(Stmt::Scoping),
                    ASTOperation::AST_ELSE,
                    Some(else_block),
                    None,
                    None
                )
            );          
        }
        Some(AST::with_mid(
            ASTKind::StmtAST(Stmt::If),
            ASTOperation::AST_IF,
            Some(cond_ast),
            Some(if_true_ast),
            if_false_ast,
            None,
        ))
    }

    // parses tokens that are in the form '(expression [< | > | >= | <= | == | !=] expression)'
    fn parse_conditional_stmt(&mut self, kind: TokenKind) -> ParseOutput<'tcx> {
        _ = self.consume(kind, &format!("'{k}' expected ", k = kind.as_str()))?;
        _ = self.consume(TokenKind::T_LPAREN, "'(' expected")?; // match and ignore '('

        let cond_ast = self.parse_equality();

        if let Some(cond_ast) = &cond_ast {
            if (cond_ast.operation < ASTOperation::AST_EQEQ) || (cond_ast.operation > ASTOperation::AST_LTHAN) {
                // if operation kind is not "relational operation"
                panic!("'{:?}' is not allowed in {kind:?}'s condition.", cond_ast.operation);
            }
        }
        _ = self.consume(TokenKind::T_RPAREN, "')' expected")?; // match and ignore ')'
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
    fn parse_var_decl_stmt(&mut self) -> ParseOutput<'tcx> {
        self.consume(TokenKind::KW_LET, "'let' expected'")?;

        // Being "inside" a function means that we are currently parsing a function's body.
        let inside_func: bool = true; // self.scope.live_scope_id().0 != 0;

        // Track the storage class for this variable.
        let mut var_class: StorageClass = StorageClass::GLOBAL;

        if inside_func {
            var_class = StorageClass::LOCAL;
        }

        // Track the type of this variable.
        //
        // The variable might not have any initial value.
        // Thus it is 'null' (or none) by default.
        let mut var_type = TyKind::None;

        // symbol's type
        let mut sym_type = SymTy::Variable;

        // Name of the variable.
        let id_token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?;

        // Parser may encounter a colon after the identifier name.
        // This means the type of this variable has been defined
        // by the user.
        if self.peek().kind == TokenKind::T_COLON {
            _ = self.consume(TokenKind::T_COLON, "':' expected")?;
            var_type = self.parse_id_type()?;

            // if the declared variable is a record
            if let TyKind::Record { name: rec_name } = &var_type {
                sym_type = SymTy::Record { name: rec_name };
            }
            else {
                self.advance();
            }
        }

        // Stores the RHS value of this variable (if defined)
        let mut assignment_parse_res: Option<ParseOutput> = None;

        // Checking whether variable is assigned at the time of its declaration.
        // If identifier name is followed by an equal sign, then it is assigned 
        // at the time of declaration.
        if self.peek().kind == TokenKind::T_EQUAL {
            _ = self.consume(TokenKind::T_EQUAL, "'=' expected")?; // match and ignore '=' sign
            assignment_parse_res = Some(self.parse_record_or_expr(Some(id_token.lexeme)));
        }

        // if there is some error during expression parsing
        if let Some(None) = assignment_parse_res {
            return None;
        } 
        else if let Some(Some(expr_ast)) = &assignment_parse_res {
            if let ASTKind::ExprAST(Expr::RecordCreation(record_create)) = &expr_ast.kind {
                sym_type = SymTy::Record { name: record_create.name }
            }
        }

        let local_off = if inside_func {
            if let SymTy::Record { .. } = sym_type {
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
            self.expect_semicolon();
            Some(AST::new(
                ASTKind::StmtAST(Stmt::VarDecl(VarDeclStmt {
                    symtbl_pos: 0xFFFFFFFF, // this value will be set by the resolver
                    symbol_type: sym_type,
                    class: var_class,
                    sym_name: id_token.lexeme,
                    ty: var_type,
                    local_offset: local_off,
                    func_id: if inside_func { self.current_function_id } else { 0xFFFFFFFF },
                })),
                ASTOperation::AST_VAR_DECL,
                Some(assign_ast_node_res?),
                None,
                Some(var_type),
            ))
        } 
        else {
            panic!("Variable declared without any assignment!")
        }
    }

    fn _next_str_label(&mut self) -> usize {
        let l = self._str_label_;
        self._str_label_ += 1;
        l
    }

    /// Parses the current token as a literal type keyword and returns the 
    /// corresponding `LitTypeVariant`.
    ///
    /// Returns an error if the token does not represent a valid data type keyword.
    fn parse_id_type(&mut self) -> Option<TyKind<'tcx>> {
        let current_tok: TokenKind = self.peek().kind;
        match current_tok {
            TokenKind::KW_LONG |
            TokenKind::KW_INT => Some(TyKind::I64),
            TokenKind::KW_CHAR => Some(TyKind::U8),
            TokenKind::KW_STR => Some(TyKind::Str),
            TokenKind::KW_VOID => Some(TyKind::Void),
            TokenKind::KW_NULL => Some(TyKind::Null),
            TokenKind::T_IDENTIFIER => Some(TyKind::Record{ name: self.peek().lexeme }),
            _ => {
                self.report_unexpected_token();
                None
            }
        }
    }

    // TODO: Write comments
    fn assign_stmt_or_func_call(&mut self) -> ParseOutput<'tcx> {
        let id_token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?;
        let tok_kind_after_id_tok: TokenKind = self.peek().kind;
        if tok_kind_after_id_tok != TokenKind::T_LPAREN {
            panic!("Cannot parse an assignment statement right now!");
        } else {
            self.parse_func_call_expr(id_token.lexeme, &id_token)
        }
    }

    fn parse_record_or_expr(&mut self, rec_alias: Option<&'tcx str>) -> ParseOutput<'tcx> {
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

    fn parse_record_creation(&mut self, rec_alias: &'tcx str) -> ParseOutput<'tcx> {
        let span_start = self.peek().pos;

        let id_token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?;
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
                    self.report_unexpected_token();
                    return None;
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

        Some(
            AST::create_leaf(
                ASTKind::ExprAST(
                    Expr::RecordCreation(
                        RecordCreationExpr { 
                            name: id_token.lexeme, 
                            fields,
                            pool_idx: 0xFFFFFFFF, // this value will be set by the Resolver
                            rec_alias,
                        }
                    )
                ),
                ASTOperation::AST_RECORD_CREATE, 
                None,
                meta
            )
        )
    }

    fn parse_record_field_assignment(&mut self, field_off: usize) -> Option<RecordFieldAssignExpr<'tcx>> {
        let id_token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?;
        _ = self.consume(TokenKind::T_EQUAL, "'=' expected"); // parse '='
        let field_val = self.parse_record_or_expr(None)?;

        if let ASTKind::ExprAST(expr) = field_val.kind {
            return Some(
                RecordFieldAssignExpr { 
                    name: id_token.lexeme, 
                    value: Box::new(expr),
                    offset: field_off
                }
            );
        }
        panic!()
    }

    fn parse_equality(&mut self) -> ParseOutput<'tcx> {
        let left = self.parse_comparision()?;
        self.try_parsing_binary(left, vec![TokenKind::T_EQEQ, TokenKind::T_NEQ])
    }

    fn parse_comparision(&mut self) -> ParseOutput<'tcx> {
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

    fn parse_addition(&mut self) -> ParseOutput<'tcx> {
        let left = self.parse_factor()?;
        self.try_parsing_binary(left, vec![TokenKind::T_PLUS, TokenKind::T_MINUS])
    }

    fn parse_factor(&mut self) -> ParseOutput<'tcx> {
        let left = self.parse_primary()?;
        self.try_parsing_binary(left, vec![TokenKind::T_SLASH, TokenKind::T_STAR])
    }

    fn try_parsing_binary(&mut self, left: AST<'tcx>, tokens: Vec<TokenKind>) -> ParseOutput<'tcx> {
        let current_token_kind = self.peek().kind;
        if !tokens.contains(&current_token_kind) {
            return Some(left);
        }

        // start of the expression             
        let span_start = left.meta.span;

        _ = self.advance(); // skip the operator

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

        Some(
            AST::create_leaf(
                ASTKind::ExprAST(
                    Expr::Binary(BinExpr {
                        operation: ast_op,
                        left: Box::new(left_expr),
                        right: Box::new(right_expr),
                        ty: TyKind::None,
                    })
                ),
                ast_op,
                None,
                NodeMeta::new(
                    combined_span,
                    vec![]
                )
            )
        )
    }

    fn parse_primary(&mut self) -> ParseOutput<'tcx> {
        let file_id = self.current_file.0;
        let current_token = self.advance();

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
                Some(
                    Parser::create_expr_ast(
                        LitValue::I32(current_token.lexeme.parse::<i32>().unwrap()),
                        ASTOperation::AST_INTLIT,
                        single_token_meta
                    )
                )
            },
            TokenKind::T_CHAR => {
                Some(
                    Parser::create_expr_ast(
                        LitValue::U8(current_token.lexeme.parse::<u8>().unwrap()),
                        ASTOperation::AST_INTLIT,
                        single_token_meta
                    )
                )
            },
            TokenKind::T_LONG_NUM => {
                Some(
                    Parser::create_expr_ast(
                        LitValue::I64(current_token.lexeme.parse::<i64>().unwrap()),
                        ASTOperation::AST_INTLIT,
                        single_token_meta
                    )
                )
            },
            TokenKind::T_FLOAT_NUM | TokenKind::T_DOUBLE_NUM => {
               Some(
                    Parser::create_expr_ast(
                        LitValue::F64(current_token.lexeme.parse::<f64>().unwrap()),
                        ASTOperation::AST_INTLIT,
                        single_token_meta
                    )
                ) 
            },
            TokenKind::T_STRING => { 
                Some(AST::create_leaf(
                    ASTKind::ExprAST(
                        Expr::LitVal(
                            LitValExpr {
                                value: LitValue::RawStr(current_token.lexeme),
                                ty: TyKind::Str,
                            }
                        )
                    ),
                    ASTOperation::AST_STRLIT,
                    Some(TyKind::Str),
                    single_token_meta
                ))
            }
            TokenKind::T_IDENTIFIER => {
                // Identifiers in a global variable declaration expression are not allowed.
                if current_token.kind == TokenKind::T_NONE {
                    self.diagnostics.push(Diagnostic::from_single_token(
                        &self.peek(), 
                        self.current_file, 
                        "initializer not a constant",
                        Severity::Error
                    ));
                    return None;
                }
                
                let symbol_name = current_token.lexeme;
                let curr_tok_kind: TokenKind = self.peek().kind;

                if curr_tok_kind == TokenKind::T_LPAREN {
                    self.parse_func_call_expr(symbol_name, &current_token)
                } 
                else if curr_tok_kind == TokenKind::T_DOT {
                    self.parse_record_field_access_expr(symbol_name, &current_token)
                }
                else {
                    Some(AST::create_leaf(
                        ASTKind::ExprAST(
                            Expr::Ident(IdentExpr {
                                ty: TyKind::None, // We don't care about the type of this symbol, yet!
                                sym_name: current_token.lexeme
                            }
                        )),
                        ASTOperation::AST_IDENT,
                        None, // type will be identified at the semantic analysis phases if the symbol is defined
                        single_token_meta
                    ))
                }
            }
            TokenKind::T_LPAREN => {
                // group expression: e.g: (a * (b + c)))
                let group_expr = self.parse_record_or_expr(None)?;
                // Group expression terminates with ')'. Match and ignore ')'.
                let _ = self.consume(TokenKind::T_RPAREN, "expected a ')'")?;
                Some(group_expr)
            },

            // null type
            TokenKind::KW_NULL => {
                Some(AST::create_leaf(
                    ASTKind::ExprAST(Expr::Null), 
                    ASTOperation::AST_NULL, 
                    Some(TyKind::Null), 
                    single_token_meta
                ))
            },
            _ => {
                self.report_unexpected_token();
                None
            }
        }
    }

    fn parse_identifier(&mut self) -> ParseOutput<'tcx> {
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
        Some(AST::create_leaf(
            ASTKind::ExprAST(
                Expr::Ident(
                    IdentExpr { 
                        sym_name: id.lexeme, 
                        ty: TyKind::None 
                    }
                )
            ),
            ASTOperation::AST_IDENT,
            None,
            meta
        ))
    }

    fn create_expr_ast(value: LitValue<'tcx>, operation: ASTOperation, meta: NodeMeta) -> AST<'tcx> {
        AST::create_leaf(
            ASTKind::ExprAST(
                Expr::LitVal(
                    LitValExpr {
                        value: value.clone(),
                        ty: TyKind::None, // SHOULD NOT BE NONE
                    }
                )
            ),
            operation,
            None,
            meta
        )
    }

    fn parse_record_field_access_expr(&mut self, rec_alias: &'tcx str, start_token: &Token) -> ParseOutput<'tcx> {
        let mut field_chain = vec![];
        let mut end_pos = SourcePos {
            line: start_token.pos.line,
            column: start_token.pos.column
        };

        while self.peek().kind == TokenKind::T_DOT {
            _ = self.consume(TokenKind::T_DOT, "'.' expected");
            let access = self.consume(TokenKind::T_IDENTIFIER, "expected an identifer")?;
            end_pos.line = access.pos.line;
            end_pos.column = access.pos.column;
            field_chain.push(access.lexeme);
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

        Some(
            AST::create_leaf(
                ASTKind::ExprAST(
                    Expr::RecordFieldAccess(
                        RecordFieldAccessExpr { 
                            rec_name: "", // name will be set by the semantic analyser
                            rec_alias, 
                            field_chain,
                            rel_stack_off: 0xFFFFFFFF, // resolver resolves this
                            ty: TyKind::None // will be determined by the semantic analyzer
                        }
                    )
                ),
                ASTOperation::AST_RECORD_FIELD_ACCESS, 
                None, 
                meta
            )
        )
    }

    fn parse_func_call_expr(&mut self, called_symbol: &'tcx str, start_token: &Token) -> ParseOutput<'tcx> {
        _ = self.consume(TokenKind::T_LPAREN, "'(' expected")?;

        let curr_token_kind: TokenKind = self.peek().kind;
        let mut func_args: Vec<FuncArg> = vec![];

        if curr_token_kind != TokenKind::T_RPAREN {
            let mut arg_pos: usize = 0;

            loop {
                let argu = self.parse_record_or_expr(None)?;
                func_args.push((arg_pos, argu.kind.expr().unwrap()));

                let is_tok_comma: bool = self.peek().kind == TokenKind::T_COMMA;
                let is_tok_rparen: bool = self.peek().kind == TokenKind::T_RPAREN;

                if !is_tok_comma && !is_tok_rparen {
                    self.report_unexpected_token();
                    return None;
                } 
                else if is_tok_rparen {
                    break;
                } 
                else {
                    _ = self.consume(TokenKind::T_COMMA, "',' expected")?;
                }
                arg_pos += 1;
            }
        }

        let end_token = self.consume(TokenKind::T_RPAREN, "')' expected")?;

        let start_pos = SourcePos {
            line: start_token.pos.line,
            column: start_token.pos.column
        };
        let end_pos = SourcePos {
            line: end_token.pos.line,
            column: end_token.pos.column
        };

        Some(AST::create_leaf(
            ASTKind::ExprAST(
                Expr::FuncCall(
                    FuncCallExpr {
                        ty: TyKind::None,
                        symbol_name: called_symbol,
                        args: func_args,
                        id: FuncId(INVALID_FUNC_ID) // resolver sets this value
                    }
                )
            ),
            ASTOperation::AST_FUNC_CALL,
            None,
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

    fn report_unexpected_token(&mut self) {
        self.diagnostics.push(
            Diagnostic::from_single_token(
                &self.peek(), 
                self.current_file, 
                "unexpected token",
                Severity::Error
            )
        );
    }

    /// Look at the current token without consuming it.
    fn peek(&self) -> Token<'tcx> {
        *self.tokens.get(self.current).unwrap_or(self.tokens.last().unwrap())
    }

    /// Look ahead N tokens
    fn look_ahead(&self, distance: usize) -> Token<'tcx> {
        *self.tokens.get(self.current + distance).unwrap_or_else(|| self.tokens.last().unwrap())
    }

    /// Check if the current token matches a kind
    fn check(&self, kind: TokenKind) -> bool {
        self.peek().kind == kind
    }

    fn consume(&mut self, kind: TokenKind, msg: &str) -> Option<Token<'tcx>> {
        let tok = self.peek();
        if tok.kind == kind {
            if kind == TokenKind::T_EOF && self.current != self.tokens.len() - 1 {
                unreachable!("EOF not at end of token stream");
            }
            Some(self.advance())
        } else {
            self.diagnostics.push(Diagnostic::from_single_token(
                &tok,
                self.current_file,
                msg,
                Severity::Error,
            ));
            None
        }
    }

    fn advance(&mut self) -> Token<'tcx> {
        if self.current < self.tokens.len() {
            let tok = self.tokens[self.current];
            self.current += 1;
            tok
        } else {
            *self.tokens.last().unwrap()
        }
    }
}

#[cfg(test)]
mod tests {
    use kagc_errors::diagnostic::DiagnosticBag;
    use kagc_lexer::Tokenizer;
    use kagc_token::{Token, TokenKind};
    use kagc_ctx::StringInterner;

    use crate::{Parser, options::ParserOptions};

    fn mk_parser<'p, 'tcx>(tokens: Vec<Token<'tcx>>, diags: &'p DiagnosticBag) -> Parser<'p, 'tcx> {
        Parser::new(
            ParserOptions {  },
            diags,
            tokens
        )
    }

    #[test]
    fn test_token_advance_logic_holds_correct() {
        let str_arena = typed_arena::Arena::<String>::new();
        let diag_bag = DiagnosticBag::default();
        let str_intern = StringInterner::new(&str_arena);
        let mut lexer = Tokenizer::new(
            &diag_bag,
            &str_intern
        );
        let tokens = lexer.tokenize("let a = 12 + 12;");
        let mut parser = mk_parser(tokens, &diag_bag);
        
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
        let str_arena = typed_arena::Arena::<String>::new();
        let diag_bag = DiagnosticBag::default();
        let str_intern = StringInterner::new(&str_arena);
        let mut lexer = Tokenizer::new(
            &diag_bag,
            &str_intern
        );
        let tokens = lexer.tokenize("let a = 12 + 12;");
        let mut parser = mk_parser(tokens, &diag_bag);

        while parser.advance().kind != TokenKind::T_EOF {}

        assert_eq!(parser.advance().kind, TokenKind::T_EOF);
        assert_eq!(parser.advance().kind, TokenKind::T_EOF);
    }
}