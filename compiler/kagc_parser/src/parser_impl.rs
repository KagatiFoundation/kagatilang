// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_ast::record::*;
use kagc_ast::*;
use kagc_comp_unit::source_map::FileId;
use kagc_errors::code::ErrCode;
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
use kagc_ast::Literal;

use crate::options::ParserOptions;

pub(crate) type ParseOutput<'tcx> = Option<AstNode<'tcx>>;

/// Represents an invalid function ID.
///
/// This constant is used to indicate that a function ID is not valid or
/// not set, serving as a sentinel value during parsing and code generation
/// to detect error states and invalid contexts.
const INVALID_ID: usize = 0xFFFFFFFF;

pub type StringLabel = usize;

/// Represents a parser for converting tokens into abstract syntax trees (ASTs).
pub struct Parser<'p, 'tcx> where 'tcx: 'p {
    /// Tokens that are going to be parsed.
    tokens: Vec<Token<'tcx>>,

    /// Counter which points to the current token index.
    current: usize,

    current_file: FileId,

    pub diagnostics: &'p DiagnosticBag,
    pub options: ParserOptions,
    next_node_id: usize,
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
            current_file: options.file_id,
            options,
            diagnostics: diags,
            next_node_id: 0 // start counting at zero
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

    pub fn parse(&mut self) -> Vec<AstNode<'tcx>> {
        if self.tokens.is_empty() {
            return Vec::with_capacity(0);
        }

        let mut nodes: Vec<AstNode> = vec![];

        loop {
            let peek_kind = self.peek().kind;
            if peek_kind == TokenKind::T_EOF {
                break;
            }

            match self.parse_top_level_decl() {
                Some(stmt) => nodes.push(stmt),
                None => break
            }
        }

        nodes
    }

	fn parse_top_level_decl(&mut self) -> ParseOutput<'tcx> {
		match self.peek().kind {
			TokenKind::KW_IMPORT => self.parse_import_stmt(),
			TokenKind::KW_DEF => self.parse_function_stmt(),
			TokenKind::KW_RECORD => self.parse_record_decl_stmt(),

			_ => {
				self.report_unexpected_token(self.peek());
				None
			}
		}
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
            TokenKind::KW_CONTINUE => self.parse_continue_stmt(),
            TokenKind::T_IDENTIFIER => self.parse_assign_stmt_or_func_call(),
            TokenKind::KW_IF => self.parse_if_stmt(),
            TokenKind::KW_WHILE => self.parse_while_stmt(),
            TokenKind::KW_FOR => self.parse_for_stmt(),
            TokenKind::T_LBRACE => self.parse_block_stmt(),
            TokenKind::KW_LOOP => self.parse_loop_stmt(),
            _ => {
                self.report_unexpected_token(self.peek());
                None
            }
        }
    }

	fn synchronize_statement(&mut self) {
		while self.peek().kind != TokenKind::T_EOF {
			match self.peek().kind {
				TokenKind::T_SEMICOLON => {
					self.advance();
					return;
				}

				TokenKind::T_RBRACE => {
					return;
				}

				TokenKind::KW_LET
				| TokenKind::KW_RETURN
				| TokenKind::KW_BREAK
				| TokenKind::KW_CONTINUE
				| TokenKind::KW_FOR
				| TokenKind::KW_WHILE
				| TokenKind::KW_IF
				| TokenKind::KW_LOOP => {
					return;
				}

				_ => {
					self.advance();
				}
			}
		}
	}

    fn expect_semicolon(&mut self) {
		if self.peek().kind == TokenKind::T_SEMICOLON {
			self.advance();
		}
		else {
			let previous = self
				.previous()
				.unwrap_or_else(|| panic!("a semicolon is expected at the very beginning"));

			let error = Diagnostic::missing_token(
			    &previous,
    			"expected ';' after this token",
    			Severity::Error
			);

			self.diagnostics.push(error);
		}
    }

    // parse a block statement(statement starting with '{' and ending with '}')
    fn parse_block_stmt(&mut self) -> ParseOutput<'tcx> {
        let open_brace = self.consume(TokenKind::T_LBRACE, "'{' expected")?; // parse and ignore '{'

        let mut statements = vec![];

        loop {
			match self.peek().kind {
				TokenKind::T_RBRACE => {
                	self.consume(TokenKind::T_RBRACE, "'}' expected")?;
                	break;
				},

				TokenKind::T_EOF => {
					let missing_closing_brace_err = Diagnostic::from_single_token(
						&open_brace, 
						"unclosed block", 
						Severity::Error
					);

					self.diagnostics.push(missing_closing_brace_err);
					return None;
				},

				_ => {
					if let Some(statement) = self.parse_single_stmt() {
						statements.push(statement);
					} else {
						return None;
					}
				}
			}
        }

        if statements.is_empty() {
            return Some(AstNode::empty());
        }

        let block_stmt_ast = Stmt::Block(BlockStmt { statements });

        Some(
            AstNode::leaf(
                self.next_node_id(),
                NodeKind::StmtAST(block_stmt_ast),
                AstOp::Block,
                None,
                NodeMeta::none()
            )
        )
    }

    fn parse_import_stmt(&mut self) -> ParseOutput<'tcx> {
        let start_tok = self.consume(TokenKind::KW_IMPORT, "'import' expected")?;

        let module_path_tok: Token = self.consume(TokenKind::T_STRING, "expected a string")?;

        let end_tok = self.consume(TokenKind::T_SEMICOLON, "';' expected")?;

        let meta = NodeMeta::new(
            start_tok.span.covering(&end_tok.span),
            vec![]
        );

        Some(
            AstNode::leaf(
                self.next_node_id(),
                NodeKind::StmtAST(
                    Stmt::Import(
                        ImportStmt { path: module_path_tok.lexeme }
                    )
                ), 
                AstOp::Import,
                None,
                meta
            )
        )
    }

    fn parse_record_decl_stmt(&mut self) -> ParseOutput<'tcx> {
        let start_tok = self.consume(TokenKind::KW_RECORD, "'record' expected")?;

        // expect name of the record
        let id_token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?;

        _ = self.consume(TokenKind::T_LBRACE, "'{' expected");

        let mut rec_fields = vec![];

        while self.peek().kind != TokenKind::T_RBRACE {
            rec_fields.push(self.parse_record_field_decl_stmt()?);
        }
        
        let end_tok = self.consume(TokenKind::T_RBRACE, "'}' expected")?;

        let meta = NodeMeta::new(
            start_tok.span.covering(&end_tok.span),
            vec![]
        );

        Some(
            AstNode::leaf(
                self.next_node_id(),
                NodeKind::StmtAST(
                    Stmt::Record(
                        RecordDeclStmt { 
                            name: id_token.lexeme,
                            size: 0,
                            alignment: 0,
                            fields: rec_fields.into_iter().enumerate().map(|(idx, field)| {
                                RecordFieldType {
                                    name: field.name,
                                    ty: field.typ,
                                    rel_stack_off: idx as i64
                                }
                            }).collect::<Vec<RecordFieldType>>() 
                        }
                    )
                ), 
                AstOp::RecDecl,
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

        _ = self.consume(TokenKind::T_LPAREN, "'(' expected")?;

        let mut func_param_types = vec![];
        let mut func_params = vec![];

        if self.peek().kind != TokenKind::T_RPAREN {
            loop {
                if let Some(param) = self.parse_parameter() {
                    func_param_types.push(param.ty);
                    func_params.push(param);
                }

                let is_tok_comma: bool = self.peek().kind == TokenKind::T_COMMA;
                let is_tok_rparen: bool = self.peek().kind == TokenKind::T_RPAREN;

                if !is_tok_comma && !is_tok_rparen {
                    self.report_unexpected_token(self.peek());
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

        let mut function_body: Option<Box<AstNode>> = None;

        // create function body
        if func_storage_class != StorageClass::EXTERN {
            let function_body_res = self.parse_block_stmt()?;
            function_body = Some(Box::new(function_body_res));
        } 
        else {
            self.expect_semicolon();
        }

        // Return AST for function declaration
        Some(
			AstNode {
				id: self.next_node_id(),
				kind: NodeKind::StmtAST(
					Stmt::FuncDecl(
						FuncDeclStmt {
							id: None,
							name: id_token.lexeme,
							ty: func_return_type,
							storage_class: func_storage_class,
							params: func_params,
							param_types: func_param_types
						}
					)
				),
				op: AstOp::Func,
				left: function_body,
				mid: None,
				right: None,
				ty: Some(func_return_type),
				meta: NodeMeta::new(
					id_token.span, 
					Vec::with_capacity(0)
				)
			}
		)
    }

    // parse function's return type
    fn parse_fn_ret_type(&mut self) -> Option<TyKind<'tcx>> {
        _ = self.consume(TokenKind::T_ARROW, "'->' expected")?;
        let func_ret_type = self.parse_id_type()?;

        if func_ret_type == TyKind::Null {
            self.diagnostics.push(
                Diagnostic::from_single_token(
                    &self.peek(), 
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

        self.consume(TokenKind::T_COLON, "':' expected")?;
  
        let param_type = self.parse_id_type()?;
  
        self.advance();
  
        Some(
            FuncParam {
                ty: param_type,
                name: param_name.lexeme,
                offset: -1
            }
        )
    }

    fn parse_return_stmt(&mut self) -> ParseOutput<'tcx> {
        let ret_tok = self.consume(TokenKind::KW_RETURN, "'return' expected")?;

        if self.peek().kind == TokenKind::T_SEMICOLON {
            self.expect_semicolon();

            let meta = NodeMeta::new(
                ret_tok.span,
                vec![]
            );

            let ret_ast = Some(
                AstNode::leaf(
                    self.next_node_id(),
                    NodeKind::StmtAST(
                        Stmt::Return(
                            ReturnStmt {
                                func_id: FuncId::invalid(),
                            }
                        )
                    ),
                    AstOp::Return,
                    None,
                    meta
                )
            );
			ret_ast
        }
        else {
            let return_expr = self.parse_record_or_expr(None);

			if return_expr.is_none() {
				self.synchronize_statement();

				return None;
			}

			let return_expr = return_expr.unwrap();

            let meta = NodeMeta::new(
                ret_tok.span.covering(&return_expr.meta.span),
                vec![]
            );

            let return_ast = AstNode {
                kind: NodeKind::StmtAST(
                    Stmt::Return(
                        ReturnStmt {
                            func_id: FuncId(INVALID_ID),
                        }
                    )
                ),
                left: Some(Box::new(return_expr)),
                right: None,
                mid: None,
                op: AstOp::Return,
                ty: None,
                meta,
                id: self.next_node_id()
            };

            self.expect_semicolon();

            Some(return_ast)
        }
    }

    fn parse_while_stmt(&mut self) -> ParseOutput<'tcx> {
        let cond_ast = self.parse_conditional_stmt(TokenKind::KW_WHILE)?;
        let while_body = self.parse_single_stmt()?;
        Some(
            AstNode::binary(
                self.next_node_id(),
                NodeKind::StmtAST(Stmt::While),
                AstOp::While,
                Some(cond_ast),
                Some(while_body),
                None
            )
        )
    }

    fn parse_loop_stmt(&mut self) -> ParseOutput<'tcx> {
		self.consume(TokenKind::KW_LOOP, "'loop' expected")?;

		let next_token = self.peek();

		let loop_condition_expr = if next_token.kind == TokenKind::KW_IF { // the 'loop' statement has a condition
			self.consume(TokenKind::KW_IF, "'if' expected")?;
			self.parse_equality()
		}
		else { None };

        let loop_body: AstNode = self.parse_block_stmt()?;
        Some(AstNode::binary(
            self.next_node_id(),
            NodeKind::StmtAST(Stmt::Loop),
            AstOp::Loop,
            Some(loop_body),
            loop_condition_expr,
            None,
        ))
    }

    fn parse_break_stmt(&mut self) -> ParseOutput<'tcx> {
        self.consume(TokenKind::KW_BREAK, "'break' expected")?;
        self.expect_semicolon();
        Some(
            AstNode::binary(
                self.next_node_id(),
                NodeKind::StmtAST(Stmt::Break),
                AstOp::Break,
                None,
                None,
                None
            )
        )
    }

    fn parse_continue_stmt(&mut self) -> ParseOutput<'tcx> {
        self.consume(TokenKind::KW_CONTINUE, "'continue' expected")?;
        self.expect_semicolon();
        Some(
            AstNode::binary(
                self.next_node_id(),
                NodeKind::StmtAST(Stmt::Continue),
                AstOp::Break,
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
        let body_ast = self.parse_block_stmt()?;
        Some(
            AstNode::ternary(
                self.next_node_id(),
                NodeKind::StmtAST(Stmt::For),
                AstOp::For,
                Some(id_ast),
                Some(expr_ast),
                Some(body_ast),
                None
            )
        )
    }

    fn parse_if_stmt(&mut self) -> ParseOutput<'tcx> {
        let cond_ast = self.parse_conditional_stmt(TokenKind::KW_IF)?;
        let if_true_ast = self.parse_single_stmt()?;

        let if_false_ast = if self.peek().kind == TokenKind::KW_ELSE {
            self.advance(); // skip 'else'

            self.parse_block_stmt()
        }
		else { None };

        Some(
            AstNode::ternary(
                self.next_node_id(),
                NodeKind::StmtAST(Stmt::If),
                AstOp::If,
                Some(cond_ast),
                Some(if_true_ast),
                if_false_ast,
                None
            )
        )
    }

    fn parse_conditional_stmt(&mut self, kind: TokenKind) -> ParseOutput<'tcx> {
        self.consume(kind, &format!("'{k}' expected ", k = kind.as_str()))?;

        self.consume(TokenKind::T_LPAREN, "'(' expected")?;

        let cond_ast = self.parse_equality()?;

        if (cond_ast.op < AstOp::EqEq) || (cond_ast.op > AstOp::LThan) {
			let diag = Diagnostic {
				code: Some(ErrCode::InvalidSyntax),
				severity: Severity::Error,
				primary_span: *kagc_span::span::HasSpan::span(&cond_ast),
				secondary_spans: vec![],
				message: "invalid values for a relational operator".to_string(),
				notes: vec![]
			};

			self.diagnostics.push(diag);
        }
			  
       	self.consume(TokenKind::T_RPAREN, "')' expected")?;

        Some(cond_ast)
    }

    /// Parses a variable declaration statement.
    fn parse_var_decl_stmt(&mut self) -> ParseOutput<'tcx> {
        self.consume(TokenKind::KW_LET, "'let' expected'")?;

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

        self.consume(TokenKind::T_EQUAL, "'=' expected");

        // Stores the RHS value of this variable (if defined)
        let assigned_value = self.parse_record_or_expr(Some(id_token.lexeme))?;

        if let NodeKind::ExprAST(Expr::RecordCreation(record_create)) = &assigned_value.kind {
            sym_type = SymTy::Record { name: record_create.name }
        }

        // self.consume(TokenKind::T_SEMICOLON, "';' expected after variable declaration");
		self.expect_semicolon();

        Some(
			AstNode::binary(
				self.next_node_id(),
				NodeKind::StmtAST(
					Stmt::VarDecl(
						VarDeclStmt {
							symtbl_pos: INVALID_ID,
							symbol_type: sym_type,
							sym_name: id_token.lexeme,
							ty: var_type
						}
					)
				),
				AstOp::VarDecl,
				Some(assigned_value),
				None,
				Some(var_type),
			)
		)
    }

	fn parse_assignment_stmt(&mut self) -> ParseOutput<'tcx> {
		let id_token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?;
		
		self.consume(TokenKind::T_EQUAL, "expected '='")?; // parse and ignore '='

		let assignment_expr = self.parse_record_or_expr(Some(id_token.lexeme))?;

		Some(
			AstNode::binary(
				self.next_node_id(), 
				NodeKind::StmtAST(
					Stmt::Assignment(
						AssignStmt {
							sym_name: id_token.lexeme
						}
					)
				), 
				AstOp::Assign,
				Some(assignment_expr),
				None,
				None
			)
		)
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
                self.report_unexpected_token(self.peek());
                None
            }
        }
    }

    // TODO: Write comments
    fn parse_assign_stmt_or_func_call(&mut self) -> ParseOutput<'tcx> {
		if self.peek().kind != TokenKind::T_IDENTIFIER {
			self.report_unexpected_token(self.peek());
			return None;
		}

        if self.look_ahead(1).kind == TokenKind::T_EQUAL { // '=' means that we are delaing with an assignment statement
			let stmt = self.parse_assignment_stmt();
			self.expect_semicolon();
			stmt
        }
		else {
        	let id_token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?;
            let stmt = self.parse_func_call_expr(id_token.lexeme, &id_token);
            self.expect_semicolon();
            stmt
        }
    }

    fn parse_record_or_expr(&mut self, rec_alias: Option<&'tcx str>) -> ParseOutput<'tcx> {
        // peek ahead to see if this is a Record initialization: i.e. Identifier followed by a { ...
        if self.check(TokenKind::T_IDENTIFIER) && self.look_ahead(1).kind == TokenKind::T_LBRACE {
            if let Some(ra) = rec_alias {
                return self.parse_record_creation(ra);
            }
			else {
                unreachable!();
            }
        }
        self.parse_equality()
    }

    fn parse_record_creation(&mut self, rec_alias: &'tcx str) -> ParseOutput<'tcx> {
        let span_start = self.peek().span.start;

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
                    self.report_unexpected_token(self.peek());
                    return None;
                }
            }

            // make space for next field
            field_off += 1;
        }

        let span_end = self.tokens[self.current - 1].span.end;
        let meta = NodeMeta::new(
            Span::new(
                self.current_file.0, 
                span_start, 
                span_end
            ),
            vec![]
        );

        Some(
            AstNode::leaf(
                self.next_node_id(),
                NodeKind::ExprAST(
                    Expr::RecordCreation(
                        RecordCreationExpr { 
                            name: id_token.lexeme, 
                            fields,
                            pool_idx: INVALID_ID, // this value will be set by the Resolver
                            rec_alias,
                        }
                    )
                ),
                AstOp::RecCreate, 
                None,
                meta
            )
        )
    }

    fn parse_record_field_assignment(&mut self, field_off: usize) -> Option<RecordFieldAssignExpr<'tcx>> {
        let id_token = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?;
        _ = self.consume(TokenKind::T_EQUAL, "'=' expected"); // parse '='
        let field_val = self.parse_record_or_expr(None)?;

        if let NodeKind::ExprAST(expr) = field_val.kind {
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
        let left = self.parse_unary()?;
        self.try_parsing_binary(left, vec![TokenKind::T_SLASH, TokenKind::T_STAR])
    }

	fn parse_unary(&mut self) -> ParseOutput<'tcx> {
		if let Some(unary_sign) = self.parse_unary_sign() {
			match unary_sign {
				UnaryOp::Neg => {
					let span_start = self.peek().span;

					self.advance();

					let expr = self.parse_unary()?;

					let span_end = expr.meta.span;

					let combined_span = span_start.covering(&span_end);

					return Some(
						AstNode::leaf(
							self.next_node_id(),
							NodeKind::ExprAST(
								Expr::Unary(
									UnaryExpr {
										op: UnaryOp::Neg,
										expr: Box::new(expr.kind.expr().unwrap()),
										ty: TyKind::None,
									}
								)
							),
							AstOp::Unary,
							None,
							NodeMeta::new(combined_span, vec![])
						)
					);
				},
				UnaryOp::Not => todo!(),
				UnaryOp::BitNot => todo!(),
			}
		}

		self.parse_primary()
	}

	fn parse_unary_sign(&mut self) -> Option<UnaryOp> {
		if self.check(TokenKind::T_MINUS) {
			Some(UnaryOp::Neg)
		}
		else {
			None
		}
	}

    fn try_parsing_binary(&mut self, left: AstNode<'tcx>, tokens: Vec<TokenKind>) -> ParseOutput<'tcx> {
        let current_token_kind = self.peek().kind;

        if !tokens.contains(&current_token_kind) {
            return Some(left);
        }

        let span_start = left.meta.span;

        self.advance(); // skip the operator

		let Some(ast_op) = AstOp::from_token_kind(current_token_kind) else {
			panic!("invalid token to Ast operation conversion. token: {:#?}", self.peek());
		};

        let right = self.parse_equality()?;

        let left_expr = left.kind.expr().unwrap();
        let right_expr = right.kind.expr().unwrap();

        let span_end = right.meta.span;

        Some(
            AstNode::leaf(
                self.next_node_id(),
                NodeKind::ExprAST(
                    Expr::Binary(
						BinExpr {
							operation: ast_op,
							left: Box::new(left_expr),
							right: Box::new(right_expr),
							ty: TyKind::None,
						}
					)
                ),
                ast_op,
                None,
                NodeMeta::new(
                    span_start.covering(&span_end),
                    vec![]
                )
            )
        )
    }

    fn parse_primary(&mut self) -> ParseOutput<'tcx> {
        let current_token = self.peek();

        let start_pos = current_token.span.start;

        let single_token_meta = NodeMeta::new(
            Span::new(
                self.current_file.0,
                start_pos,
                SourcePos { 
                    line: start_pos.line, 
                    column: start_pos.column + current_token.lexeme.len(),
					offset: current_token.span.file_id
                }
            ),
            vec![]
        );

        match current_token.kind {
            TokenKind::T_INT_NUM => {
				self.advance();

                Some(
                    Parser::create_expr_ast(
                        self.next_node_id(),
                        Literal::I64(current_token.lexeme.parse::<i64>().unwrap()),
                        AstOp::IntLit,
                        single_token_meta
                    )
                )
            },

            TokenKind::T_CHAR => {
				self.advance();

                Some(
                    Parser::create_expr_ast(
                        self.next_node_id(),
                        Literal::I64(current_token.lexeme.parse::<i64>().unwrap()),
                        AstOp::IntLit,
                        single_token_meta
                    )
                )
            },

            TokenKind::T_LONG_NUM => {
				self.advance();

                Some(
                    Parser::create_expr_ast(
                        self.next_node_id(),
                        Literal::I64(current_token.lexeme.parse::<i64>().unwrap()),
                        AstOp::IntLit,
                        single_token_meta
                    )
                )
            },

            TokenKind::T_FLOAT_NUM | TokenKind::T_DOUBLE_NUM => {
				self.advance();

               	Some(
					Parser::create_expr_ast(
						self.next_node_id(),
						Literal::F64(current_token.lexeme.parse::<f64>().unwrap()),
						AstOp::IntLit,
						single_token_meta
					)
                ) 
            },

            TokenKind::T_STRING => { 
				self.advance();

                Some(
					AstNode::leaf(
						self.next_node_id(),
						NodeKind::ExprAST(
							Expr::LitVal(
								LitValExpr {
									value: Literal::RawStr(current_token.lexeme),
									ty: TyKind::Str,
								}
							)
						),
						AstOp::Str,
						Some(TyKind::Str),
						single_token_meta
					)
				)
            }

            TokenKind::T_IDENTIFIER => {
				let id_token = self.advance();

                let symbol_name = id_token.lexeme;

                let current_tok_kind = self.peek().kind;

                if current_tok_kind == TokenKind::T_LPAREN {
                    self.parse_func_call_expr(symbol_name, &current_token)
                } 
                else if current_tok_kind == TokenKind::T_DOT {
                    self.parse_record_field_access_expr(symbol_name, &current_token)
                }
                else {
                    Some(
						AstNode::leaf(
							self.next_node_id(),
							NodeKind::ExprAST(
								Expr::Ident(
									IdentExpr {
										ty: TyKind::None,
										sym_name: current_token.lexeme
									}
								)
							),
							AstOp::Ident,
							None,
							single_token_meta
						)
					)
                }
            }

            TokenKind::T_LPAREN => {
				self.advance(); // ignore '('

                // group expression: e.g: (a * (b + c)))
                let group_expr = self.parse_record_or_expr(None)?;

                // group expression terminates with ')'
                self.consume(TokenKind::T_RPAREN, "')' expected")?;
				
                Some(group_expr)
            },

            // null type
            TokenKind::KW_NULL => {
				self.advance();

                Some(
					AstNode::leaf(
						self.next_node_id(),
						NodeKind::ExprAST(Expr::Null), 
						AstOp::Null, 
						Some(TyKind::Null), 
						single_token_meta
                	)
				)
            },
            _ => {
                self.report_unexpected_token(current_token);
                None
            }
        }
    }

    fn parse_identifier(&mut self) -> ParseOutput<'tcx> {
        let id = self.consume(TokenKind::T_IDENTIFIER, "expected an identifier")?;

        let meta = NodeMeta::new(
            id.span, 
            Vec::with_capacity(0)
        );

        Some(AstNode::leaf(
            self.next_node_id(),
            NodeKind::ExprAST(
                Expr::Ident(
                    IdentExpr { 
                        sym_name: id.lexeme, 
                        ty: TyKind::None 
                    }
                )
            ),
            AstOp::Ident,
            None,
            meta
        ))
    }

    fn create_expr_ast(node_id: NodeId, value: Literal<'tcx>, operation: AstOp, meta: NodeMeta) -> AstNode<'tcx> {
        AstNode::leaf(
            node_id,
            NodeKind::ExprAST(
                Expr::LitVal(
                    LitValExpr {
                        value: value.clone(),
                        ty: value.kind(),
                    }
                )
            ),
            operation,
            None,
            meta
        )
    }

    fn parse_record_field_access_expr(&mut self, rec_alias: &'tcx str, start_token: &Token) -> ParseOutput<'tcx> {
        _ = self.consume(TokenKind::T_DOT, "'.' expected");
        let access = self.consume(TokenKind::T_IDENTIFIER, "expected an identifer")?;

		let span = start_token.span.covering(&access.span);

        let field_name = access.lexeme;

        let meta = NodeMeta::new(
            span,
            vec![]
        );

        Some(
            AstNode::leaf(
                self.next_node_id(),
                NodeKind::ExprAST(
                    Expr::RecordFieldAccess(
                        RecordFieldAccessExpr { 
                            rec_name: "", // name will be set by the semantic analyser
                            rec_alias, 
                            field_name,
                            rel_stack_off: 0xFFFFFFFF, // resolver resolves this
                            ty: TyKind::None // will be determined by the semantic analyzer
                        }
                    )
                ),
                AstOp::RecFieldAccess, 
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
                    self.report_unexpected_token(self.peek());
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

		let combined_span = start_token.span.covering(&end_token.span);

        Some(AstNode::leaf(
            self.next_node_id(),
            NodeKind::ExprAST(
                Expr::FuncCall(
                    FuncCallExpr {
                        ty: TyKind::None,
                        symbol_name: called_symbol,
                        args: func_args,
                        id: FuncId(INVALID_ID) // resolver sets this value
                    }
                )
            ),
            AstOp::FuncCall,
            None,
            NodeMeta::new(
            	combined_span,
            	vec![]
            )
        ))
    }

    fn report_unexpected_token(&mut self, tok: Token<'tcx>) {
        self.diagnostics.push(
            Diagnostic::from_single_token(
                &tok, 
                "unexpected token",
                Severity::Error
            )
        );
    }

    /// Look at the current token without consuming it.
    fn peek(&self) -> Token<'tcx> {
        *self.tokens.get(self.current).unwrap_or(self.tokens.last().unwrap())
    }

	fn previous(&self) -> Option<Token<'tcx>> {
    	if self.current == 0 {
        	None
    	} else {
        	self.tokens.get(self.current - 1).copied()
    	}
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
            Some(self.advance())
        }
		else {
			if tok.kind == TokenKind::T_EOF {
				self.diagnostics.push(
					Diagnostic {
						code: Some(ErrCode::UnexpectedEOF),
						severity: Severity::Error,
						primary_span: tok.span,
						secondary_spans: vec![],
						message: "unexpected end-of-file encountered".to_string(),
						notes: vec!["you might have forgotten to include a closing brace ('}')".to_string()]
					}
				);

				return None;
			}

            self.diagnostics.push(
				Diagnostic::from_single_token(
					&tok,
					msg,
					Severity::Error,
				)
			);
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

    fn next_node_id(&mut self) -> NodeId {
        let curr_id = self.next_node_id;
        self.next_node_id += 1;
        NodeId(curr_id)
    }
}

#[cfg(test)]
mod tests {
    use kagc_comp_unit::source_map::FileId;
use kagc_errors::diagnostic::DiagnosticBag;
    use kagc_lexer::{Tokenizer, TokenizerOptions};
    use kagc_token::{Token, TokenKind};
    use kagc_ctx::StringInterner;

    use crate::{Parser, options::ParserOptions};

    fn mk_parser<'p, 'tcx>(tokens: Vec<Token<'tcx>>, diags: &'p DiagnosticBag) -> Parser<'p, 'tcx> {
        Parser::new(
            ParserOptions { file_id: FileId(0) },
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
			TokenizerOptions { file_id: 0 },
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
			TokenizerOptions { file_id: 0 },
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