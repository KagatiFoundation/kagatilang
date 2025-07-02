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

use core::panic;
use std::cell::RefCell;
use std::rc::Rc;

use kagc_ast::{record::*, *};
use kagc_comp_unit::CompilationUnit;
use kagc_ctx::CompilerCtx;
use kagc_errors::*;
use kagc_symbol::{function::*, *};
use kagc_token::*;
use kagc_types::{
    builtins::builtin::TypeId, 
    record::{
        RecordFieldType, 
        RecordType
    }, 
    LitType, 
    LitTypeVariant
};

/// A type alias representing the result of parsing, which can either
/// be an AST node on successful parsing or a ParseError indicating a
/// parsing failure.
type ParseResult2<'a> = Result<AST, Box<BErr>>;

type TokenMatch<'a> = Result<&'a Token, Box<BErr>>;

/// Represents an invalid function ID.
///
/// This constant is used to indicate that a function ID is not valid or
/// not set, serving as a sentinel value during parsing and code generation
/// to detect error states and invalid contexts.
const INVALID_FUNC_ID: usize = 0xFFFFFFFF;

pub type StringLabel = usize;

#[derive(Debug, Clone, Default)]
pub struct SharedParserCtx {
    next_str_label: StringLabel
}

impl SharedParserCtx {
    pub fn next_str_label(&mut self) -> StringLabel {
        let ret = self.next_str_label;
        self.next_str_label += 1;
        ret
    }
}

#[derive(Debug, Clone)]
struct ParserContext {
    is_erronous_parse: bool,
}

impl ParserContext {
    pub fn toggle_error_flag(&mut self) {
        self.is_erronous_parse = !self.is_erronous_parse;
    }
}

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

    /// Local symbols of the function that is currently being parsed.
    temp_local_syms: Symtable<Symbol>,

    /// Offset of next local variable.
    local_offset: i32,

    /// Position of next local symbol.
    next_local_sym_pos: usize,

    next_fn_id: usize,

    /// Context in which the ```Parser``` is going to work on.
    ctx: Rc<RefCell<CompilerCtx>>,

    /// Context of this parser.
    __pctx: ParserContext,

    shared_pctx: Rc<RefCell<SharedParserCtx>>,

    __panic_mode: bool,

    /// Label generator that is going to be used by string literals only.
    _str_label_: usize
}

impl Parser {
    #[allow(clippy::new_without_default)]
    pub fn new(panic_mode: bool, ctx: Rc<RefCell<CompilerCtx>>, shared_pctx: Rc<RefCell<SharedParserCtx>>) -> Self {
        let current_token: Token = Token::none();
        Self {
            tokens: Rc::new(vec![]),
            current: 0,
            next_fn_id: 0,
            current_token,
            current_function_id: INVALID_FUNC_ID,
            current_function_name: None,
            temp_local_syms: Symtable::default(),
            local_offset: 0,
            next_local_sym_pos: 0,
            ctx,
            __pctx: ParserContext {
                is_erronous_parse: false,
            },
            shared_pctx,
            __panic_mode: panic_mode,
            _str_label_: 0
        }
    }

    pub fn parse(&mut self, unit: &mut CompilationUnit) -> Vec<AST> {
        if let Some(tokens) = &unit.tokens {
            self.tokens = tokens.clone();
        }
        else {
            panic!("No tokens provided!");
        }

        self.current_token = self.tokens[0].clone();

        let mut nodes: Vec<AST> = vec![];
        loop {
            if self.current_token.kind == TokenKind::T_EOF {
                break;
            }
            let stmt_parse_result: ParseResult2 = self.parse_single_stmt();
            if let Ok(stmt) = stmt_parse_result {
                nodes.push(stmt);
            } else if let Some(parse_error) = stmt_parse_result.err() {
                if !parse_error.is_ignorable() {
                    parse_error.fatal();
                    self.__pctx.toggle_error_flag();
                    self.skip_to_next_stmt();
                }
            }
        }

        nodes
    }

    pub fn has_parsing_errors(&self) -> bool {
        self.__pctx.is_erronous_parse
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
    fn parse_single_stmt(&mut self) -> ParseResult2 {
        let curr_tok_kind: TokenKind = self.current_token.kind;
        let result: ParseResult2 = match curr_tok_kind {
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
                let err: Result<_, Box<BErr>> = Err(
                    Box::new(
                        BErr::unexpected_token(
                            self.get_current_file_name(),
                            vec![
                                TokenKind::KW_LET, 
                                TokenKind::KW_IF, 
                                TokenKind::KW_WHILE, 
                                TokenKind::KW_FOR, 
                                TokenKind::KW_LOOP
                            ],
                            self.current_token.clone(),
                        )
                    )
                );
                err
            }
        };

        match curr_tok_kind {
            TokenKind::KW_LET
            | TokenKind::KW_RETURN
            | TokenKind::KW_BREAK
            | TokenKind::T_IDENTIFIER => {
                _ = self.token_match(TokenKind::T_SEMICOLON)?;
            },
            _ => ()
        }
        result
    }

    // parse compound statement(statement starting with '{' and ending with '}')
    fn parse_compound_stmt(&mut self) -> ParseResult2 {
        _ = self.token_match(TokenKind::T_LBRACE)?;
        let mut left: Option<AST> = None;
        let mut stmt_count: i32 = 0;
        loop {
            if self.current_token.kind == TokenKind::T_RBRACE {
                _ = self.token_match(TokenKind::T_RBRACE)?; // match and ignore '}'
                break;
            }
            let tree_result: ParseResult2 = self.parse_single_stmt();
            if let Err(parse_error) = tree_result {
                if !parse_error.is_ignorable() {
                    return Err(parse_error);
                }
            } else if let Ok(parse_res) = tree_result {
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
        if let Some(node) = left {
            Ok(node)
        } else {
            let __err: Result<AST, Box<BErr>> = Err(Box::new(BErr::unexpected_token(
                self.get_current_file_name(),
                vec![],
                self.current_token.clone(),
            )));
            if self.__panic_mode {
                panic!("{:?}", __err);
            }
            __err
        }
    }

    fn parse_import_stmt(&mut self) -> ParseResult2 {
        _ = self.token_match(TokenKind::KW_IMPORT)?; // match 'import' keyword
        let module_path_tok: Token = self.token_match(TokenKind::T_STRING)?.clone();
        _ = self.token_match(TokenKind::T_SEMICOLON); // match ';'
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
                LitTypeVariant::None, // none type
                None,
                None
            )
        )
    }

    fn parse_record_decl_stmt(&mut self) -> ParseResult2 {
        _ = self.token_match(TokenKind::KW_RECORD); // match 'record' keyword

        // expect name of the record
        let id_token = self.token_match(TokenKind::T_IDENTIFIER)?.clone();

        _ = self.token_match(TokenKind::T_LBRACE); // match '{'

        let mut rec_fields = vec![];

        while self.current_token.kind != TokenKind::T_RBRACE {
            rec_fields.push(self.parse_record_field_decl_stmt()?);
        }
        
        _ = self.token_match(TokenKind::T_RBRACE); // match '}'

        let record_entry = RecordType {
            name: id_token.lexeme.clone(),
            size: 0,
            __alignment: 0,
            fields: rec_fields.into_iter().enumerate().map(|(idx, field)| {
                RecordFieldType {
                    name: field.name.clone(),
                    typ: field.typ,
                    rel_stack_off: idx
                }
            }).collect::<Vec<RecordFieldType>>()
        };

        self.ctx.borrow_mut().create_record(record_entry);

        Ok(
            AST::create_leaf(
                ASTKind::StmtAST(
                    Stmt::Record(RecordDeclStmt { name: id_token.lexeme.clone() })
                ), 
                ASTOperation::AST_RECORD_DECL,
                LitTypeVariant::Record,
                None,
                None
            )
        )
    }

    fn parse_record_field_decl_stmt(&mut self) -> Result<RecordField, Box<BErr>> {
        let id_token = self.token_match(TokenKind::T_IDENTIFIER)?.clone();
        _ = self.token_match(TokenKind::T_COLON); // match ':'

        let id_type = self.parse_id_type()?;
        if id_type == LitTypeVariant::Null {
            return Err(
                Box::new(
                    BErr::new(
                        BErrType::InvalidRecordFieldType, 
                        "record".to_string(), 
                        id_token.clone()
                    )
                )
            )
        }

        self.skip_to_next_token(); // skip id type

        // check if default value has been assigned
        if self.current_token.kind == TokenKind::T_EQUAL {
            todo!("Default value is not supported right now in record field!");
        }

        _ = self.token_match(TokenKind::T_SEMICOLON); // match ';'

        Ok(
            RecordField { 
                typ: TypeId::from(id_type), 
                name: id_token.lexeme.clone(), 
                default_value: None 
            }
        )
    }

    // parsing a function declaration and definition
    // supports multiple parameters
    fn parse_function_stmt(&mut self) -> ParseResult2 {
        // reset local offset counter to 0
        self.local_offset = 0;

        // Creating a new scope for function declaration
        let func_scope_id: usize = self.ctx.borrow_mut().enter_new_scope();

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
        _ = self.token_match(TokenKind::T_LPAREN)?;

        let current_file: String = self.get_current_file_name();

        let mut func_param_types: Vec<LitTypeVariant> = vec![];

        if self.current_token.kind != TokenKind::T_RPAREN {
            loop {
                if let Ok(param) = self.parse_parameter() {
                    let sym: Symbol = Symbol::create(
                        param.name.clone(), 
                        param.lit_type, 
                        SymbolType::Variable, 
                        param.lit_type.size(), 
                        StorageClass::PARAM, 
                        param.offset,
                        None,
                        self.current_function_id,
                    );

                    func_param_types.push(sym.lit_type);
                    self.add_symbol_local(sym.clone());
                } 

                let is_tok_comma: bool = self.current_token.kind == TokenKind::T_COMMA;
                let is_tok_rparen: bool = self.current_token.kind == TokenKind::T_RPAREN;

                if !is_tok_comma && !is_tok_rparen {
                    let err: Result<_, Box<BErr>> = Err(Box::new(BErr::unexpected_token(
                        current_file.clone(), 
                        vec![TokenKind::T_COMMA, TokenKind::T_RPAREN],
                        self.current_token.clone()
                    )));

                    if self.__panic_mode {
                        panic!("{:?}", err);
                    }

                    return err;
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
            let function_body_res: ParseResult2 = self.parse_compound_stmt();

            let body: AST = match function_body_res {
                Ok(ast) => ast,
                Err(err) => return Err(err)
            };

            function_body = Some(body);
        } 
        else {
            _ = self.token_match(TokenKind::T_SEMICOLON)?;
        }

        let temp_func_id: usize = self.current_function_id;
        self.current_function_id = INVALID_FUNC_ID; 

        // create a new FunctionInfo
        self.ctx.borrow_mut().exit_scope();

        // reset offset counter after parsing function
        let tmp_off = self.local_offset;
        self.local_offset = 0;

        // reset temporary symbols holder after the function has been parsed
        let tmp_syms = self.temp_local_syms.clone();
        self.temp_local_syms = Symtable::default();
        
        /*
        Stack offset calculation:
         'x29' and 'x30' has to be preserved. Thus, the extra 15 bytes has to 
         be allocated for them.
         Also the x0-x3 has to be preserved if they are used during function calls. 
         So, allocate extra 32 bytes for them as well.
         */
        let stack_offset: i32 = (tmp_off + 15 + 32) & !15;

        // Return AST for function declaration
        Ok(AST::new(
            ASTKind::StmtAST(Stmt::FuncDecl(FuncDeclStmt {
                func_id: temp_func_id,
                stack_off: stack_offset as usize,
                name: id_token.lexeme.clone(),
                scope_id: func_scope_id,
                return_type: TypeId::from(func_return_type),
                storage_class: func_storage_class,
                locals: tmp_syms,
                func_param_types
            })),
            ASTOperation::AST_FUNCTION,
            function_body,
            None,
            func_return_type,
        ))
    }

    // parse function's return type
    fn parse_fn_ret_type(&mut self) -> Result<LitTypeVariant, Box<BErr>> {
        let curr_tok: Token = self.current_token.clone();
        if curr_tok.kind != TokenKind::T_ARROW {
            return Err(Box::new(
                BErr::new(
                    BErrType::MissingReturnType, 
                    self.get_current_file_name(), 
                    curr_tok.clone()
                )
            ))
        }

        _ = self.token_match(TokenKind::T_ARROW)?;
        let func_ret_type: LitTypeVariant = self.parse_id_type()?;

        if func_ret_type == LitTypeVariant::Null {
            return Err(Box::new(
                BErr::new(
                    BErrType::InvalidReturnType, 
                    self.get_current_file_name(), 
                    curr_tok
                )
            ));
        }

        Ok(func_ret_type)
    }

    fn parse_parameter(&mut self) -> Result<FuncParam, Box<BErr>> {
        let param_name: Token = self.token_match(TokenKind::T_IDENTIFIER)?.clone();
        let _ = self.token_match(TokenKind::T_COLON)?;
        let param_type: LitTypeVariant = self.parse_id_type()?;
        let param_loc_off: i32 = self.gen_next_local_offset() as i32;
        self.skip_to_next_token();
        Ok(FuncParam {
            lit_type: param_type,
            name: param_name.lexeme,
            offset: param_loc_off
        })
    }

    fn parse_return_stmt(&mut self) -> ParseResult2 {
        // check whether parser's parsing a function or not
        if self.current_function_id == INVALID_FUNC_ID {
            let __err: Result<_, Box<BErr>> = Err(
                Box::new(
                    BErr::unexpected_token(
                        self.get_current_file_name(), 
                        vec![],
                        self.current_token.clone()
                    )
                )
            );

            if self.__panic_mode {
                panic!("{:?}", __err);
            }

            return __err;
        }

        _ = self.token_match(TokenKind::KW_RETURN)?;

        if self.current_token.kind == TokenKind::T_SEMICOLON {
            return Ok(
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
                    None,
                    None
                )
            );
        }
        let return_expr: AST = self.parse_record_or_expr()?;
        Ok(AST::new(
            ASTKind::StmtAST(Stmt::Return(ReturnStmt {
                func_id: self.current_function_id,
            })),
            ASTOperation::AST_RETURN,
            Some(return_expr),
            None,
            LitTypeVariant::None,
        ))
    }

    fn parse_while_stmt(&mut self) -> ParseResult2 {
        let cond_ast: ParseResult2 = self.parse_conditional_stmt(TokenKind::KW_WHILE);
        let while_body: AST = self.parse_single_stmt()?;
        Ok(AST::new(
            ASTKind::StmtAST(Stmt::While),
            ASTOperation::AST_WHILE,
            cond_ast.ok(),
            Some(while_body),
            LitTypeVariant::None,
        ))
    }

    fn parse_loop_stmt(&mut self) -> ParseResult2 {
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

    fn parse_break_stmt(&mut self) -> ParseResult2 {
        _ = self.token_match(TokenKind::KW_BREAK)?; // match and ignore 'break'
        _ = self.token_match(TokenKind::T_SEMICOLON)?; // match and ignore ';'
        Ok(AST::new(
            ASTKind::StmtAST(Stmt::Break),
            ASTOperation::AST_BREAK,
            None,
            None,
            LitTypeVariant::None
        ))
    }

    fn parse_for_stmt(&mut self) -> ParseResult2 {
        _ = self.token_match(TokenKind::KW_FOR)?; // match and ignore the keyword 'for'
        _ = self.token_match(TokenKind::T_LPAREN)?; // match and ignore '('
        let pre_stmt: AST = self.parse_single_stmt()?; // initialization statement
                                                       // _ = self.token_match(TokenKind::T_SEMICOLON);
        let cond_ast: ParseResult2 = self.parse_equality(); // conditional section of for loop
        if let Ok(_icast) = &cond_ast {
            if (_icast.operation < ASTOperation::AST_EQEQ)
                || (_icast.operation > ASTOperation::AST_LTHAN)
            {
                // if operation kind is not "relational operation"
                panic!("Please provide conditional expression for 'for'");
            }
        }
        _ = self.token_match(TokenKind::T_SEMICOLON)?; // expect semicolon
        let incr_ast: ParseResult2 = self.parse_single_stmt();
        _ = self.token_match(TokenKind::T_RPAREN)?; // match and ignore ')'
        let for_body: ParseResult2 = self.parse_single_stmt();
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

    fn parse_if_stmt(&mut self) -> ParseResult2 {
        let cond_ast: AST = self.parse_conditional_stmt(TokenKind::KW_IF)?;

        let if_scope = self.ctx.borrow_mut().enter_new_scope();

        let if_true_ast: AST = self.parse_single_stmt()?;

        self.ctx.borrow_mut().exit_scope();

        let mut if_false_ast: Option<AST> = None;
        if self.current_token.kind == TokenKind::KW_ELSE {
            self.skip_to_next_token(); // skip 'else'
            if_false_ast = Some(self.parse_single_stmt()?);
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
    fn parse_conditional_stmt(&mut self, kind: TokenKind) -> ParseResult2 {
        _ = self.token_match(kind)?;
        _ = self.token_match(TokenKind::T_LPAREN)?; // match and ignore '('

        let cond_ast: ParseResult2 = self.parse_equality();

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
    fn parse_var_decl_stmt(&mut self) -> ParseResult2 {
        // consume 'let'
        _ = self.token_match(TokenKind::KW_LET)?;

        // Being "inside" a function means that we are currently parsing a function's body.
        //
        // INVALID_FUNC_ID equals 0xFFFFFFFF if we are not parsing a function currently.
        let inside_func: bool = self.current_function_id != INVALID_FUNC_ID;

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

        // Parser may encounter a colon after the identifier name.
        // This means the type of this variable has been defined
        // by the user.
        if self.current_token.kind == TokenKind::T_COLON {
            _ = self.token_match(TokenKind::T_COLON)?;
            var_type = self.parse_id_type()?;

            // if the declared variable is a record
            if var_type == LitTypeVariant::Record {
                let record_name = self.token_match(TokenKind::T_IDENTIFIER)?.clone();
                sym_type = SymbolType::Record { name: record_name.lexeme };
            }
            else {
                self.skip_to_next_token();
            }
        }

        // Stores the RHS value of this variable (if defined)
        let mut assignment_parse_res: Option<ParseResult2> = None;

        // Checking whether variable is assigned at the time of its declaration.
        // If identifier name is followed by an equal sign, then it is assigned 
        // at the time of declaration.
        if self.current_token.kind == TokenKind::T_EQUAL {
            _ = self.token_match(TokenKind::T_EQUAL)?; // match and ignore '=' sign
            assignment_parse_res = Some(self.parse_record_or_expr());
        }

        // Default value contains compile-time evaluated expression's result.
        let mut default_value: Option<LitType> = None;

        // if there is some error during expression parsing
        if let Some(Err(parse_err)) = assignment_parse_res {
            return Err(parse_err);
        } 
        else if let Some(Ok(_)) = assignment_parse_res {
            if var_type == LitTypeVariant::Str {
                let str_const_label: usize = self.shared_pctx.borrow_mut().next_str_label();
                default_value = Some(LitType::I32(str_const_label as i32));
            }
        }

        // let symbol_add_pos: usize = self.add_symbol_local(sym.clone()).unwrap();

        let return_result: ParseResult2 = if let Some(assign_ast_node_res) = assignment_parse_res {
            Ok(AST::new(
                ASTKind::StmtAST(Stmt::VarDecl(VarDeclStmt {
                    symtbl_pos: 0xFFFFFFFF, // this value will be set by the resolver
                    symbol_type: sym_type,
                    class: var_class,
                    sym_name: id_token.lexeme.clone(),
                    type_id: TypeId::from(var_type),
                    local_offset: if inside_func { self.gen_next_local_offset() } else { 0 },
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
            Ok(AST::new(
                ASTKind::StmtAST(Stmt::VarDecl(VarDeclStmt {
                    symtbl_pos: 0xFFFFFFFF, // this value will be set by the resolver
                    class: var_class,
                    symbol_type: sym_type,
                    sym_name: id_token.lexeme.clone(),
                    type_id: TypeId::from(var_type),
                    local_offset: if inside_func { self.gen_next_local_offset() } else { 0 },
                    func_id: if inside_func { self.current_function_id } else { 0xFFFFFFFF },
                    default_value
                })),
                ASTOperation::AST_VAR_DECL,
                None,
                None,
                var_type,
            ))
        };
        return_result
    }

    fn gen_next_local_offset(&mut self) -> usize {
        let tmp_off: i32 = self.local_offset;
        self.local_offset += 1;
        tmp_off as usize
    }

    /// Parses the current token as a literal type keyword and returns the 
    /// corresponding `LitTypeVariant`.
    ///
    /// Returns an error if the token does not represent a valid data type keyword.
    fn parse_id_type(&mut self) -> Result<LitTypeVariant, Box<BErr>> {
        let current_tok: TokenKind = self.current_token.kind;
        match current_tok {
            TokenKind::KW_INT => Ok(LitTypeVariant::I32),
            TokenKind::KW_CHAR => Ok(LitTypeVariant::U8),
            TokenKind::KW_STR => Ok(LitTypeVariant::Str),
            TokenKind::KW_LONG => Ok(LitTypeVariant::I64),
            TokenKind::KW_VOID => Ok(LitTypeVariant::Void),
            TokenKind::KW_NULL => Ok(LitTypeVariant::Null),
            TokenKind::T_IDENTIFIER => Ok(LitTypeVariant::Record),
            _ => {
                Err(Box::new(
                    BErr::unexpected_token(
                        self.get_current_file_name(), 
                        vec![TokenKind::T_DTYPE], 
                        self.current_token.clone()
                    )
                ))
            }
        }
    }

    // TODO: Write comments
    fn assign_stmt_or_func_call(&mut self) -> ParseResult2 {
        let id_token: Token = self.token_match(TokenKind::T_IDENTIFIER)?.clone();
        let tok_kind_after_id_tok: TokenKind = self.current_token.kind;
        if tok_kind_after_id_tok != TokenKind::T_LPAREN {
            self.parse_assignment_stmt(id_token)
        } else {
            self.parse_func_call_expr(&id_token.lexeme, &id_token)
        }
    }

    fn parse_assignment_stmt(&mut self, id_token: Token) -> ParseResult2 {
        _ = self.token_match(TokenKind::T_EQUAL)?;

        let bin_expr_ast_node: AST = self.parse_record_or_expr()?;

        // _ = self.token_match(TokenKind::T_SEMICOLON)?;

        let lvalueid: AST = AST::create_leaf(
            ASTKind::StmtAST(Stmt::LValue2{ name: id_token.lexeme.clone() }),
            ASTOperation::AST_LVIDENT,
            LitTypeVariant::None,
            None,
            None
        );

        Ok(AST::new(
            ASTKind::StmtAST(Stmt::Assignment(AssignStmt {
                sym_name: id_token.lexeme.clone()
            })),
            ASTOperation::AST_ASSIGN,
            Some(lvalueid),
            Some(bin_expr_ast_node),
            LitTypeVariant::Void,
        ))
    }

    fn parse_record_or_expr(&mut self) -> ParseResult2 {
        if self.current_token.kind == TokenKind::T_IDENTIFIER {
            if let Some(next) = self.tokens.get(self.current + 1) {
                if next.kind == TokenKind::T_LBRACE {
                    return self.parse_record_creation();
                }
            }
        }
        self.parse_equality()
    }

    fn parse_record_creation(&mut self) -> ParseResult2 {
        let id_token = self.token_match(TokenKind::T_IDENTIFIER)?.clone();
        _ = self.token_match(TokenKind::T_LBRACE)?;

        let mut fields = vec![];

        while self.current_token.kind != TokenKind::T_RBRACE {
            fields.push(self.parse_record_field_assignment()?);
        
            match self.current_token.kind {
                TokenKind::T_COMMA => {
                    self.token_match(TokenKind::T_COMMA)?; // match ','
                }
                TokenKind::T_RBRACE => {
                    self.token_match(TokenKind::T_RBRACE)?; // match '}'
                    break;
                }
                _ => {
                    return Err(Box::new(BErr::unexpected_token(
                        self.get_current_file_name(),
                        vec![TokenKind::T_COMMA, TokenKind::T_RBRACE],
                        self.current_token.clone(),
                    )));
                }
            }
        }

        Ok(
            AST::create_leaf(
                ASTKind::ExprAST(
                    Expr::RecordCreation(
                        RecordCreationExpr { 
                            name: id_token.lexeme.clone(), 
                            fields
                        }
                    )
                ),
                ASTOperation::AST_RECORD_CREATE, 
                LitTypeVariant::Record, 
                None, 
                None
            )
        )
    }

    fn parse_record_field_assignment(&mut self) -> Result<RecordFieldAssignExpr, Box<BErr>> {
        let id_token = self.token_match(TokenKind::T_IDENTIFIER)?.clone();
        _ = self.token_match(TokenKind::T_EQUAL); // parse '='
        let field_val = self.parse_record_or_expr()?;

        if let ASTKind::ExprAST(expr) = field_val.kind {
            return Ok(
                RecordFieldAssignExpr { 
                    name: id_token.lexeme.clone(), 
                    value: Box::new(expr),
                    offset: self.gen_next_local_offset()
                }
            );
        }

        panic!()
    }

    fn parse_equality(&mut self) -> ParseResult2 {
        let left: ParseResult2 = self.parse_comparision();
        self.try_parsing_binary(left, vec![TokenKind::T_EQEQ, TokenKind::T_NEQ])
    }

    fn parse_comparision(&mut self) -> ParseResult2 {
        let left: ParseResult2 = self.parse_addition();
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

    fn parse_addition(&mut self) -> ParseResult2 {
        let left: ParseResult2 = self.parse_factor();
        self.try_parsing_binary(left, vec![TokenKind::T_PLUS, TokenKind::T_MINUS])
    }

    fn parse_factor(&mut self) -> ParseResult2 {
        let left: ParseResult2 = self.parse_primary();
        self.try_parsing_binary(left, vec![TokenKind::T_SLASH, TokenKind::T_STAR])
    }

    fn try_parsing_binary(&mut self, lhs: ParseResult2, tokens: Vec<TokenKind>) -> ParseResult2 {
        let left: AST = lhs.clone()?;
        let current_token_kind: TokenKind = self.current_token.kind;

        if !tokens.contains(&current_token_kind) {
            return lhs;
        }

        self.skip_to_next_token(); // skip the operator

        let ast_op: ASTOperation = ASTOperation::from_token_kind(current_token_kind).unwrap(); // DANGEROUS unwrap CALL

        let right: AST = self.parse_equality()?;

        let left_expr: Expr = left.kind.expr().unwrap();
        let right_expr: Expr = right.kind.expr().unwrap();

        Ok(AST::create_leaf(
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
            None,
            None
        ))
    }

    fn parse_primary(&mut self) -> ParseResult2 {
        let current_token: Token = self.current_token.clone();
        let current_file: String = self.get_current_file_name();
        self.skip_to_next_token();
        match current_token.kind {
            TokenKind::T_INT_NUM => Ok(Parser::create_expr_ast(
                LitType::I32(current_token.lexeme.parse::<i32>().unwrap()),
                ASTOperation::AST_INTLIT,
            )),
            TokenKind::T_CHAR => Ok(Parser::create_expr_ast(
                LitType::U8(current_token.lexeme.parse::<u8>().unwrap()),
                ASTOperation::AST_INTLIT,
            )),
            TokenKind::T_LONG_NUM => Ok(Parser::create_expr_ast(
                LitType::I64(current_token.lexeme.parse::<i64>().unwrap()),
                ASTOperation::AST_INTLIT,
            )),
            TokenKind::T_FLOAT_NUM | TokenKind::T_DOUBLE_NUM => Ok(Parser::create_expr_ast(
                LitType::F64(current_token.lexeme.parse::<f64>().unwrap()),
                ASTOperation::AST_INTLIT,
            )),
            TokenKind::T_STRING => {
                let str_label = self.shared_pctx.borrow_mut().next_str_label();

                let str_const_symbol: Symbol = Symbol::new(
                    format!("_STR_{}---{}", str_label, current_token.lexeme.clone()),
                    LitTypeVariant::Str,
                    SymbolType::Constant,
                    StorageClass::GLOBAL,
                );

                // keep strings at the global scope
                self.ctx.borrow_mut().root_scope_mut().declare(str_const_symbol);
                
                Ok(AST::create_leaf(
                    ASTKind::ExprAST(
                        Expr::LitVal(
                            LitValExpr {
                                value: LitType::Str { 
                                    value: current_token.lexeme.clone(), 
                                    label_id: str_label
                                },
                                result_type: LitTypeVariant::Str,
                            }
                        )
                    ),
                    ASTOperation::AST_STRLIT,
                    LitTypeVariant::Str,
                    None,
                    None
                ))
            }
            TokenKind::T_IDENTIFIER => {
                // Identifiers in a global variable declaration expression are not allowed.
                if self.is_scope_global() {
                    println!("hehe: {current_token:#?}");
                    return Err(Box::new(
                        BErr::new(
                            BErrType::TypeError(
                                BTypeErr::InitializerNotAConstant { 
                                    lexeme: current_token.lexeme.clone() 
                                }
                            ),
                            current_file.clone(), 
                            current_token.clone()
                        )
                    ));
                }
                
                let symbol_name: String = current_token.lexeme.clone();
                let curr_tok_kind: TokenKind = self.current_token.kind;

                if curr_tok_kind == TokenKind::T_LPAREN {
                    self.parse_func_call_expr(&symbol_name, &current_token)
                } 
                else if curr_tok_kind == TokenKind::T_DOT {
                    self.parse_record_field_access_expr(&symbol_name)
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
                        None,
                        None
                    ))
                }
            }
            TokenKind::T_LPAREN => {
                // group expression: e.g: (a * (b + c)))
                let group_expr: ParseResult2 = self.parse_record_or_expr();
                // Group expression terminates with ')'. Match and ignore ')'.
                _ = self.token_match(TokenKind::T_RPAREN)?;
                Ok(group_expr.unwrap())
            },

            // null type
            TokenKind::KW_NULL => {
                Ok(
                    AST::create_leaf(
                        ASTKind::ExprAST(Expr::Null), 
                        ASTOperation::AST_NULL, 
                        LitTypeVariant::Null, 
                        None, 
                        None
                    )
                )
            },

            _ => {
                let __e: Result<AST, Box<BErr>> = Err(
                    Box::new(
                        BErr::unexpected_token(
                        current_file.clone(),
                        vec![TokenKind::T_EXPR],
                        current_token.clone(),
                        )
                    )
                );
                if self.__panic_mode {
                    panic!("{:?}", __e);
                }
                __e
            }
        }
    }

    fn create_expr_ast(value: LitType, operation: ASTOperation) -> AST {
        AST::create_leaf(
            ASTKind::ExprAST(Expr::LitVal(LitValExpr {
                value: value.clone(),
                result_type: value.variant(),
            })),
            operation,
            value.variant(),
            None,
            None
        )
    }

    fn _parse_array_index_expr(&mut self, indexed_symbol: &Symbol, sym_index: usize, sym_token: &Token) -> ParseResult2 {
        _ = self.token_match(TokenKind::T_LBRACKET)?;
        let current_file: String = self.get_current_file_name();
        let array_access_expr_result: ParseResult2 = self.parse_equality();

        #[allow(clippy::question_mark)]
        if array_access_expr_result.is_err() {
            return array_access_expr_result;
        }

        let array_access_expr: AST = array_access_expr_result.ok().unwrap();
        if indexed_symbol.sym_type != SymbolType::Array {
            return Err(Box::new(BErr::nonsubsriptable_ident(
                current_file.clone(),
                sym_token.clone(),
            )));
        }

        _ = self.token_match(TokenKind::T_RBRACKET)?;
        Ok(AST::create_leaf(
            ASTKind::ExprAST(Expr::Subscript(SubscriptExpr {
                index: Box::new(array_access_expr.kind.expr().unwrap()),
                symtbl_pos: sym_index,
                result_type: indexed_symbol.lit_type,
            })),
            ASTOperation::AST_ARRAY_ACCESS,
            indexed_symbol.lit_type,
            None,
            None
        ))
    }

    fn parse_record_field_access_expr(&mut self, rec_alias: &str) -> ParseResult2 {
        _ = self.token_match(TokenKind::T_DOT); // match '.'
        let access = self.token_match(TokenKind::T_IDENTIFIER)?;
        Ok(
            AST::create_leaf(
                ASTKind::ExprAST(
                    Expr::RecordFieldAccess(
                        RecordFieldAccessExpr { 
                            rec_name: "".to_string(),
                            rec_alias: rec_alias.to_string(), 
                            field_name: access.lexeme.clone(),
                            rel_stack_off: 0
                        }
                    )
                ),
                ASTOperation::AST_RECORD_FIELD_ACCESS, 
                LitTypeVariant::None, 
                None, 
                None
            )
        )
    }

    fn parse_func_call_expr(&mut self, called_symbol: &str, sym_token: &Token) -> ParseResult2 {
        let current_file: String = self.get_current_file_name();
        _ = self.token_match(TokenKind::T_LPAREN)?;

        let curr_token_kind: TokenKind = self.current_token.kind;
        let mut func_args: Vec<FuncArg> = vec![];

        if curr_token_kind != TokenKind::T_RPAREN {
            let mut arg_pos: usize = 0;

            loop {
                let argu: AST = self.parse_record_or_expr()?;
                func_args.push((arg_pos, argu.kind.expr().unwrap()));

                let is_tok_comma: bool = self.current_token.kind == TokenKind::T_COMMA;
                let is_tok_rparen: bool = self.current_token.kind == TokenKind::T_RPAREN;

                if !is_tok_comma && !is_tok_rparen {
                    let err: Result<AST, Box<BErr>> = Err(
                        Box::new(
                            BErr::unexpected_token(
                                current_file.clone(), 
                                vec![TokenKind::T_COMMA, TokenKind::T_RPAREN],
                                self.current_token.clone()
                            )
                        )
                    );
                    return err;
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

        _ = self.token_match(TokenKind::T_RPAREN)?;

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
            Some(sym_token.clone()),
            None
        ))
    }

    fn get_current_file_name(&self) -> String {
        String::from("ok")
    }

    /// Adds the symbol to the current scope.
    fn add_symbol_local(&mut self, sym: Symbol) -> Option<usize> {
        let insert_pos = self.ctx.borrow_mut().declare(sym.clone());
        self.next_local_sym_pos += 1;
        insert_pos
    }

    fn is_scope_global(&self) -> bool {
        self.ctx.borrow().live_scope_id() == 0
    }

    fn token_match_no_advance(&mut self, kind: TokenKind) -> TokenMatch {
        let current: Token = self.current_token.clone();
        if kind != current.kind {
            return Err(
                Box::new(
                    BErr::unexpected_token(
                        self.get_current_file_name(), 
                        vec![kind],
                        current
                    )
                )
            );
        }
        // self.skip_to_next_token();
        Ok(&self.tokens[self.current - 1])
    }

    fn token_match(&mut self, kind: TokenKind) -> TokenMatch {
        let current: Token = self.current_token.clone();

        if kind != current.kind {
            let err = Err(
                Box::new(
                    BErr::unexpected_token(
                        self.get_current_file_name(), 
                        vec![kind],
                        current
                    )
                )
            );

            if self.__panic_mode {
                panic!("{:?}", err);
            }

            return err;
        }

        self.skip_to_next_token();
        Ok(&self.tokens[self.current - 1])
    }

    fn skip_to_next_stmt(&mut self) {
        let stmt_start_tokens = [TokenKind::KW_LET, TokenKind::T_IDENTIFIER, TokenKind::KW_IF, TokenKind::KW_WHILE, TokenKind::KW_LOOP];
        loop {
            if stmt_start_tokens.contains(&self.current_token.kind) {
                break;
            }
            self.skip_to_next_token();
        }
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

// REWRITE ALL THE TEST CASES
#[cfg(test)]
mod tests {

}