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

use kagc_ast::*;
use kagc_errors::code::ErrCode;
use kagc_errors::diagnostic::Diagnostic;
use kagc_errors::diagnostic::Severity;
use kagc_symbol::*;
use kagc_types::*;

use crate::SAResult;

pub struct TypeChecker;

impl TypeChecker {
    /// Performs type checking for variable declaration statements.
    /// 
    /// This function infers the type of an expression assigned to a variable,
    /// updates the variable's symbol if it was declared without an explicit 
    /// type, and ensures the assigned expression's type matches the declared or 
    /// inferred type. If there's a type mismatch that cannot be reconciled, an 
    /// error is returned.
    pub fn type_check_var_decl_stmt(var_decl_sym: &mut Symbol, expr_type: LitTypeVariant, meta: &NodeMeta) -> SAResult {
        // This has do be done because variables might be defined without 
        // explicit type annotation. For example, 'let a = 5;'. Thus, the 
        // symbol table has to be updated with this new type information.
        if var_decl_sym.lit_type == LitTypeVariant::None {
            var_decl_sym.lit_type = {
                match expr_type {
                    // implicitly convert no-type-annotated byte-type into an integer
                    LitTypeVariant::U8 => LitTypeVariant::I32,
                    _ => expr_type.clone()
                }
            }
        }
        if 
            var_decl_sym.lit_type != expr_type
            && !is_type_coalescing_possible(expr_type.clone(), var_decl_sym.lit_type.clone()) 
        {
            let diag = Diagnostic {
                code: Some(ErrCode::TYP2104),
                severity: Severity::Error,
                primary_span: meta.span,
                secondary_spans: vec![],
                message: format!("expected type `{}`, found `{}`", var_decl_sym.lit_type, expr_type.clone()),
                notes: vec![]
            };
            return Err(diag);
        }
        Ok(expr_type)
    }

    pub fn check_bin_expr_type_compatability(
        a: LitTypeVariant, b: LitTypeVariant, 
        op: ASTOperation,   meta: &NodeMeta
    ) -> SAResult {
        match op {
            ASTOperation::AST_ADD | ASTOperation::AST_SUBTRACT |
            ASTOperation::AST_MULTIPLY | ASTOperation::AST_DIVIDE => {
                // arithmetic operations require both of the expressions to be integer types(for now)
                if a.is_int() && b.is_int() {
                    let a_size: usize = a.size();
                    let b_size: usize = b.size();

                    #[allow(clippy::comparison_chain)]
                    if a_size > b_size {
                        return SAResult::Ok(a);
                    }
                    else if b_size > a_size {
                        return SAResult::Ok(b);
                    }
                    else {
                        return SAResult::Ok(a); // both sizes are equal; return any
                    }
                }
                let diag = Diagnostic {
                    code: Some(ErrCode::TYP2103),
                    severity: Severity::Error,
                    primary_span: meta.span,
                    secondary_spans: vec![],
                    message: format!("'{}' is not compatible with '{}'", a, b),
                    notes: vec![]
                };
                Err(diag)
            },
            ASTOperation::AST_GTHAN
            | ASTOperation::AST_LTHAN 
            | ASTOperation::AST_EQEQ
            | ASTOperation::AST_NEQ
            | ASTOperation::AST_LTEQ
            | ASTOperation::AST_GTEQ => {
                if Self::are_comparable(a, b, op) {
                    Ok(LitTypeVariant::I32)
                }
                else {
                    panic!()
                }
            },
            _ => {
                panic!()
            }
        }
    }

    fn are_comparable(a: LitTypeVariant, b: LitTypeVariant, op: ASTOperation) -> bool {
        let compare_ops = [
            ASTOperation::AST_GTHAN, 
            ASTOperation::AST_LTHAN, 
            ASTOperation::AST_EQEQ, 
            ASTOperation::AST_NEQ
        ];

        if compare_ops.contains(&op) {
            [
                a == b,
                is_type_coalescing_possible(a.clone(), b.clone()),
                is_type_coalescing_possible(b, a)
            ]
            .iter()
            .any(|c| *c)
        }
        else {
            false
        }
    }

    pub fn is_coalesciable_both_ways(src: LitTypeVariant, dest: LitTypeVariant) -> bool {
        TypeChecker::is_type_coalesciable(src.clone(), dest.clone()) 
        && TypeChecker::is_type_coalesciable(dest, src)
    }

    pub fn is_type_coalesciable(src: LitTypeVariant, dest: LitTypeVariant) -> bool {
        match src {
            LitTypeVariant::U8 => matches!(dest, LitTypeVariant::U8 | LitTypeVariant::I16 | LitTypeVariant::I32 | LitTypeVariant::I64),
            LitTypeVariant::I16 => matches!(dest, LitTypeVariant::I16 | LitTypeVariant::I32 | LitTypeVariant::I64),
            LitTypeVariant::I32 => matches!(dest, LitTypeVariant::I32 | LitTypeVariant::I64),
            LitTypeVariant::RawStr => matches!(dest, LitTypeVariant::PoolStr),
            LitTypeVariant::PoolStr => matches!(dest, LitTypeVariant::RawStr),
            LitTypeVariant::Record { name: rec_name } => {
                match dest {
                    LitTypeVariant::Record { name } => name == rec_name,
                    _ => false
                }
            }
            _ => false
        }
    }

    pub fn is_callable(sym: &Symbol) -> bool {
        sym.sym_type == SymbolType::Function
    }
}