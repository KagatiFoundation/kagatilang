// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_ast::*;
use kagc_types::TyKind;
use kagc_symbol::SymTable;

pub fn fold_constants<'tcx>(node: AstNode<'tcx>, sym_table: &'tcx SymTable<'tcx>) -> AstNode<'tcx> {
    match node.kind {
        NodeKind::ExprAST(expr_ast) => fold_expr(expr_ast, node.meta, sym_table),
        _ => node,
    }
}

fn fold_expr<'tcx>(expr: Expr<'tcx>, meta: NodeMeta, sym_table: &'tcx SymTable<'tcx>) -> AstNode<'tcx> {
	match expr {
		Expr::Binary(bin_expr) => fold_binary_expr(bin_expr, meta, sym_table),
		Expr::Ident(ident_expr) => fold_ident_expr(ident_expr, meta, sym_table),
		_ => todo!(),
	}
}

fn fold_ident_expr<'tcx>(ident: IdentExpr<'tcx>, meta: NodeMeta, sym_table: &'tcx SymTable<'tcx>) -> AstNode<'tcx> {
    if let Some(symbol) = sym_table.get(ident.sym_name) {
        return AstNode::leaf(
			NodeId(1),
			NodeKind::ExprAST(
				Expr::LitVal(
					LitValExpr { 
						value: Literal::I64(5),
						ty: TyKind::I64
					}
				)
			),
			AstOp::Add,
			None,
			meta
		);
    }
	todo!()
}

fn fold_binary_expr<'tcx>(bin: BinExpr<'tcx>, meta: NodeMeta, sym_table: &'tcx SymTable<'tcx>) -> AstNode<'tcx> {
	let op = bin.operation;
	let folded_left_node = fold_expr(*bin.left, meta.clone(), sym_table);
	let folded_right_node = fold_expr(*bin.right, meta.clone(), sym_table);

	let left = folded_left_node.as_expr_owned().unwrap();
	let right = folded_right_node.as_expr_owned().unwrap();

    if let (Expr::LitVal(l_lit), Expr::LitVal(r_lit)) = (&left, &right) {
        if let (Literal::I64(l_val), Literal::I64(r_val)) = (&l_lit.value, &r_lit.value) {
            let folded_val = match op {
                AstOp::Add => Some(l_val + r_val),
                AstOp::Subtract => Some(l_val - r_val),
                AstOp::Multiply => Some(l_val * r_val),
                AstOp::Divide if *r_val != 0 => Some(l_val / r_val),
                _ => None, 
            };

            if let Some(val) = folded_val {
                return AstNode::leaf(
                    NodeId(1),
                    NodeKind::ExprAST(
						Expr::LitVal(
							LitValExpr {
								value: Literal::I64(val),
								ty: TyKind::I64,
                        	}
						)
					),
                    AstOp::IntLit,
                    None,
                    meta,
                );
            }
        }
    }

    AstNode::leaf(
        NodeId(1),
        NodeKind::ExprAST(Expr::Binary(BinExpr {
            operation: op,
            left: Box::new(left),
            right: Box::new(right),
            ty: TyKind::None,
        })),
        op,
        None,
        meta,
    )
}