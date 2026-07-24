// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::block::{BlockId, IrBasicBlock, Terminator};
use crate::function::IrFunction;

pub struct IrVerifier;

impl IrVerifier {
    pub fn verify_function(func: &IrFunction) {
        Self::verify_blocks(func);
		Self::verify_terminators(func);
    }

	fn verify_terminators(func: &IrFunction) {
		for (_, block) in &func.blocks {
			match block.terminator {
				Terminator::Fallthrough(jump_id)
				| Terminator::Jump(jump_id) => assert!(block.successors.contains(&jump_id)),
				Terminator::CondJump { then_block, else_block, .. } => {
					assert!(block.successors.contains(&then_block));
					assert!(block.successors.contains(&else_block));
				},
				Terminator::Return { .. } => {},
			}
		}
	}

	fn verify_blocks(func: &IrFunction) {
		for (_, block) in &func.blocks {
    		for succ in &block.successors {
				Self::is_a_successor(func, block, *succ);
    		}

			for pred in &block.predecessors {
				Self::is_a_predecessor(func, block, *pred);
			}
		}
	}

	fn is_a_successor(func: &IrFunction, of: &IrBasicBlock, target: BlockId) {
		let target_block = func
			.blocks
			.get(&target)
			.unwrap_or_else(|| {
				panic!(
					"block {:?} references missing successor {:?}",
					of.id,
					target
				)
			});

		assert!(target_block.predecessors.contains(&of.id));
	}

	fn is_a_predecessor(func: &IrFunction, of: &IrBasicBlock, target: BlockId) {
		let target_block = func
			.blocks
			.get(&target)
			.unwrap_or_else(|| {
				panic!(
					"block {:?} references missing predecessor {:?}",
					of.id,
					target
				)
			});

		assert!(target_block.successors.contains(&of.id));
	}
}