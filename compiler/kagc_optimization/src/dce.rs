// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::FunctionPass;

use kagc_mir::function::IrFunction;

#[derive(Debug, Default)]
pub struct DeadCodeElimination;

impl FunctionPass for DeadCodeElimination {
	fn name(&self) -> &'static str {
		"Dead Code Elimination (DCE)"
	}

	fn run_on_function(&mut self, func: &mut IrFunction) -> bool {
		self.prune_dead_code(func);
		true
	}
}

impl DeadCodeElimination {
	fn prune_dead_code(&self, func: &mut IrFunction) {
		let liveness_data = kagc_mir::liveness::compute_function_live_ranges(func);

		for (block_id, block) in &mut func.blocks {
			let mut currently_live = liveness_data[block_id].out_set.clone();
			let mut optimized_instructions = Vec::new();

			currently_live.extend(block.terminator.uses());

			for inst in block.instructions.drain(..).rev() {
				let (uses, defs) = inst.uses_and_defs();

				let mut is_dead = false;
				for defined_reg in &defs {
					if !currently_live.contains(defined_reg) {
						// nobody reads this value id! it is practically dead.
						is_dead = true;
					}
				}

				if is_dead {
					continue;
				}

				for d in defs { currently_live.remove(&d); }
				for u in uses { currently_live.insert(u); }

				optimized_instructions.push(inst);
			}

			optimized_instructions.reverse();
        	block.instructions = optimized_instructions;
		}
	}
}