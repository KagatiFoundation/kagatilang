// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

mod function_pass;
use std::fmt::Debug;

pub use function_pass::*;

pub mod constant_folder;
pub mod dce;

use kagc_mir::function::IrFunction;
use kagc_mir::mir_verifier::IrVerifier;

use crate::constant_folder::ConstantFolder;
use crate::dce::DeadCodeElimination;

#[derive(Default)]
pub struct OptimizationPipeline {
	passes: Vec<Box<dyn FunctionPass>>
}

impl Debug for OptimizationPipeline {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "OptimizationPipeline")
	}
}

impl OptimizationPipeline {
	pub fn apply(&mut self, func: &mut IrFunction) {
		for pass in &mut self.passes {
			pass.run_on_function(func);

			IrVerifier::verify_function(func);
		}
	}

	pub fn add_optimization_pass(&mut self, opti_pass: Box<dyn FunctionPass>) {
		self.passes.push(opti_pass);
	}

	pub fn standard_pipeline() -> Self {
		Self {
			passes: vec![
				Box::new(ConstantFolder::default()),
				Box::new(DeadCodeElimination) // DCE pass should run after other optimization passes
			]
		}
	}
}