// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashSet;

use indexmap::IndexMap;
use kagc_symbol::StorageClass;
use kagc_types::TyKind;
use kagc_utils::bug;

use crate::function::*;
use crate::types::*;
use crate::block::*;
use crate::instruction::IrCondition;
use crate::instruction::IrInstruction;
use crate::instruction::IrLocation;
use crate::module::MirModule;
use crate::value::{IrValue, IrValueId};

#[derive(Debug, Default)]
pub struct IrBuilder<'tcx> {
    current_function: Option<IrFunctionId>,
    current_block: Option<BlockId>,

    pub functions: IndexMap<IrFunctionId, BuilderFunction<'tcx>>,

    function_id: usize,
    block_id: usize,
    value_id: usize,
}

#[derive(Debug)]
pub struct BuilderFunction<'tcx> {
    pub name: String,
    pub signature: IrFunctionSignature<'tcx>,
    pub anchor: IrFunctionAnchor,
    pub blocks: IndexMap<BlockId, BuilderBlock>,
	reserved_blocks: IndexMap<BlockId, BuilderBlock>
}

#[derive(Debug, Clone)]
pub struct BuilderBlock {
    pub name: String,
    pub instructions: Vec<IrInstruction>,
    pub terminator: Option<Terminator>,
    pub successors: HashSet<BlockId>,
    pub predecessors: HashSet<BlockId>,
}

impl<'tcx> IrBuilder<'tcx> {
    pub fn create_function(
        &mut self, 
        name: String, 
        params: Vec<(&'tcx str, TyKind<'_>)>, 
        return_type: IrType,
        class: StorageClass,
    ) -> IrFunctionContext {
		self.reset_internal_state();

        let function_id = self.next_function_id();
        self.current_function = Some(function_id);

        let entry_block = self.next_block_id();
        let exit_block = self.next_block_id();

        let anchor = IrFunctionAnchor::new(function_id, entry_block, exit_block);
		let mut function_ctx = IrFunctionContext::new(anchor);

        self.current_block = Some(entry_block);

		let mut function_params = vec![];

		let mut param_instrs = vec![];

		for (index, (name, ty)) in params.iter().enumerate() {
			let param_var_id = function_ctx.map_var(name.to_string());

			function_params.push(
				IrFunctionParam {
					id: param_var_id,
					ty: IrType::from(*ty),
					name
				}
			);

			param_instrs.push(IrInstruction::Param { index, var_id: param_var_id });
		}

        let signature = IrFunctionSignature { 
			params: function_params.clone(), 
			return_type, 
			class
		};
        
        let mut blocks = IndexMap::new();
        
        blocks.insert(entry_block, BuilderBlock::new("function-entry"));
        // blocks.insert(exit_block, BuilderBlock::new("function-exit"));

        let builder_func = BuilderFunction {
            name,
            signature,
            anchor,
            blocks,
			reserved_blocks: IndexMap::new()
        };

        self.functions.insert(function_id, builder_func);

		param_instrs
			.iter()
			.for_each(|inst| { self.inst(inst.clone()); });

		function_ctx
    }

	pub fn reset_internal_state(&mut self) {
		self.block_id = 0;
	}

    pub fn create_block(&mut self, name: &str) -> BlockId {
        let fid = self.current_fn_id_unchecked();
        let bid = self.next_block_id();
        
        let func = self.functions.get_mut(&fid).unwrap();
        func.blocks.insert(bid, BuilderBlock::new(name));
        
        self.current_block = Some(bid);
        bid
    }

	pub fn reserve_block(&mut self, name: &str) -> BlockId {
        let fid = self.current_fn_id_unchecked();
        let bid = self.next_block_id();
        
        let func = self.functions.get_mut(&fid).unwrap();
		func.reserved_blocks.insert(bid, BuilderBlock::new(name));
        
        bid
	}

	pub fn commit_block(&mut self, bid: BlockId) {
        let fid = self.current_fn_id_unchecked();

        let func = self.functions.get_mut(&fid).unwrap();
		let block = func
			.reserved_blocks
			.shift_remove(&bid)
			.unwrap_or_else(|| bug!("block to be committed doesn't exist"));

		func.blocks.insert(bid, block.clone());
	}

	pub fn commit_and_switch_block(&mut self, bid: BlockId) {
		self.commit_block(bid);
		self.switch_to_block(bid);
	}

	pub fn remove_block(&mut self, bid: BlockId) {
        let fid = self.current_fn_id_unchecked();

        let func = self.functions.get_mut(&fid).unwrap();
        func.blocks.shift_remove(&bid);
	}

    pub fn inst(&mut self, instruction: IrInstruction) -> Option<IrValueId> {
        let fid = self.current_fn_id_unchecked();
        let bid = self.current_block_id_unchecked();
        
        let func = self.functions.get_mut(&fid).unwrap();
        let block = func.blocks.get_mut(&bid).unwrap();

        if block.terminator.is_some() {
            bug!("Cannot append instructions to block ({:?}) after terminator is set", bid);
        }

        let value_id = instruction.get_value_id();
        block.instructions.push(instruction);
        value_id
    }

    pub fn set_terminator(&mut self, bid: BlockId, term: Terminator) {
        let fid = self.current_fn_id_unchecked();
        let func = self.functions.get_mut(&fid).unwrap();
        let block = func.blocks.get_mut(&bid).unwrap();
        block.terminator = Some(term);
    }

	pub fn link_blocks(&mut self, from: BlockId, to: BlockId) {
        let fid = self.current_fn_id_unchecked();
        let func = self.functions.get_mut(&fid).unwrap();

		if let Some(b) = func.blocks.get_mut(&from) {
			b.successors.insert(to);
		}
		else if let Some(b) = func.reserved_blocks.get_mut(&from) {
			b.successors.insert(to);
		}

		if let Some(b) = func.blocks.get_mut(&to) {
			b.predecessors.insert(from);
		}
		else if let Some(b) = func.reserved_blocks.get_mut(&to) {
			b.predecessors.insert(from);
		}
    }

   	pub fn switch_to_block(&mut self, block_id: BlockId) {
    	let fid = self.current_fn_id_unchecked();
    	let func = self.functions.get(&fid).unwrap_or_else(|| bug!("active function not found in builder"));

    	if !func.blocks.contains_key(&block_id) {
        	bug!("cannot switch to a non-existing block ({:?}) in function {:?}", block_id, fid);
    	}

    	self.current_block = Some(block_id);
	}	

	pub fn has_terminator(&self, block_id: BlockId) -> bool {
    	let fid = self.current_fn_id_unchecked();
    	let func = self.functions.get(&fid).unwrap_or_else(|| bug!("active function not found in builder"));

		let block = func.blocks.iter().find(|b| *b.0 == block_id).expect("block not found");
		block.1.has_terminator()
	}

    /// Ensures that control flow can continue from the given block.
    ///
    /// This function checks whether the provided `continuation` block
    /// already has a terminator instruction (e.g., `Jump`, `Return`, etc.).
    ///
    /// - If the block **does not** have a terminator, it means control flow
    ///   naturally continues from this block, so we can keep emitting
    ///   instructions into it.
    ///
    /// - If the block **does** have a terminator, control flow for that block
    ///   has already ended. In that case, a new block is created and linked
    ///   from the terminated one to represent the next valid insertion point.
    ///
    /// This is useful in statement lowering (especially in loops or conditionals)
    /// where some statements may end the current block (like `return` or `break`)
    /// while others don't. It guarantees that lowering always has a valid,
    /// “active” block to continue emitting into.
    pub fn ensure_continuation_block(&mut self, continuation: BlockId) -> BlockId {
        let fid = self.current_fn_id_unchecked();

        let func = self
			.functions
			.get(&fid)
			.unwrap_or_else(|| bug!("Active function missing"));

        let block = func.blocks.get(&continuation).unwrap_or_else(|| {
            bug!("Continuation block {:?} does not exist in function {:?}", continuation, fid)
        });

		if !block.has_terminator() { return continuation; }

        let new_block = self.create_block("continuation block");
		self.set_terminator(continuation, Terminator::Jump(new_block));
        self.link_blocks(continuation, new_block);
        self.current_block = Some(new_block);
        new_block
    }

    pub fn link_blocks_multiple(&mut self, from: BlockId, tos: Vec<BlockId>) {
        for to in tos {
            self.link_blocks(from, to);
        }
    }

    pub fn occupy_value_id(&mut self) -> IrValueId {
        self.next_value_id()
    }

    pub fn create_label(&mut self) -> BlockId {
        self.next_block_id()
    }

    pub fn create_add(&mut self, lhs: IrValue, rhs: IrValue) -> IrValueId {
        let result = self.next_value_id();
        self.inst(IrInstruction::Add { result, lhs, rhs })
            .expect("create_add: no value ID created")
    }

    pub fn create_subtract(&mut self, lhs: IrValue, rhs: IrValue) -> IrValueId {
        let result = self.next_value_id();
        self.inst(IrInstruction::Subtract { result, lhs, rhs })
            .expect("create_subtract: no value ID created")
    }

    pub fn create_multiply(&mut self, lhs: IrValue, rhs: IrValue) -> IrValueId {
        let result = self.next_value_id();
        self.inst(IrInstruction::Multiply { result, lhs, rhs })
            .expect("create_multiply: no value ID created")
    }
    
    pub fn create_divide(&mut self, lhs: IrValue, rhs: IrValue) -> IrValueId {
        let result = self.next_value_id();
        self.inst(IrInstruction::Divide { result, lhs, rhs })
            .expect("create_divide: no value ID created")
    }

	pub fn create_negate(&mut self, value: IrValue) -> IrValueId {
        let result = match value {
			IrValue::Register(r) => r,
			_ => self.next_value_id(),
		};

        self.inst(IrInstruction::Neg { result, value })
            .expect("create_negate: no value ID created")
	}

	pub fn create_conditional_eqeq(
		&mut self,
		lhs: IrValue,
		rhs: IrValue
	) -> IrValueId {
		self.create_conditional(IrCondition::EqEq, lhs, rhs)
	}

	pub fn create_conditional_neq(
		&mut self,
		lhs: IrValue,
		rhs: IrValue
	) -> IrValueId {
		self.create_conditional(IrCondition::NEq, lhs, rhs)
	}

	pub fn create_conditional_lthan(
		&mut self,
		lhs: IrValue,
		rhs: IrValue
	) -> IrValueId {
		self.create_conditional(IrCondition::LThan, lhs, rhs)
	}
	
	pub fn create_conditional_gthan(
		&mut self,
		lhs: IrValue,
		rhs: IrValue
	) -> IrValueId {
		self.create_conditional(IrCondition::GThan, lhs, rhs)
	}

	pub fn create_conditional(
		&mut self,
		condition: IrCondition,
		lhs: IrValue,
		rhs: IrValue,
	) -> IrValueId {
        let result = self.next_value_id();
		self.inst(
			IrInstruction::Cmp { lhs, rhs, result, condition }
		).expect("create_conditional_internal: no value ID created")
	}

    pub fn create_conditional_jump(
		&mut self,
		cond: IrCondition,
		lhs: IrValue,
		rhs: IrValue,
		then_block: BlockId,
		else_block: BlockId
	) -> IrValueId {
        self.inst(IrInstruction::CondJump { cond, lhs, rhs, then_block, else_block })
            .expect("create_conditional_jump: no value ID created")
    }

	pub fn create_jump(&mut self, jump_block_id: BlockId) {
		self.inst(
			IrInstruction::Jump { block: jump_block_id }
		);
	}

    pub fn create_move(&mut self, value: IrValue) -> IrValueId {
        let result = self.next_value_id();
        self.inst(IrInstruction::Mov { result, src: value })
            .expect("create_move: no value ID created")
    }

    pub fn create_load(&mut self, location: IrLocation) -> IrValueId {
        let result = self.next_value_id();
        self.inst(IrInstruction::Load { location, result })
            .expect("create_load: no value ID created")
    }

	pub fn create_store(&mut self, src: IrValueId, location: IrLocation) -> IrValueId {
        self.inst(IrInstruction::Store { src, location })
            .expect("create_move: no value ID created");
		src
    }

    pub fn create_load_const(&mut self, pool_index: usize) -> IrValueId {
        let result = self.next_value_id();
        self.inst(IrInstruction::LoadConst { pool_idx: pool_index, result })
            .expect("create_load_const: no value ID created")
    }

    pub fn build(self) -> MirModule<'tcx> {
    	let mut module = MirModule::new();

    	for (func_id, b_func) in self.functions {
        	let mut finalized_blocks = IndexMap::new();

        	for (block_id, b_block) in b_func.blocks {
				let terminator = b_block.terminator.unwrap_or_else(|| {
    				if b_func.signature.class == StorageClass::EXTERN {
    					Terminator::Fallthrough(BlockId(0))
    				}
					else {
        				bug!("block '{block_id:#?}' doesn't have a terminator set ({})", b_func.name);
					}
				});

            	let ir_block = IrBasicBlock {
                	id: block_id,
                	instructions: b_block.instructions,
                	successors: b_block.successors,
                	predecessors: b_block.predecessors,
                	terminator,
                	name: b_block.name,
            	};
            	finalized_blocks.insert(block_id, ir_block);
        	}

        	let function = IrFunction {
            	signature: b_func.signature,
            	id: func_id,
            	name: b_func.name.clone(),
            	blocks: finalized_blocks,
            	entry_block: b_func.anchor.entry_block,
            	exit_block: b_func.anchor.exit_block,
            	is_leaf: false,
        	};

        	module.add_function(function);
    	}

    	module
	}

    fn next_function_id(&mut self) -> IrFunctionId {
        let fid = self.function_id;
        self.function_id += 1;
        IrFunctionId(fid)
    }

    fn next_block_id(&mut self) -> BlockId {
        let bid = self.block_id;
        self.block_id += 1;
        BlockId(bid)
    }

    fn next_value_id(&mut self) -> IrValueId {
        let vid = self.value_id;
        self.value_id += 1;
        IrValueId(vid)
    }

    pub fn current_block_id(&self) -> Option<BlockId> {
		self.current_function.and_then(|fid| {
			self.current_block.and_then(|bid| {
				self
					.functions
					.get(&fid)
					.unwrap()
					.blocks
					.contains_key(&bid)
					.then_some(bid)
			})
		})
    }

    pub fn current_block_id_unchecked(&self) -> BlockId {
        self
			.current_block_id()
            .unwrap_or_else(|| bug!("No active basic block set in the current context"))
    }

    pub fn current_block_mut(&mut self) -> Option<&mut BuilderBlock> {
        let fid = self.current_function?;
        let bid = self.current_block?;
        
        let func = self.functions.get_mut(&fid)?;
        func.blocks.get_mut(&bid)
    }

    pub fn current_block_mut_unchecked(&mut self) -> &mut BuilderBlock {
        let fid = self.current_fn_id_unchecked();
        let bid = self.current_block
            .unwrap_or_else(|| bug!("No active block context"));

        self.functions
            .get_mut(&fid)
            .and_then(|func| func.blocks.get_mut(&bid))
            .unwrap_or_else(|| bug!("Active block {:?} not found inside function {:?}", bid, fid))
    }

	fn current_fn_id_unchecked(&self) -> IrFunctionId {
		self.current_function.expect("No active function context")
	}
}

impl BuilderBlock {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            instructions: Vec::new(),
            terminator: None,
            successors: HashSet::new(),
            predecessors: HashSet::new(),
        }
    }

	pub fn has_terminator(&self) -> bool {
		self.terminator.is_some()
	}
}