// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;
use std::collections::HashSet;

use indexmap::IndexMap;

use crate::function::*;
use crate::instruction::IRAddress;
use crate::instruction::IRCondition;
use crate::module::Module;
use crate::types::*;
use crate::block::*;
use crate::instruction::IRInstruction;
use crate::value::IRValue;
use crate::value::IRValueId;

#[derive(Debug, Default)]
pub struct IRBuilder {
    current_function: Option<FunctionId>,
    current_block: Option<BlockId>,

    pub function_signatures: HashMap<FunctionId, FunctionSignature>,
    pub function_blocks: IndexMap<FunctionId, IndexMap<BlockId, IRBasicBlock>>,
    entry_blocks: HashMap<FunctionId, BlockId>,

    block_instructions: HashMap<BlockId, Vec<IRInstruction>>,
    block_terminators: HashMap<BlockId, Terminator>,

    block_successors: HashMap<BlockId, HashSet<BlockId>>,
    block_predecessors: HashMap<BlockId, HashSet<BlockId>>,

    block_names: HashMap<BlockId, &'static str>,
    function_names: HashMap<FunctionId, String>,

    function_id: usize,
    block_id: usize,
    value_id: usize,
}

impl IRBuilder {
    pub fn create_function(&mut self, name: String, params: Vec<FunctionParam>, return_type: IRType) -> (FunctionId, BlockId) {
        let id = self.next_function_id();
        let func_signature = FunctionSignature { 
            params, 
            return_type 
        }; 
        self.function_names.insert(id, name);
        self.function_signatures.insert(id, func_signature); 
        self.function_blocks.insert(id, IndexMap::new()); 
        self.current_function = Some(id); 
        let entry_block = self.create_block("function"); 
        self.entry_blocks.insert(id, entry_block); 
        (id, entry_block)
    }

    pub fn create_block(&mut self, name: &'static str) -> BlockId {
        if self.current_function.is_some() {
            let bid = self.next_block_id();
            self.current_block = Some(bid);
            self.block_instructions.insert(bid, vec![]);
            self.block_successors.insert(bid, HashSet::new());
            self.block_predecessors.insert(bid, HashSet::new());
            self.block_names.insert(bid, name);
            bid
        }
        else {
            panic!("create_block: Cannot create a block outside function")
        }
    }

    pub fn switch_to_block(&mut self, block_id: BlockId) {
        if !self.block_instructions.contains_key(&block_id) {
            panic!("switch_to_block: Cannot switch to a non-existing block");
        }
        self.current_block = Some(block_id);
    }

    pub fn set_terminator(&mut self, bid: BlockId, term: Terminator) {
        if !self.block_instructions.contains_key(&bid) {
            panic!("set_terminator: Cannot set a terminator for a non-existing block");
        }
        self.block_terminators.insert(bid, term);
    }

    pub fn link_blocks(&mut self, from: BlockId, to: BlockId) {
        if !self.block_instructions.contains_key(&from) || !self.block_instructions.contains_key(&to) {
            panic!("link_blocks: Cannot link non-existing blocks");
        }
        self.block_successors.get_mut(&from).unwrap().insert(to);
        self.block_predecessors.get_mut(&to).unwrap().insert(from);
    }

    pub fn link_blocks_multiple(&mut self, from: BlockId, tos: Vec<BlockId>) {
        for to in tos {
            self.link_blocks(from, to);
        }
    }

    pub fn inst(&mut self, instruction: IRInstruction) -> Option<IRValueId> {
        let block_id = self.current_block.expect("inst: No active block to insert into");

        if self.block_terminators.contains_key(&block_id) {
            panic!("inst: Cannot add instructions after a terminator is set in the block({})", block_id.0);
        }

        let value_id = instruction.get_value_id();
        self.block_instructions
            .get_mut(&block_id)
            .expect("inst: Block not found")
            .push(instruction);

        value_id
    }

    pub fn create_add(&mut self, lhs: IRValue, rhs: IRValue) -> IRValueId {
        let result = self.next_value_id();
        self.inst(IRInstruction::Add { result, lhs, rhs })
            .expect("create_add: no value ID created")
    }

    pub fn create_subtract(&mut self, lhs: IRValue, rhs: IRValue) -> IRValueId {
        let result = self.next_value_id();
        self.inst(IRInstruction::Subtract { result, lhs, rhs })
            .expect("create_subtract: no value ID created")
    }

    pub fn create_multiply(&mut self, lhs: IRValue, rhs: IRValue) -> IRValueId {
        let result = self.next_value_id();
        self.inst(IRInstruction::Multiply { result, lhs, rhs })
            .expect("create_multiply: no value ID created")
    }
    
    pub fn create_divide(&mut self, lhs: IRValue, rhs: IRValue) -> IRValueId {
        let result = self.next_value_id();
        self.inst(IRInstruction::Divide { result, lhs, rhs })
            .expect("create_divide: no value ID created")
    }

    pub fn create_conditional_jump(&mut self, cond: IRCondition, lhs: IRValue, rhs: IRValue) -> IRValueId {
        let result = self.next_value_id();
        self.inst(IRInstruction::CondJump { result, cond, lhs, rhs })
            .expect("create_conditional_jump: no value ID created")
    }

    pub fn create_move(&mut self, value: IRValue) -> IRValueId {
        let result = self.next_value_id();
        self.inst(IRInstruction::Mov { result, src: value })
            .expect("create_move: no value ID created")
    }

    pub fn create_load(&mut self, addr: IRAddress) -> IRValueId {
        let result = self.next_value_id();
        self.inst(IRInstruction::Load { src: addr, result })
            .expect("create_move: no value ID created")
    }

    pub fn build(&mut self) -> Module {
        let mut module = Module::new();
        let mut func_stack_size = 0;

        for (func_id, func_blocks) in &mut self.function_blocks {
            for (block_id, block_instrs) in self.block_instructions.iter() {
                let block_terminator = self.block_terminators.get(block_id).unwrap_or(&Terminator::Return(None));
                if self.block_instructions.contains_key(block_id) {
                    let ir_block = IRBasicBlock { 
                        id: *block_id, 
                        instructions: block_instrs.clone(), 
                        successors: self.block_successors.get(block_id).unwrap().clone(), 
                        predecessors: self.block_predecessors.get(block_id).unwrap().clone(),
                        terminator: block_terminator.clone(),
                        name: self.block_names.get(block_id).unwrap().to_string()
                    };
                    func_stack_size += IRBuilder::calculate_block_stack_usage(&ir_block);
                    func_blocks.insert(
                        *block_id,
                        ir_block
                    );
                }
            }

            if let (Some(signature), Some(entry_block)) = (
                self.function_signatures.get(func_id),
                self.entry_blocks.get(func_id)
            ) {
                func_stack_size += signature.params.len() * 8; // each param accounts for 8 bytes of space
                let function = IRFunction {
                    signature: signature.clone(),
                    id: *func_id,
                    name: self.function_names.get(func_id).unwrap().clone(),
                    blocks: func_blocks.clone(),
                    entry_block: *entry_block,
                    frame_info: FunctionFrame { size: func_stack_size }
                };
                module.add_function(function);
            }
        }
        module
    }

    // Calculate the amount of space a function's block takes.
    fn calculate_block_stack_usage(block: &IRBasicBlock) -> usize {
        let mut size = 0;
        for inst in block.instructions.iter() {
            if let IRInstruction::Store { .. } = inst {
                size += 8;
            }
        }
        size
    }

    pub fn get_block(&self, block_id: BlockId) -> Option<&IRBasicBlock> {
        let curr_func_id = self.current_function.expect("Not active function! Aborting...");
        let func_blocks = self.function_blocks.get(&curr_func_id).unwrap();
        func_blocks.get(&block_id)
    }

    pub fn get_block_unchecked(&self, block_id: BlockId) -> &IRBasicBlock {
        self.get_block(block_id).unwrap_or_else(|| panic!("No block found in current function with the ID {block_id:?}"))
    }

    pub fn get_block_mut(&mut self, block_id: BlockId) -> Option<&mut IRBasicBlock> {
        let curr_func_id = self.current_function.expect("Not active function! Aborting...");
        let func_blocks = self.function_blocks.get_mut(&curr_func_id).unwrap();
        func_blocks.get_mut(&block_id)
    }

    pub fn get_block_mut_unchecked(&mut self, block_id: BlockId) -> &mut IRBasicBlock {
        self.get_block_mut(block_id).unwrap_or_else(|| panic!("No block found in current function with the ID {block_id:?}"))
    }

    pub fn current_block_id(&self) -> Option<BlockId> {
        let curr_block = self.current_block.expect("No active block! Aborting...");
        if self.block_instructions.contains_key(&curr_block) {
            Some(curr_block)
        }
        else {
            None
        }
    }

    pub fn current_block_id_unchecked(&self) -> BlockId {
        self.current_block_id().expect("No current block set")
    }

    pub fn current_block_mut(&mut self) -> Option<&mut IRBasicBlock> {
        let curr_func_id = self.current_function.expect("Not active function! Aborting...");
        let func_blocks = self.function_blocks.get_mut(&curr_func_id).unwrap();
        let curr_block = self.current_block.expect("No active block! Aborting...");
        func_blocks.get_mut(&curr_block)
    }

    pub fn current_block_mut_unchecked(&mut self) -> &mut IRBasicBlock {
        self.current_block_mut().expect("No current block set")
    }

    fn next_function_id(&mut self) -> FunctionId {
        let fid = self.function_id;
        self.function_id += 1;
        FunctionId(fid)
    }

    fn next_block_id(&mut self) -> BlockId {
        let bid = self.block_id;
        self.block_id += 1;
        BlockId(bid)
    }

    fn next_value_id(&mut self) -> IRValueId {
        let vid = self.value_id;
        self.value_id += 1;
        IRValueId(vid)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::block::{BlockId, Terminator};
    use crate::builder::IRBuilder;
    use crate::instruction::IRInstruction;
    use crate::types::IRType;
    use crate::value::{IRValue, IRValueId};

    #[test]
    fn test_builder() {
        let mut b = IRBuilder::default();
        let (fid, entry_bid) = b.create_function("add".to_owned(), vec![], IRType::I64);
        b.inst(
            IRInstruction::Add {
                result: IRValueId(0),
                lhs: IRValue::Constant(32),
                rhs: IRValue::Constant(32)
            }
        );
        b.inst(
            IRInstruction::Mov { 
                result: IRValueId(1), 
                src: IRValue::Var(IRValueId(0)) 
            }
        );

        b.set_terminator(entry_bid, Terminator::Return(Some(IRValueId(3))));

        assert_eq!(b.block_terminators.len(), 1);

        let module = b.build();
        assert!(module.functions.contains_key(&fid));

        let func = module.functions.get(&fid).unwrap();
        assert_eq!(func.blocks.len(), 1);
        assert_eq!(func.entry_block, BlockId(0));
        
        assert!(func.blocks.contains_key(&entry_bid));

        let e_block = func.blocks.get(&entry_bid).unwrap();
        assert_eq!(e_block.terminator, Terminator::Return(Some(IRValueId(3))));

        assert_eq!(e_block.instructions.len(), 2);
    }

    #[test]
    fn test_blocks_linking() {
        let mut builder = IRBuilder::default();
        let (_, f_bid) = builder.create_function("test_fn".to_owned(), vec![], IRType::Void);
        let _ = builder.create_move(IRValue::Constant(12345)); // variable, maybe?

        let loop_id = builder.create_block("loop-test-block");
        builder.link_blocks(f_bid, loop_id);

        assert!(builder.block_successors.contains_key(&f_bid));
        assert!(builder.block_predecessors.contains_key(&loop_id));

        // function's entry block has no predecessors
        assert_eq!(builder.block_predecessors.get(&f_bid).unwrap(), &HashSet::new());
        assert_eq!(builder.block_successors.get(&f_bid).unwrap().len(), 1);

        assert_eq!(
            vec![&f_bid],
            builder.block_predecessors
                .get(&loop_id)
                .unwrap()
                .iter()
                .collect::<Vec<&BlockId>>()
        );
    }
}