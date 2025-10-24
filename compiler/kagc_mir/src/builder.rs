// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;
use std::collections::HashSet;

use indexmap::IndexMap;
use kagc_const::pool::PoolIdx;
use kagc_symbol::StorageClass;
use kagc_utils::bug;

use crate::function::*;
use crate::instruction::{IRAddress, IRCondition};
use crate::module::MirModule;
use crate::types::*;
use crate::block::*;
use crate::instruction::IRInstruction;
use crate::value::{IRValue, IRValueId};

#[derive(Debug, Default)]
pub struct IRBuilder {
    current_function: Option<FunctionId>,
    current_block: Option<BlockId>,

    function_signatures: HashMap<FunctionId, FunctionSignature>,
    function_blocks: IndexMap<FunctionId, IndexMap<BlockId, IRBasicBlock>>,
    function_anchors: IndexMap<FunctionId, FunctionAnchor>,

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
    pub fn create_function(
        &mut self, 
        name: String, 
        params: Vec<FunctionParam>, 
        return_type: IRType,
        class: StorageClass
    ) -> FunctionAnchor {
        let id = self.next_function_id();
        let func_signature = FunctionSignature { 
            params, 
            return_type,
            class
        }; 
        self.function_names.insert(id, name);
        self.function_signatures.insert(id, func_signature); 
        self.function_blocks.insert(id, IndexMap::new()); 
        self.current_function = Some(id); 

        // function's entry block
        let entry_block = self.create_block("function-entry"); 
        // function's exit block
        let exit_block = self.create_block("function-exit");
        self.function_anchors.insert(id, FunctionAnchor::new(id, entry_block, exit_block));
        
        self.switch_to_block(entry_block);
        FunctionAnchor::new(id, entry_block, exit_block)
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
            bug!("cannot create a block outside function")
        }
    }

    pub fn switch_to_block(&mut self, block_id: BlockId) {
        if !self.block_instructions.contains_key(&block_id) {
            bug!("cannot switch to a non-existing block");
        }
        self.current_block = Some(block_id);
    }

    pub fn set_terminator(&mut self, bid: BlockId, term: Terminator) {
        if !self.block_instructions.contains_key(&bid) {
            bug!("cannot set a terminator for a non-existing block");
        }
        self.block_terminators.insert(bid, term);
    }

    pub fn has_terminator(&self, bid: BlockId) -> bool {
        self.block_terminators.contains_key(&bid)
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
        if !self.has_terminator(continuation) {
            // The block is still open — safe to keep emitting instructions.
            continuation
        } 
        else {
            // The block is already terminated — create a new block so that
            // subsequent lowering has a valid place to insert new instructions.
            let new_block = self.create_block("continuation block");
            self.link_blocks(continuation, new_block);
            new_block
        }
    }

    pub fn link_blocks(&mut self, from: BlockId, to: BlockId) {
        if !self.block_instructions.contains_key(&from) || !self.block_instructions.contains_key(&to) {
            bug!("cannot link non-existing blocks");
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
        let block_id = self.current_block.unwrap_or_else(|| bug!("no active block to insert into"));

        if self.block_terminators.contains_key(&block_id) {
            bug!("cannot add instructions after a terminator is set in the block({})", block_id.0);
        }

        let value_id = instruction.get_value_id();
        self.block_instructions
            .get_mut(&block_id)
            .unwrap_or_else(|| bug!("block not found"))
            .push(instruction);

        value_id
    }

    pub fn occupy_value_id(&mut self) -> IRValueId {
        self.next_value_id()
    }

    pub fn create_label(&mut self) -> BlockId {
        self.next_block_id()
    }

    pub fn create_function_parameter(&mut self, ty: IRType) -> FunctionParam {
        FunctionParam { 
            id: self.next_value_id(), 
            ty
        }
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

    pub fn create_memory_allocation(&mut self, size: IRValue, ob_type: IRValue, pool_idx: PoolIdx) -> IRValueId {
        let result = self.next_value_id();
        self.inst(IRInstruction::MemAlloc { size, ob_ty: ob_type, result, pool_idx })
            .unwrap_or_else(|| bug!("cannot create memory allocation instruction"))
    }

    pub fn build(&mut self) -> MirModule {
        let mut module = MirModule::new();
        let mut func_stack_size = 0;

        for (func_id, func_blocks) in &mut self.function_blocks {
            let func_anchor = self
                .function_anchors
                .get(func_id)
                .unwrap_or_else(|| bug!("function is not anchored"));

            for (block_id, block_instrs) in self.block_instructions.iter() {
                let block_terminator = self
                    .block_terminators
                    .get(block_id)
                    .cloned()
                    .unwrap_or({ 
                        Terminator::Return { 
                                value: None, 
                                target: func_anchor.exit_block
                            }
                        }
                    );
                    
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

            if let (Some(signature), Some(anchor)) = (
                self.function_signatures.get(func_id),
                self.function_anchors.get(func_id)
            ) {
                func_stack_size += signature.params.len() * 8; // each param accounts for 8 bytes of space
                let function = IRFunction {
                    signature: signature.clone(),
                    id: *func_id,
                    name: self.function_names.get(func_id).unwrap().clone(),
                    blocks: func_blocks.clone(),
                    entry_block: anchor.entry_block,
                    exit_block: anchor.exit_block,
                    frame_info: FunctionFrame { size: func_stack_size },
                    is_leaf: false
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
        let curr_func_id = self.current_function.unwrap_or_else(|| bug!("not active function"));
        let func_blocks = self.function_blocks.get(&curr_func_id).unwrap();
        func_blocks.get(&block_id)
    }

    pub fn get_block_unchecked(&self, block_id: BlockId) -> &IRBasicBlock {
        self.get_block(block_id).unwrap_or_else(|| bug!("no block found in current function with the ID {block_id:?}"))
    }

    pub fn get_block_mut(&mut self, block_id: BlockId) -> Option<&mut IRBasicBlock> {
        let curr_func_id = self.current_function.unwrap_or_else(|| bug!("not active function"));
        let func_blocks = self.function_blocks.get_mut(&curr_func_id).unwrap();
        func_blocks.get_mut(&block_id)
    }

    pub fn get_block_mut_unchecked(&mut self, block_id: BlockId) -> &mut IRBasicBlock {
        self.get_block_mut(block_id).unwrap_or_else(|| bug!("no block found in current function with the ID {block_id:?}"))
    }

    pub fn current_block_id(&self) -> Option<BlockId> {
        let curr_block = self.current_block.unwrap_or_else(|| bug!("no active block"));
        if self.block_instructions.contains_key(&curr_block) {
            Some(curr_block)
        }
        else {
            None
        }
    }

    pub fn current_block_id_unchecked(&self) -> BlockId {
        self.current_block_id().unwrap_or_else(|| bug!("no current block set"))
    }

    pub fn current_block_mut(&mut self) -> Option<&mut IRBasicBlock> {
        let curr_func_id = self.current_function.unwrap_or_else(|| bug!("not active function"));
        let func_blocks = self.function_blocks.get_mut(&curr_func_id).unwrap();
        let curr_block = self.current_block.unwrap_or_else(|| bug!("no active block"));
        func_blocks.get_mut(&curr_block)
    }

    pub fn current_block_mut_unchecked(&mut self) -> &mut IRBasicBlock {
        self.current_block_mut().unwrap_or_else(|| bug!("no current block set"))
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

    use kagc_symbol::StorageClass;

    use crate::block::{BlockId, Terminator};
    use crate::builder::IRBuilder;
    use crate::instruction::IRInstruction;
    use crate::types::IRType;
    use crate::value::{IRValue, IRValueId};

    #[test]
    fn test_builder() {
        let mut b = IRBuilder::default();
        let fn_ctx = b.create_function("add".to_owned(), vec![], IRType::I64, StorageClass::GLOBAL);
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

        b.set_terminator(
            fn_ctx.entry_block, 
            Terminator::Return {
                value: Some(IRValueId(3)),
                target: fn_ctx.exit_block
            }
        );

        assert_eq!(b.block_terminators.len(), 1);

        let module = b.build();
        assert!(module.functions.contains_key(&fn_ctx.id));

        let func = module.functions.get(&fn_ctx.id).unwrap();
        assert_eq!(func.blocks.len(), 1);
        assert_eq!(func.entry_block, BlockId(0));
        
        assert!(func.blocks.contains_key(&fn_ctx.entry_block));

        let e_block = func.blocks.get(&fn_ctx.entry_block).unwrap();
        assert_eq!(
            e_block.terminator, 
            Terminator::Return {
                value: Some(IRValueId(3)),
                target: fn_ctx.exit_block
            }
        );

        assert_eq!(e_block.instructions.len(), 2);
    }

    #[test]
    fn test_blocks_linking() {
        let mut builder = IRBuilder::default();
        let fn_ctx = builder.create_function("test_fn".to_owned(), vec![], IRType::Void, StorageClass::GLOBAL);
        let f_bid = fn_ctx.entry_block;
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