use std::collections::HashMap;

use crate::function::*;
use crate::module::Module;
use crate::types::*;
use crate::block::*;
use crate::instruction::IRInstruction;
use crate::value::IRValueId;

#[derive(Debug, Default)]
pub struct IRBuilder {
    pub current_function: Option<FunctionId>,
    pub current_block: Option<BlockId>,
    pub function_signatures: HashMap<FunctionId, FunctionSignature>,
    pub function_blocks: HashMap<FunctionId, HashMap<BlockId, IRBasicBlock>>,
    pub entry_blocks: HashMap<FunctionId, BlockId>,
    pub block_instructions: HashMap<BlockId, Vec<IRInstruction>>,
    pub block_terminators: HashMap<BlockId, Terminator>,

    function_id: usize,
    block_id: usize,
}

impl IRBuilder {
    pub fn function(
        &mut self, 
        params: Vec<FunctionParam>,
        return_type: IRType
    ) -> (FunctionId, BlockId) {
        let id = self.next_function_id();
        let func_signature = FunctionSignature { 
            params, 
            return_type 
        }; 
        self.function_signatures.insert(id, func_signature); 
        self.function_blocks.insert(id, HashMap::new()); 
        self.current_function = Some(id); 
        let entry_block = self.block(); 
        self.entry_blocks.insert(id, entry_block); 
        (id, entry_block)
    }

    pub fn block(&mut self) -> BlockId {
        let bid = self.next_block_id();
        if self.current_function.is_some() {
            self.current_block = Some(bid);
            self.block_instructions.insert(bid, vec![]);
        }
        bid
    }

    pub fn set_terminator(&mut self, bid: BlockId, term: Terminator) {
        self.block_terminators.insert(bid, term);
    }

    pub fn inst(&mut self, instruction: IRInstruction) -> Option<IRValueId> {
        let block_id = self.current_block.expect("No active block to insert into");

        if self.block_terminators.contains_key(&block_id) {
            panic!("Cannot add instruction after a terminator in block {:?}", block_id);
        }

        let value_id = instruction.get_value_id();
        self.block_instructions
            .get_mut(&block_id)
            .expect("Block not found")
            .push(instruction);

        value_id
    }

    pub fn build(&mut self) -> Module {
        let mut module = Module::new();

        for (func_id, func_blocks) in &mut self.function_blocks {
            for (block_id, block_instrs) in self.block_instructions.iter() {
                let block_terminator = self.block_terminators.get(block_id).unwrap_or(&Terminator::Return(None));
                if self.block_instructions.contains_key(block_id) {
                    func_blocks.insert(
                        *block_id, 
                        IRBasicBlock { 
                            id: *block_id, 
                            instructions: block_instrs.clone(), 
                            successors: vec![], 
                            predecessors: vec![],
                            terminator: block_terminator.clone()
                        }
                    );
                }
            }

            if let (Some(signature), Some(entry_block)) = (
                self.function_signatures.get(func_id),
                self.entry_blocks.get(func_id)
            ) {
                let function = IRFunction {
                    signature: signature.clone(),
                    id: *func_id,
                    blocks: func_blocks.clone(),
                    entry_block: *entry_block
                };
                module.add_function(function);
            }
        }
        module
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
}

#[cfg(test)]
mod tests {
    use crate::block::{BlockId, Terminator};
    use crate::builder::IRBuilder;
    use crate::instruction::IRInstruction;
    use crate::types::IRType;
    use crate::value::{IRValue, IRValueId};

    #[test]
    fn test_builder() {
        let mut b = IRBuilder::default();
        let (fid, entry_bid) = b.function(vec![], IRType::I64);
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
}