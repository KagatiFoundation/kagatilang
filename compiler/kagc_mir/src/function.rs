// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use indexmap::IndexMap;
use kagc_symbol::StorageClass;

use crate::block::IrBasicBlock;
use crate::block::BlockId;

use crate::loop_ctx::IrLoopContext;
use crate::types::IrType;
use crate::variable::IrVariableId;

use std::collections::HashMap;

#[derive(Default, Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub struct IrFunctionId(pub usize);

#[derive(Default, Debug, Clone, Copy)]
pub struct IrFunctionParam {
    pub id: IrVariableId,
    pub ty: IrType
}

#[derive(Default, Debug, Clone)]
pub struct IrFunctionSignature {
    pub params: Vec<IrFunctionParam>,
    pub return_type: IrType,
    pub class: StorageClass
}

#[derive(Default, Debug)]
pub struct IrFunction {
    pub id: IrFunctionId,
    pub name: String,
    pub signature: IrFunctionSignature,
    pub blocks: IndexMap<BlockId, IrBasicBlock>,
    pub entry_block: BlockId,
    pub exit_block: BlockId,
    pub is_leaf: bool
}

#[derive(Debug, Clone, Copy)]
pub struct IrFunctionAnchor {
    pub id: IrFunctionId,
    pub entry_block: BlockId,
    pub exit_block: BlockId
}

impl IrFunctionAnchor {
    pub fn new(id: IrFunctionId, entry: BlockId, exit: BlockId) -> Self {
        Self {
            id,
            entry_block: entry,
            exit_block: exit
        }
    }
}

pub struct IrFunctionContext {
	pub variable_id: usize,
	pub anchor: IrFunctionAnchor,
	loop_stack: Vec<IrLoopContext>,
	return_label: Option<BlockId>,
	var_map: HashMap<String, IrVariableId>
}

impl IrFunctionContext {
	pub fn new(anchor: IrFunctionAnchor) -> Self {
		Self {
			anchor,
			variable_id: 0,
			loop_stack: vec![],
			return_label: None,
			var_map: HashMap::new()
		}
	}

    pub fn set_return_label(&mut self, return_label: BlockId) {
        self.return_label = Some(return_label);
    }

    pub fn get_return_label(&self) -> Option<BlockId> {
        self.return_label
    }

	pub fn next_variable_id(&mut self) -> IrVariableId {
		let var_id = self.variable_id;
		self.variable_id += 1;
		IrVariableId(var_id)
	}

	pub fn enter_loop(&mut self, ctx: IrLoopContext) {
		self.loop_stack.push(ctx);
	}

	pub fn exit_loop(&mut self) -> Option<IrLoopContext> {
		self.loop_stack.pop()
	}

	pub fn map_var(&mut self, var_name: String) -> IrVariableId {
		let var_id = self.next_variable_id();
		self.var_map.insert(var_name, var_id);
		var_id
	}

	pub fn get_mapped_var_unchecked(&mut self, var_name: String) -> IrVariableId {
		*self.var_map.get(&var_name).expect("unchecked var map error")
	}
}