// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use indexmap::IndexMap;
use kagc_symbol::StorageClass;

use crate::block::IrBasicBlock;
use crate::block::BlockId;

use crate::types::IrType;
use crate::variable::IrVariableId;

#[derive(Default, Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub struct FunctionId(pub usize);

#[derive(Default, Debug, Clone, Copy)]
pub struct FunctionParam {
    pub id: IrVariableId,
    pub ty: IrType
}

#[derive(Default, Debug, Clone)]
pub struct FunctionSignature {
    pub params: Vec<FunctionParam>,
    pub return_type: IrType,
    pub class: StorageClass
}

#[derive(Default, Debug)]
pub struct IrFunction {
    pub id: FunctionId,
    pub name: String,
    pub signature: FunctionSignature,
    pub blocks: IndexMap<BlockId, IrBasicBlock>,
    pub entry_block: BlockId,
    pub exit_block: BlockId,
    pub is_leaf: bool
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionAnchor {
    pub id: FunctionId,
    pub entry_block: BlockId,
    pub exit_block: BlockId
}

impl FunctionAnchor {
    pub fn new(id: FunctionId, entry: BlockId, exit: BlockId) -> Self {
        Self {
            id,
            entry_block: entry,
            exit_block: exit
        }
    }
}