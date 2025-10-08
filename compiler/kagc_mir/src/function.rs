// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use indexmap::IndexMap;

use crate::block::IRBasicBlock;
use crate::block::BlockId;

use crate::types::IRType;
use crate::value::IRValueId;

#[derive(Default, Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub struct FunctionId(pub usize);

#[derive(Default, Debug, Clone)]
pub struct FunctionParam {
    pub id: IRValueId,
    pub ty: IRType,
}

#[derive(Default, Debug, Clone)]
pub struct FunctionSignature {
    pub params: Vec<FunctionParam>,
    pub return_type: IRType,
}


#[derive(Default, Debug, Clone, Copy)]
pub struct FunctionFrame {
    pub size: usize
}

#[derive(Default, Debug)]
pub struct IRFunction {
    pub id: FunctionId,
    pub name: String,
    pub signature: FunctionSignature,
    pub blocks: IndexMap<BlockId, IRBasicBlock>,
    pub entry_block: BlockId,
    pub frame_info: FunctionFrame
}