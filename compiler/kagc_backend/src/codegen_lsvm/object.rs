// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use crate::codegen_lsvm::opcode::LsvmConst;

pub struct ObjectHeader {
    pub ref_count: usize,
    pub ob_size: usize
}

pub struct StringObject<'a> {
    value: &'a str
}

pub struct RecordObject {

}

pub enum LsvmObjectKind<'a> {
    Const(LsvmConst),
    String(StringObject<'a>),
    Record(RecordObject)
}

pub struct LsvmObject<'a> {
    pub header: ObjectHeader,
    pub data: LsvmObjectKind<'a>
}