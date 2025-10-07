// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_mir::value::IRValueId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VReg(pub usize);

#[derive(Debug, Default)]
pub struct VRegMapper {
    pub(crate) vreg_id: usize,
    pub(crate) mapping: HashMap<IRValueId, VReg>
}

impl VRegMapper {
    pub fn get_or_create(&mut self, value: IRValueId) -> VReg {
        if let Some(&vreg) = self.mapping.get(&value) {
            vreg
        } 
        else {
            let vreg = VReg(self.vreg_id);
            self.mapping.insert(value, vreg);
            self.vreg_id += 1;
            vreg
        }
    }
}

#[derive(Debug, Clone)]
pub struct VRegLiveRange {
    pub vreg: VReg,
    pub start: usize,  // instruction index in function
    pub end: usize,    // instruction index in function
}