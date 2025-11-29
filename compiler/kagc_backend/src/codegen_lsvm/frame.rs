// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::collections::HashMap;

use kagc_lir::vreg::VReg;
use kagc_utils::bug;

/// Represents a single slot in a virtual machine’s call frame.
#[derive(Debug, Clone, Copy, Default)]
pub struct FrameSlot(pub usize);

/// Maintains the mapping between virtual registers (`VReg`) and
/// their assigned virtual frame slots within a single function.
///
/// This structure is used by the bytecode generator or VM backend
/// to translate *virtual registers* (used in your LIR) into concrete
/// *frame slot indices* that the virtual machine understands.
#[derive(Debug, Clone, Default)]
pub struct FrameMapping {
    /// Mapping from virtual registers to their corresponding
    /// virtual machine frame slots.
    vreg_to_slot: HashMap<VReg, FrameSlot>,
    next_slot: usize
}

impl FrameMapping {
    /// Create a virtual slot in Frame mapping for a virtual register.
    pub fn create(&mut self, vreg: VReg) -> FrameSlot {
        let ns = self.next_slot;
        self.vreg_to_slot.insert(vreg, FrameSlot(ns));
        self.next_slot += 1;
        FrameSlot(ns)
    }

    pub fn get_unchecked(&self, vreg: &VReg) -> FrameSlot {
        *self.vreg_to_slot.get(vreg).unwrap_or_else(|| bug!("VReg to frame slot mapping not found"))
    }
}