/*
MIT License

Copyright (c) 2023 Kagati Foundation

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

pub struct RegAllocError;

/// Register size
pub type RegSize = usize;

/// Register index
pub type RegIdx = usize;

pub const INVALID_REG_IDX: usize = 0xFFFFFFFF;

/// Indicating no register was produced from an code generation operation.
pub const NO_REG: usize = 0xFFFFFFFF;

pub const EARLY_RETURN: usize = 0xEEEEEEEE;

/// Defines the allocation strategy for registers,  
/// determining whether spilling to memory is allowed.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum AllocStrategy {
    /// The register can be spilled to memory if needed.
    Spill,

    /// The register must not be spilled under any circumstances.
    NoSpill,
}

/// Represents the status of a register at a given moment.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub enum RegStatus {
    /// The register is currently allocated and in use.
    Alloced,

    /// The register is available for allocation.
    Free,

    /// The register has been spilled to memory.
    Spilled,

    /// The register is in an invalid state (e.g., uninitialized or corrupted).
    Invalid,
}

/// Stores information about an allocated register.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct AllocedReg {
    /// The size of the allocated register (e.g., 32-bit, 64-bit).
    pub size: RegSize,

    /// The index of the allocated register within the register set.
    pub idx: RegIdx,

    /// The current status of the allocated register.
    pub status: RegStatus,
}

impl AllocedReg {
    pub fn no_reg() -> Self {
        Self {
            size: 0xFFFFFFFF,
            idx: INVALID_REG_IDX,
            status: RegStatus::Invalid
        }
    }

    pub fn is_valid(&self) -> bool {
        self.idx != INVALID_REG_IDX
    }

    pub fn name(&self) -> String {
        match self.size {
            8 => format!("x{}", self.idx),
            _ => format!("w{}", self.idx),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub struct RegState {
    pub idx: usize,
    pub curr_alloced_size: usize,
    pub status: RegStatus
}

impl RegState {
    pub fn new(idx: usize, size: usize, status: RegStatus) -> Self {
        Self {
            idx,
            curr_alloced_size: size,
            status
        }
    }
}

pub type RegAllocResult = Result<AllocedReg, RegAllocError>;

pub trait RegManager2 {
    fn allocate_register(&mut self, alloc_size: usize) -> AllocedReg;

    fn allocate_register_with_idx(&mut self, alloc_size: usize, idx: RegIdx, strat: AllocStrategy) -> AllocedReg;
    
    fn allocate_param_register_with_idx(&mut self, alloc_size: usize, idx: RegIdx, strat: AllocStrategy) -> AllocedReg;

    fn allocate_param_register(&mut self, alloc_size: usize) -> AllocedReg;

    fn free_register(&mut self, reg: usize);

    fn spill_register(&mut self, alloc_size: usize, idx: Option<RegIdx>) -> AllocedReg;
    
    fn spill_param_register(&mut self, alloc_size: usize, idx: Option<RegIdx>) -> AllocedReg;
    
    fn restore_register(&mut self) -> usize;

    fn reset(&mut self);

    fn name(&self, idx: usize, alloc_size: usize) -> String;

    fn is_free(&self, idx: usize) -> bool;
}