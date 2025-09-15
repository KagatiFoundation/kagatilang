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

use crate::ir_instr::{
    IRFunc, 
    IRInstr, 
    IR
};

/// Byte offset of the `data` field in the `gc_object_t` struct.
///
/// On a 64-bit system, the `gc_object_t` layout is as follows:
/// ```text
/// ref_count      (8 bytes)  -> offset 0
/// size           (8 bytes)  -> offset 8
/// num_children   (8 bytes)  -> offset 16
/// children       (8 bytes)  -> offset 24
/// data           (8 bytes)  -> offset 32
/// ob_type        (8 bytes)  -> offset 40
/// ```
///
/// This constant represents the **raw byte offset** of the `data` pointer within
/// the struct. It can be used directly in assembly memory instructions.
pub const GCOBJECT_BUFFER_OFF: usize = 40; // bytes

/// Index of the `data` field in the `gc_object_t` struct.
///
/// On a 64-bit system, the `gc_object_t` layout is as follows:
/// ```text
/// ref_count      (8 bytes)  -> index 0
/// size           (8 bytes)  -> index 1
/// num_children   (8 bytes)  -> index 2
/// children       (8 bytes)  -> index 3
/// data           (8 bytes)  -> index 4
/// ob_type        (8 bytes)  -> index 5
/// ```
///
/// This constant represents the **field index** of the `data` pointer within
/// the struct. To compute the byte offset in assembly, multiply by the size
/// of each field (8 bytes on 64-bit systems):
/// `offset_in_bytes = GCOBJECT_BUFFER_IDX * 8`.
pub const GCOBJECT_BUFFER_IDX: usize = 5;

pub type HeapRange = (usize, usize);

pub struct HeapLivenessAnalyer;

impl HeapLivenessAnalyer {
    pub fn analyze(fn_ir: &IRFunc) -> Vec<usize> {
        for body_item in &fn_ir.body {
            match body_item {
                IR::Instr(instr) => {
                    match instr {
                        IRInstr::MemAlloc { .. } => {

                        },
                        _ => continue
                    }
                },
                _ => unreachable!()
            }
        }
        vec![]
    }
}