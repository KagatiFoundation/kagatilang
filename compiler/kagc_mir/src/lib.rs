// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

pub mod ir_operands;
pub mod ir_instr;
pub mod ir_liveness;
pub mod builtin;

/// Label ID
pub type LabelId = usize;

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
#[deprecated]
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
#[deprecated]
pub const GCOBJECT_BUFFER_IDX: usize = 5;

pub const REG_SIZE_8: usize = 8;
pub const REG_SIZE_4: usize = 4;

pub mod value;
pub mod instruction;
pub mod block;
pub mod function;
pub mod mir_builder;
pub mod types;
pub mod module;
pub mod analyzer;