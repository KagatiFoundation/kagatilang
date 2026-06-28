// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#[derive(Default, Debug)]
pub(crate) struct CurrentFunctionState {
    pub is_leaf: bool,
    pub computed_stack_size: i64
}