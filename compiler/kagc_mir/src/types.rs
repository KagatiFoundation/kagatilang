// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

pub type Label<'a> = &'a str;

pub type Identifier<'a> = &'a str;

#[derive(Debug, Default, Clone, Copy)]
pub enum IRType {
    I64,

    #[default]
    Void,
}