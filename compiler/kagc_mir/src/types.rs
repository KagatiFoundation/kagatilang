// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_types::LitTypeVariant;

pub type Label<'a> = &'a str;

pub type Identifier<'a> = &'a str;

#[derive(Debug, Default, Clone, Copy)]
pub enum IRType {
    I64,

    #[default]
    Void,
}

impl From<LitTypeVariant> for IRType {
    fn from(value: LitTypeVariant) -> Self {
        match value {
            LitTypeVariant::I64 => IRType::I64,
            LitTypeVariant::Void => IRType::Void,
            _ => todo!(),
        }
    }
}