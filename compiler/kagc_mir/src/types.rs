// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_types::{LitTypeVariant, TyKind};

pub type Label<'a> = &'a str;

pub type Identifier<'a> = &'a str;

#[derive(Debug, Default, Clone, Copy)]
pub enum IRType {
    I64,

    I32,

    #[default]
    Void,

    RawStr
}

impl From<LitTypeVariant> for IRType {
    fn from(value: LitTypeVariant) -> Self {
        match value {
            LitTypeVariant::I64 => IRType::I64,
            LitTypeVariant::I32 => IRType::I32,
            LitTypeVariant::Void => IRType::Void,
            LitTypeVariant::RawStr => IRType::RawStr,
            _ => unimplemented!("{value:#?}"),
        }
    }
}

impl From<TyKind<'_>> for IRType {
    fn from(value: TyKind<'_>) -> Self {
        match value {
            TyKind::I64 => IRType::I64,
            TyKind::Void => IRType::Void,
            TyKind::Str => IRType::RawStr,
            _ => unimplemented!("{value:#?}")
        }
    }
}