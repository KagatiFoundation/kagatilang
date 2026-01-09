// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_types::TyKind;

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

impl From<TyKind<'_>> for IRType {
    fn from(value: TyKind) -> Self {
        match value {
            TyKind::I64 => IRType::I64,
            TyKind::Void => IRType::Void,
            TyKind::Str => IRType::RawStr,
            _ => unimplemented!("{value:#?}"),
        }
    }
}