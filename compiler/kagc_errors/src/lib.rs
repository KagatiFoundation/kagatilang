// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

pub mod diagnostic;
pub mod code;
pub mod terminal;

/// Internal compiler errors
pub mod compiler_internals {
    pub enum InternalError {
        InvalidCompilerState
    }

    impl InternalError {
        pub fn dump(&self) {
            match self {
                InternalError::InvalidCompilerState => {
                    eprintln!("Invalid compiler state! Aborting...")
                }
            }
        }
    }
}