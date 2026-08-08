// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use kagc_comp_unit::source_map::FileId;

#[derive(Debug, Clone)]
pub struct ParserOptions {
	pub(crate) file_id: FileId
}

impl ParserOptions {
	pub fn new(file_id: FileId) -> Self {
		Self {
			file_id
		}
	}
}