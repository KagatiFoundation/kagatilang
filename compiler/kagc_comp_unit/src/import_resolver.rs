// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::env;
use std::path::{Path, PathBuf};

use crate::source::loader::SourceFileLoader;
use crate::source::SourceFile;

pub struct ImportResolver;

impl ImportResolver {
    pub fn resolve<'tcx>(
        path: &str,
        arena: &'tcx typed_arena::Arena<String>
    ) -> std::io::Result<SourceFile<'tcx>> {
        let import_path = Path::new(path).with_extension("kag");

        if import_path.exists() {
            return SourceFile::from_file(PathBuf::from(path), arena);
        }

        if let Ok(kagc_path) = env::var("KAGC_PATH") {
            let alt_path = PathBuf::from(kagc_path).join(import_path);
            if alt_path.exists() {
                return SourceFileLoader::load(&alt_path, arena);
            }
        }
        else {
            eprintln!("KAGC_PATH is not set.");
        }

        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("No module named '{}' found", path),
        ))
    }
}