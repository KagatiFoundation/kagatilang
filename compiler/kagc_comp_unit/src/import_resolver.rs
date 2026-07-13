// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use crate::FileMeta;
use crate::SourceFile;

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
        		let content = fs::read_to_string(path)?;
        		let alloced_str = arena.alloc(content);
        		return Ok(SourceFile {
            		content: alloced_str,
            		meta: FileMeta {
                		name: alt_path.file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_default(),
                		abs_path: PathBuf::from(path)
            		}
        		});
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