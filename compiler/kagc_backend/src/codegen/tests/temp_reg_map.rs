// SPDX-License-Identifier: MIT
// Copyright (c) 2023 Kagati Foundation

#[cfg(test)]
mod tests {
    use kagc_codegen::{CustomMap, TRMap};
    use kagc_target::reg::*;
    use std::collections::HashMap;

    fn sample_alloced_reg(idx: u8) -> AllocedReg {
        AllocedReg {
            size: REG_SIZE_8,
            idx: idx as usize,
            status: RegStatus::Alloced,
        }
    }

    #[test]
    fn test_insert_and_find_temp_by_reg() {
        let mut map = TRMap { reg_map: HashMap::new() };
        let temp = 1;
        let reg = sample_alloced_reg(3);

        map.reg_map.insert(temp, reg.clone());

        assert_eq!(map.reverse_get(reg.clone()), Some(temp));
        assert_eq!(map.reverse_get(reg.clone()), None);
    }

    #[test]
    fn test_drop_removes_entry() {
        let mut map = TRMap { reg_map: HashMap::new() };
        let temp = 2;
        let reg = sample_alloced_reg(7);

        map.reg_map.insert(temp, reg.clone());

        let removed = map.drop(&temp);
        assert_eq!(removed, Some(reg.clone()));

        assert_eq!(map.reverse_get(reg.clone()), None);
    }

    #[test]
    fn test_drop_nonexistent_key() {
        let mut map = TRMap { reg_map: HashMap::new() };

        // Dropping a non-existent key returns None
        assert_eq!(map.drop(&99), None);
    }
}