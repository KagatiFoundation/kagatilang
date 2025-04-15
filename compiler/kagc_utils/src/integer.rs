use std::fmt::LowerHex;
use itertools::Itertools;

pub fn to_hex<T: LowerHex>(num: T) -> String {
    format!("0x{:x}", num)
}

/// Split the given 64 bit hex string  into 4 parts.
/// Example: 0xf98dcba4b4bacd23 into vec!["f98d", "cba4", "b4ba", "cd23"]
pub fn i64_hex_split_quarter(x: String) -> Vec<String> {
    if !x.starts_with("0x") {
        return vec![];
    }
    let only_hex: &str = x.trim_start_matches("0x");
    if only_hex.len() > 16 || only_hex.is_empty() {
        return vec![];
    }
    let mut result: Vec<String> = vec![];
    let padded_hex: String = format!("{:0>16}", only_hex);
    for chunk in &padded_hex.chars().chunks(4) {
        result.push(String::from_iter(chunk));
    }
    result
}

/// Split the given 32 bit hex string into 2 parts.
/// Example: 0xf98dcba4 into vec!["f98d", "cba4"]
pub fn i32_hex_split_half(x: String) -> Vec<String> {
    if !x.starts_with("0x") {
        return vec![];
    }
    let only_hex: &str = x.trim_start_matches("0x");
    if only_hex.len() > 8 || only_hex.is_empty() {
        return vec![];
    }
    let padded_hex: String = format!("{:0>8}", only_hex);
    let mut result: Vec<String> = vec![];
    for chunk in &padded_hex.chars().chunks(4) {
        result.push(String::from_iter(chunk));
    }
    result
}

pub fn split_i64_hex_half_each(x: String) -> Vec<String> {
    if !x.starts_with("0x") {
        panic!("{} is not a hex string", x);
    }
    let only_hex: &str = x.trim_start_matches("0x");
    if only_hex.len() > 16 {
        panic!("{} exceeds the length of a 64-bit hex string", x);
    }
    let padded_hex: String = format!("{:0>16}", only_hex);
    let (first_half, second_half) = padded_hex.split_at(4);
    vec![String::from(first_half), String::from(second_half)]
}

#[cfg(test)]
mod tests {
    use crate::integer::{i32_hex_split_half, i64_hex_split_quarter};

    #[test]
    fn test_i32_hex_split_half1() {
        let input: String = "0xbcab".to_string();
        assert_eq!(i32_hex_split_half(input), ["0000", "bcab"]);
    }

    #[test]
    fn test_i32_hex_split_half2() {
        let input: String = "0xbc".to_string();
        assert_eq!(i32_hex_split_half(input), ["0000", "00bc"]);
    }

    #[test]
    fn test_i32_hex_split_half3() {
        let input: String = "0x".to_string();
        assert_eq!(i32_hex_split_half(input), Vec::<String>::new());
    }

    #[test]
    fn test_i64_hex_split_quarter1() {
        let input: String = "0xbcab".to_string();
        assert_eq!(i64_hex_split_quarter(input), ["0000", "0000", "0000", "bcab"]);
    }

    #[test]
    fn test_i64_hex_split_quarter2() {
        let input: String = "0xbc".to_string();
        assert_eq!(i64_hex_split_quarter(input), ["0000", "0000", "0000", "00bc"]);
    }

    #[test]
    fn test_i64_hex_split_quarter3() {
        let input: String = "0x".to_string();
        assert_eq!(i64_hex_split_quarter(input), Vec::<String>::new());
    }
}