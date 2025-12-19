#[cfg(test)]
mod ast_lowering_tests {
    #[test]
    fn test_basic_lowering() {
        let nodes = kagc_parser::prelude::parse_expression("12 + 12");
        assert!(nodes.is_ok());
        let nodes = nodes.ok().unwrap();
    }
}