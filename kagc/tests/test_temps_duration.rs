#[cfg(test)]
mod tests_temp_duration {
    use kagc::compiler::Compiler;
    use kagc_codegen::aarch64::Aarch64Codegen;
    use kagc_target::asm::aarch64::Aarch64RegMgr;

    #[test]
    pub fn test_temps_liveness_after_certain_instructions() {
        let mut c = Compiler::new();
        let irs = c.compile_into_ir("/Users/rigelstar/Desktop/KagatiFoundation/kagatilang/examples/ex1.kag");
        assert!(irs.is_ok());

        let irs = irs.unwrap();
        let cg = Aarch64Codegen::new(c.ctx.clone(), Aarch64RegMgr::new());

        // cg.is_temp_alive_after(temp, n_instrs)
    }
}