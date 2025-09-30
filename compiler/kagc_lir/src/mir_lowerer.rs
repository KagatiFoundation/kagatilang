#[cfg(test)]
mod tests {
    // use crate::regalloc::*;

    // fn mock_aarch64_reg_file() -> RegisterFile {
    //     let mut id = 0;

    //     let gprs = vec![
    //         Register { id: { id += 1; id - 1 }, name: "x9".to_string(),  class: RegClass::GPR },
    //         Register { id: { id += 1; id - 1 }, name: "x10".to_string(), class: RegClass::GPR },
    //         Register { id: { id += 1; id - 1 }, name: "x11".to_string(), class: RegClass::GPR },
    //         Register { id: { id += 1; id - 1 }, name: "x12".to_string(), class: RegClass::GPR },
    //         Register { id: { id += 1; id - 1 }, name: "x13".to_string(), class: RegClass::GPR },
    //     ];

    //     RegisterFile {
    //         registers: gprs.clone(),
    //         reserved: vec![
    //             Register { id: 200, name: "sp".to_string(), class: RegClass::GPR },
    //             Register { id: 201, name: "fp".to_string(), class: RegClass::GPR },
    //         ],
    //         caller_saved: gprs,
    //         callee_saved: vec![],
    //     }
    // }
}