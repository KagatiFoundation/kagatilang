use kagc_ir::ir_instr::{IRInstr, IR};
use kagc_target::reg::{AllocedReg, RegIdx};

use crate::errors::CodeGenErr;

pub type CodeGenResult = Result<AllocedReg, CodeGenErr>;

pub type CGRes = Result<Vec<IR>, CodeGenErr>;

pub type CGExprEvalRes = Result<Vec<IRInstr>, CodeGenErr>;

/// Counter for generating unique temporary variable IDs.
pub type TempCounter = usize;

/// Tracks the current stack offset for local variables.
pub type StackOffset = usize;

/// Holds function-specific context during AST to IR conversion.
#[derive(Default, Debug)]
pub struct FnCtx {
    /// Tracks the next available stack slot.
    pub stack_offset: StackOffset,

    /// Tracks the next available temporary variable ID.
    pub temp_counter: TempCounter,

    /// Preferred register for allocation, if available.
    pub reg_counter: Option<RegIdx>,

    /// Indicates that the next value should use `reg_counter` instead of a temporary.
    pub force_reg_use: bool,

    /// Indicates whether the next return statement is an early return or not.
    pub early_return: bool,
}

impl FnCtx {
    pub fn force_reg_use(&mut self, reg: RegIdx) {
        self.force_reg_use = true;
        self.reg_counter = Some(reg);
    }

    pub fn clear_reg_hint(&mut self) {
        self.force_reg_use = false;
        self.reg_counter = None;
    }
}