use kagc_ir::ir_instr::{IRInstr, IR};
use kagc_target::reg::AllocedReg;

use crate::errors::CodeGenErr;

pub type CodeGenResult = Result<AllocedReg, CodeGenErr>;

pub type CGRes = Result<Vec<IR>, CodeGenErr>;

pub type CGExprEvalRes = Result<Vec<IRInstr>, CodeGenErr>;

/// Counter for generating unique temporary variable IDs.
pub type TempCounter = usize;

/// Tracks the current stack offset for local variables.
pub type StackOffset = usize;