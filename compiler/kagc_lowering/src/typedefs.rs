use kagc_errors::diagnostic::Diagnostic;
use kagc_ir::ir_instr::{IRInstr, IR};
use kagc_target::reg::AllocedReg;

pub type CodeGenResult = Result<AllocedReg, Diagnostic>;

pub type CGRes = Result<Vec<IR>, Diagnostic>;

pub type CGExprEvalRes = Result<Vec<IRInstr>, Diagnostic>;

/// Counter for generating unique temporary variable IDs.
pub type TempCounter = usize;

/// Tracks the current stack offset for local variables.
pub type StackOffset = usize;