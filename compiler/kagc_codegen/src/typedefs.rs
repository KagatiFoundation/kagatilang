use kagc_ast::ASTOperation;
use kagc_ir::{ir_instr::{IRInstr, IR}, LabelId};
use kagc_symbol::INVALID_FUNC_ID;
use kagc_target::reg::{AllocedReg, RegIdx};

use crate::errors::CodeGenErr;

pub type CodeGenResult = Result<AllocedReg, CodeGenErr>;

pub type CGRes = Result<Vec<IR>, CodeGenErr>;

pub type CGExprEvalRes = Result<Vec<IRInstr>, CodeGenErr>;

/// Counter for generating unique temporary variable IDs.
pub type TempCounter = usize;

/// Tracks the current stack offset for local variables.
pub type StackOffset = usize;
