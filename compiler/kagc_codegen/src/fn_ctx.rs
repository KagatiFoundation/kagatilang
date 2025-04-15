use kagc_ast::ASTOperation;
use kagc_ir::LabelId;
use kagc_symbol::INVALID_FUNC_ID;
use kagc_target::reg::RegIdx;

use crate::typedefs::*;

/// Holds function-specific context during AST to IR conversion.
#[derive(Debug)]
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

    pub prev_ast_kind: Option<ASTOperation>,

    pub parent_ast_kind: ASTOperation,

    pub next_label: LabelId,

    pub force_label_use: LabelId
}

impl Default for FnCtx {
    fn default() -> Self {
        Self { 
            stack_offset: Default::default(), 
            temp_counter: Default::default(), 
            reg_counter: Default::default(), 
            force_reg_use: Default::default(), 
            early_return: Default::default(), 
            prev_ast_kind: None,
            parent_ast_kind: ASTOperation::AST_NONE,
            next_label: 0,
            force_label_use: INVALID_FUNC_ID
        }
    }
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

    pub fn change_parent_ast_kind(&mut self, new_op: ASTOperation) {
        self.prev_ast_kind = Some(self.parent_ast_kind);
        self.parent_ast_kind = new_op;
    }

    pub fn reset_parent_ast_kind(&mut self) {
        if self.prev_ast_kind.is_some() {
            self.parent_ast_kind = self.prev_ast_kind.unwrap();
        }
    }

    pub fn get_next_label(&mut self) -> LabelId {
        let current: LabelId = self.next_label;
        self.next_label += 1;
        current
    }

    pub fn change_label_hint(&mut self, new_label: LabelId) {
        self.force_label_use = new_label;
    }

    pub fn reset_label_hint(&mut self) {
        self.force_label_use = INVALID_FUNC_ID;
    }
}