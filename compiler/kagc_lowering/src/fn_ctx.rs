use std::collections::HashMap;

use kagc_ast::ASTOperation;
use kagc_mir::block::BlockId;
use kagc_mir::ir_operands::TempId;
use kagc_mir::value::IRValueId;
use kagc_mir::LabelId;
use kagc_symbol::function::INVALID_FUNC_ID;

use crate::loop_ctx::LoopContext;
use crate::typedefs::*;

/// Holds function-specific context during AST-to-IR conversion.
#[derive(Debug)]
pub struct FnCtx {
    /// Next available stack slot for local variables.
    pub stack_offset: StackOffset,

    /// Counter for generating fresh temporary variable IDs.
    pub temp_counter: TempCounter,

    /// Marks whether the next return is an early return.
    pub early_return: bool,

    /// Kind of the previous AST operation processed.
    pub prev_ast_kind: Option<ASTOperation>,

    /// Kind of the parent AST operation.
    pub parent_ast_kind: ASTOperation,

    /// Counter for generating unique labels.
    pub next_label: LabelId,

    /// Forces the use of a specific label ID.
    pub force_label_use: LabelId,

    /// Maps variable names to their stack offsets.
    pub var_offsets: HashMap<String, StackOffset>,

    value_id: usize,

    return_label: Option<BlockId>,

    loop_stack: Vec<LoopContext>
}

impl FnCtx {
    pub fn new(next_label: LabelId) -> Self {
        Self {
            stack_offset: Default::default(), 
            temp_counter: 0, 
            early_return: Default::default(), 
            prev_ast_kind: None,
            parent_ast_kind: ASTOperation::AST_FUNCTION,
            next_label,
            force_label_use: 0,
            var_offsets: HashMap::new(),
            value_id: 0,
            return_label: None,
            loop_stack: vec![]
        }
    }

    pub fn set_return_label(&mut self, return_label: BlockId) {
        self.return_label = Some(return_label);
    }

    pub fn get_return_label(&self) -> Option<BlockId> {
        self.return_label
    }

    pub fn enter_loop(&mut self, loop_ctx: LoopContext) {
        self.loop_stack.push(loop_ctx);
    }

    pub fn exit_loop(&mut self) -> Option<LoopContext> {
        self.loop_stack.pop()
    }

    pub fn current_loop(&self) -> Option<&LoopContext> {
        self.loop_stack.last()
    }

    pub fn current_loop_unchecked(&self) -> &LoopContext {
        self.loop_stack.last().unwrap()
    }

    pub fn next_value_id2(&mut self) -> IRValueId {
        let id = IRValueId(self.value_id);
        self.value_id += 1;
        id
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

    pub fn next_temp(&mut self) -> TempId {
        let nt = self.temp_counter;
        self.temp_counter += 1;
        nt
    }

    pub fn next_stack_off(&mut self) -> StackOffset {
        let so = self.stack_offset;
        self.stack_offset += 1;
        so
    }
}