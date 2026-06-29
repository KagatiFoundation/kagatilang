use std::collections::HashMap;

use kagc_ast::AstOp;
use kagc_mir::block::BlockId;
use kagc_mir::value::IrValueId;
use kagc_mir::LabelId;
use kagc_mir::variable::IrVariableId;
use kagc_symbol::function::INVALID_FUNC_ID;

use crate::loop_ctx::LoopContext;

/// Holds function-specific context during AST-to-IR conversion.
#[derive(Debug)]
pub struct FunctionContext {
    /// Marks whether the next return is an early return.
    pub early_return: bool,

    /// Kind of the previous AST operation processed.
    pub prev_ast_kind: Option<AstOp>,

    /// Kind of the parent AST operation.
    pub parent_ast_kind: AstOp,

    /// Counter for generating unique labels.
    pub next_label: LabelId,

    /// Forces the use of a specific label ID.
    pub force_label_use: LabelId,

    value_id: usize,

    return_label: Option<BlockId>,

    loop_stack: Vec<LoopContext>,

	variable_id: usize,

	var_map: HashMap<String, IrVariableId>
}

impl FunctionContext {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            early_return: Default::default(), 
            prev_ast_kind: None,
            parent_ast_kind: AstOp::Func,
            next_label: 0,
            force_label_use: 0,
            value_id: 0,
            return_label: None,
            loop_stack: vec![],
			variable_id: 0,
			var_map: HashMap::new()
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

    pub fn next_value_id2(&mut self) -> IrValueId {
        let id = IrValueId(self.value_id);
        self.value_id += 1;
        id
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

	pub fn map_var(&mut self, var_name: String) -> IrVariableId {
		let var_id = self.next_variable_id();
		self.var_map.insert(var_name, var_id);
		var_id
	}

	pub fn get_mapped_var_unchecked(&mut self, var_name: String) -> IrVariableId {
		*self.var_map.get(&var_name).expect("unchecked var map error")
	}

	pub fn next_variable_id(&mut self) -> IrVariableId {
		let id = self.variable_id;
		self.variable_id += 1;
		IrVariableId(id)
	}
}