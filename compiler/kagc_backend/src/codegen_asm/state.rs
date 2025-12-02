use kagc_mir::function::FunctionId;

#[derive(Default, Debug)]
pub struct CurrentFunctionState {
    pub id: FunctionId,
    pub is_leaf: bool,
    pub computed_stack_size: usize
}