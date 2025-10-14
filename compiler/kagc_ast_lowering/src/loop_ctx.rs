use kagc_mir::block::BlockId;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq)]
pub struct LoopContext {
    pub head_block: BlockId,
    pub exit_block: BlockId
}