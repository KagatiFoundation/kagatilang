use crate::block::BlockId;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq)]
pub struct IrLoopContext {
    pub head_block: BlockId,
    pub exit_block: BlockId
}