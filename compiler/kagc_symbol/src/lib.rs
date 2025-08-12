// mod function;
mod sym;
mod symbol_table;
pub mod symbol_ctx;
pub mod function;
pub mod record;
pub mod registery;

pub use sym::*;
pub use symbol_table::*;

pub type SymbolPos = usize;