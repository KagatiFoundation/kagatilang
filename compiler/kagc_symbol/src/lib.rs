// mod function;
mod sym;
mod symbol_table;
pub mod function;
pub mod record;

pub use sym::*;
pub use symbol_table::*;

pub type SymbolPos = usize;