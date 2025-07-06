pub mod type_checker;
pub mod errors;
pub mod typedefs;
pub mod resolver;
pub mod boxing;

mod analyzer;
pub use analyzer::*;