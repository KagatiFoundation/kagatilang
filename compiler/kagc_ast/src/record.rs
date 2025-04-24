use kagc_types::builtins::builtin::TypeId;

use crate::Expr;

#[derive(Debug, Clone)]
pub struct RecordField {
    pub typ: TypeId,
    pub name: String,
    pub default_value: Option<Expr>
}