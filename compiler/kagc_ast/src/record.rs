use kagc_types::LitTypeVariant;

use crate::Expr;

#[derive(Debug, Clone)]
pub struct RecordField {
    pub typ: LitTypeVariant,
    pub name: String,
    pub default_value: Option<Expr>
}