use crate::LitTypeVariant;

use super::cast::CastTo;

pub trait BuiltinType: CastTo {
    fn name(&self) -> String;

    fn size(&self) -> usize;

    fn type_id(&self) -> TypeId;

    fn can_cast_to(&self, target: TypeId) -> bool {
        self.type_id().can_cast_to(target)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum TypeId {
    #[default]
    Int32,
    Int8,
    Str,
    Bool,
    Record,
    Null,
    Void,
    None
}

impl TypeId {
    pub fn can_cast_to(&self, target: TypeId) -> bool {
        match (self, target) {
            (TypeId::Bool, TypeId::Bool) => true,
            (TypeId::Bool, TypeId::Int32) => true,
            _ => todo!()
        }
    }
}

impl From<LitTypeVariant> for TypeId {
    fn from(value: LitTypeVariant) -> Self {
        match value {
            LitTypeVariant::I32 => Self::Int32,
            LitTypeVariant::I64 => todo!(),
            LitTypeVariant::I16 => todo!(),
            LitTypeVariant::U8 => todo!(),
            LitTypeVariant::F64 => todo!(),
            LitTypeVariant::F32 => todo!(),
            LitTypeVariant::Void => Self::Void,
            LitTypeVariant::Str => Self::Str,
            LitTypeVariant::Array => todo!(),
            LitTypeVariant::Null => Self::Null,
            LitTypeVariant::None => Self::None,
            LitTypeVariant::Record => Self::Record,
        }
    }
}