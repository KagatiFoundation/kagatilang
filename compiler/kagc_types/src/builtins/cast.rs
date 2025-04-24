use super::builtin::{BuiltinType, TypeId};

pub trait CastTo {
    fn cast_to(&self, target: TypeId) -> Option<Box<dyn BuiltinType>>;
}