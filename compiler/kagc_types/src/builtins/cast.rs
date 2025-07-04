use super::obj::{KagatiObj, TypeId};

pub trait CastTo {
    fn cast_to(&self, target: TypeId) -> Option<Box<dyn KagatiObj>>;
}