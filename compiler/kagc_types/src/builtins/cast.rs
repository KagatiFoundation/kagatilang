use crate::LitTypeVariant;

use super::obj::KagatiObj;

pub trait CastTo {
    fn cast_to(&self, target: LitTypeVariant) -> Option<Box<dyn KagatiObj>>;
}