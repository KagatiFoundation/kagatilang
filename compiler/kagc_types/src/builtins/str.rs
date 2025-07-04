use super::{obj::{KagatiObj, TypeId}, cast::CastTo};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringType {
    __value: String,

    pub label: usize
}

impl KagatiObj for StringType {
    fn name(&self) -> String {
        "string".to_string()
    }
    
    fn size(&self) -> usize {
        self.__value.len()
    }

    fn type_id(&self) -> TypeId {
        TypeId::Bool
    }
}
    
impl CastTo for StringType {
    fn cast_to(&self, target: TypeId) -> Option<Box<dyn KagatiObj>> {
        match target {
            TypeId::Str => Some(Box::new(self.clone())),
            _ => None
        }
    }
}