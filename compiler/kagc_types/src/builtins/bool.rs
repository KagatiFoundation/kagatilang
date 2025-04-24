use super::{
    builtin::{
        BuiltinType, 
        TypeId
    }, cast::CastTo, 
    Int32Type, 
    Int8Type
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BoolType {
    pub(crate) __value: bool
}

impl BuiltinType for BoolType {
    fn name(&self) -> String {
        "boolean".to_string()
    }

    fn size(&self) -> usize {
        1
    }

    fn type_id(&self) -> TypeId {
        TypeId::Bool
    }
}

impl CastTo for BoolType {
    fn cast_to(&self, target: TypeId) -> Option<Box<dyn BuiltinType>> {
        match target {
            TypeId::Bool => Some(Box::new(*self)),
            TypeId::Int32 => {
                let int_value = Int32Type {
                    __value: if self.__value { 1 } else { 0 }
                };
                Some(Box::new(int_value))
            },
            TypeId::Int8 => {
                let int_value = Int8Type {
                    __value: if self.__value { 1 } else { 0 }
                };
                Some(Box::new(int_value))
            },
            _ => None
        }
    }
}