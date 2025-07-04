use super::{
    obj::{
        KagatiObj, 
        TypeId
    }, 
    cast::CastTo, 
    BoolType
};

pub trait IntegerType {
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Int32Type {
    pub(crate) __value :i32
}

impl KagatiObj for Int32Type {
    fn size(&self) -> usize {
        4
    }

    fn type_id(&self) -> TypeId {
        TypeId::Int32
    }
    
    fn name(&self) -> String {
        "integer".to_string()
    }
}

impl IntegerType for Int32Type {

}

impl CastTo for Int32Type {
    fn cast_to(&self, target: TypeId) -> Option<Box<dyn KagatiObj>> {
        match target {
            TypeId::Bool => {
                let bool_val = BoolType {
                    __value: self.__value != 0
                };
                Some(Box::new(bool_val))
            },
            TypeId::Int32 => Some(Box::new(*self)),
            _ => None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Int8Type {
    pub(crate) __value: u8
}

impl KagatiObj for Int8Type {
    fn size(&self) -> usize {
        1
    }

    fn type_id(&self) -> TypeId {
        TypeId::Int8
    }
    
    fn name(&self) -> String {
        "integer".to_string()
    }
}

impl IntegerType for Int8Type {

}

impl CastTo for Int8Type {
    fn cast_to(&self, target: TypeId) -> Option<Box<dyn KagatiObj>> {
        match target {
            TypeId::Int8 => Some(Box::new(*self)),
            TypeId::Bool => {
                let bool_val = BoolType {
                    __value: self.__value != 0
                };
                Some(Box::new(bool_val))
            },
            TypeId::Int32 => {
                let int_val = Int32Type {
                    __value: self.__value as i32
                };
                Some(Box::new(int_val))
            }
            _ => None
        }
    }
}