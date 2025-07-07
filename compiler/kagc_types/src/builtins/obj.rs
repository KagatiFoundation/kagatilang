use crate::LitTypeVariant;

use super::cast::CastTo;

#[derive(Debug, Clone)]
pub enum ObjType {
    StringObj,
    Record
}

pub trait KagObj {
    fn size(&self) -> usize;

    fn to_string(&self) -> String;
}

#[derive(Debug, Clone)]
pub struct StringObj {
    pub __value: String,
    pub label: usize
}

impl StringObj {
    pub fn new(value: String, label: usize) -> Self {
        Self {
            __value: value,
            label
        }
    }
}

impl KagObj for StringObj {
    fn size(&self) -> usize {
        self.__value.len()
    }
    
    fn to_string(&self) -> String {
        self.__value.clone()
    }
}

#[derive(Debug, Clone)]
pub struct RecordField {
    pub name: String,
    pub typ: ObjType,
    pub rel_stack_off: usize
}

#[derive(Debug, Clone)]
pub struct RecordObj {
    pub name: String,
    pub size: usize,
    pub fields: Vec<RecordField>,
    pub __alignment: usize
}

impl KagObj for RecordObj {
    fn size(&self) -> usize {
        todo!()
    }

    fn to_string(&self) -> String {
        todo!()
    }
}

pub struct ArrayObj {
    __values: Vec<Box<dyn KagObj>>,
    size: usize
}

impl KagObj for ArrayObj {
    fn size(&self) -> usize {
        self.size
    }

    fn to_string(&self) -> String {
        format!("ArrayObj({})", self.size)
    }
}

pub trait KagatiObj: CastTo {
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
            LitTypeVariant::PoolStr => Self::Str,
            LitTypeVariant::RawStr => Self::Str
        }
    }
}