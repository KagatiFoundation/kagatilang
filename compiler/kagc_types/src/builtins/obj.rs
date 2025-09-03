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
}