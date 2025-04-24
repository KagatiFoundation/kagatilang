use crate::{builtins::builtin::TypeId, TypeSized};

pub type RecordFieldOffset = usize;

#[derive(Debug, Default)]
pub struct RecordType {
    pub name: String,
    pub size: usize,
    pub fields: Vec<RecordFieldType>,
    pub __alignment: usize
}

impl RecordType {
    pub fn new(name: String) -> Self {
        let size = Self::calc_size();

        Self {
            name,
            size,
            fields: vec![],
            __alignment: 0
        }
    }

    fn calc_size() -> usize {
        0
    }
}

impl TypeSized for RecordType {
    fn type_size(&self) -> usize {
        todo!()
    }
}

#[derive(Debug, Default)]
pub struct RecordFieldType {
    pub name: String,
    pub typ: TypeId,
    pub offset: RecordFieldOffset,
}