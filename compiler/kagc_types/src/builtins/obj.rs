#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum KObjType {
    KInt = 0,
    KStr = 1,
    KRec = 2
}

impl KObjType {
    pub fn value(&self) -> usize {
        match self {
            KObjType::KInt => 0,
            KObjType::KStr => 1,
            KObjType::KRec => 2,
        }
    }
}