#[derive(Debug)]
pub enum CodeGenErr {
    NoContext,
    UndefinedSymbol
}

impl CodeGenErr {
    pub fn dump(&self) {
        match self {
            Self::NoContext => {
                panic!("internal error: no context was provided for code generator")
            },
            Self::UndefinedSymbol => {
                panic!("internal error: code generator tried to search for a non-existing symbol")
            }
        }
    }
}