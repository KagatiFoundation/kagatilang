pub enum IrValue {
    Register(usize),
    Constant(i32),
}

pub enum IrOp {
    Add(IrValue, IrValue, usize), 
    Assign(String, IrValue),
}