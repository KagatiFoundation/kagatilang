#[derive(Debug)]
pub enum ErrCode {
    /// Missing semicolon
    SYN1001     = 1001,

    /// Symbol not found
    SEM2001     = 2001,

    /// Non-callable type
    TYP2101     = 2101,

    /// Argument length does not match
    TYP2102     = 2102,

    /// Types are not compatible
    TYP2103     = 2103,

    /// Assignment type mismatch
    TYP2104     = 2104
}