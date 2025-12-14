#[derive(Debug)]
pub enum ErrCode {
    /// Missing semicolon
    SYN1000     = 1000,

    /// Symbol not found
    SEM2000     = 2000,

    /// Symbol already defined
    SEM2001     = 2001,

    /// Non-callable type
    TYP3000     = 3000,

    /// Argument length does not match
    TYP3001     = 3001,

    /// Types are not compatible
    TYP3002     = 3002,

    /// Assignment type mismatch
    TYP3003     = 3003,

    /// Unknown field
    REC4000     = 4000,

    /// Missing field
    REC4001     = 4001
}