#[derive(Debug, Clone)]
pub enum ErrCode {
    /// Missing semicolon
    ExpectedSemicolon     		= 1000,

    /// Invalid numeric type
    InvalidNumericLiteral     	= 1001,

	/// Invalid syntax
	InvalidSyntax				= 1002,

	/// Unterminated string
	UnterminatedString			= 1003,
	
	/// Unexpected end-of-file
	UnexpectedEOF				= 1004,

    /// Symbol not found
    UndefinedSymbol     		= 2000,

    /// Symbol already defined
    DuplicateSymbol     		= 2001,

    /// Non-callable type
    NotCallable     			= 3000,

    /// Argument length does not match
    ArgumentCountMismatch     	= 3001,

    /// Types are not compatible
    TypeMismatch     			= 3002,

    /// Assignment type mismatch
    AssignmentTypeMismatch     = 3003,

    /// Unknown field
	UnknownField     		   = 4000,

    /// Missing field
    MissingField     		   = 4001
}