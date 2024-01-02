#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Unknown,
    Eof,

    Fn,
    Return,

    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,

    Identifier(String),
    Float(f64),
    Integer(i64),
    Nil,

    ColonEqual,
    Equal,
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
}

impl Token {
    pub fn describe(&self) -> &'static str {
        use Token::*;

        match self {
            Unknown => "unknown token",
            Eof => "end of file",
            Identifier(..) => "identifier",
            Float(..) => "float",
            Integer(..) => "integer",
            ColonEqual => "':='",
            Nil => "'nil'",
            Fn => "'fn'",
            LParen => "'('",
            RParen => "')'",
            LBrace => "'{'",
            RBrace => "'}'",
            Comma => "','",
            Equal => "'='",
            Plus => "'+'",
            Minus => "'-'",
            Star => "'*'",
            Slash => "'/'",
            Caret => "'^'",
            Return => "'return'",
        }
    }
}
