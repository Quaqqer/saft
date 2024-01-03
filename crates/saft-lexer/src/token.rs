#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Unknown,
    Eof,
    Error(String),

    Fn,
    Return,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Semicolon,

    Identifier(String),
    Float(f64),
    Integer(i64),
    String(String),
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
    pub fn describe(&self) -> String {
        use Token::*;

        match self {
            Unknown => "unknown token".into(),
            Eof => "end of file".into(),
            Identifier(i) => format!("identifier '{}'", i),
            Float(f) => format!("float '{}'", f),
            Integer(i) => format!("integer '{}'", i),
            ColonEqual => "':='".into(),
            Nil => "'nil'".into(),
            Fn => "'fn'".into(),
            LParen => "'('".into(),
            RParen => "')'".into(),
            LBrace => "'{'".into(),
            RBrace => "'}'".into(),
            Comma => "','".into(),
            Equal => "'='".into(),
            Plus => "'+'".into(),
            Minus => "'-'".into(),
            Star => "'*'".into(),
            Slash => "'/'".into(),
            Caret => "'^'".into(),
            Return => "'return'".into(),
            Semicolon => "';'".into(),
            String(s) => format!("string '{}'", s),
            Error(e) => format!("error '{}'", e),
            LBracket => "'['".into(),
            RBracket => "']'".into(),
        }
    }
}
