#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Unknown,
    Eof,

    Fn,

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
}

impl Token {
    pub fn describe(&self) -> String {
        match self {
            Token::Unknown => "unknown token".into(),
            Token::Eof => "end of file".into(),
            Token::Identifier(..) => "identifier".into(),
            Token::Float(..) => "float".into(),
            Token::Integer(..) => "integer".into(),
            Token::ColonEqual => "':='".into(),
            Token::Nil => "'nil'".into(),
            Token::Fn => "'fn'".into(),
            Token::LParen => "'('".into(),
            Token::RParen => "')'".into(),
            Token::LBrace => "'{'".into(),
            Token::RBrace => "'}'".into(),
            Token::Comma => "','".into(),
            Token::Equal => "'='".into(),
            Token::Plus => "'+'".into(),
            Token::Minus => "'-'".into(),
            Token::Star => "'*'".into(),
            Token::Slash => "'/'".into(),
        }
    }
}
