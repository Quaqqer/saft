#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Unknown,
    Eof,

    Identifier(String),
    Float(f64),
    Integer(i64),
    Nil,

    ColonEqual,
    Operator(String),
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
            Token::Operator(op) => format!("'{}'", op),
        }
    }
}
