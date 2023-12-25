#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Unknown,
    Eof,

    Identifier(String),
    Float(f64),
    Integer(i64),
    Nil,

    ColonEqual,
}
