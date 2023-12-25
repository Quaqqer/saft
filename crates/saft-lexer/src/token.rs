#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
    Unknown,
    Eof,

    Identifier(&'a str),
    Float(f64),
    Integer(i64),

    ColonEqual,
}
