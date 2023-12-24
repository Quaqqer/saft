#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
    Unknown,

    Identifier(&'a str),
    Float(f64),
    Integer(i64),

    ColonEqual,
}
