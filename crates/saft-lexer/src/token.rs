#[derive(Debug)]
pub enum Token<'a> {
    Identifier(&'a str),
    Float(f64),
    Integer(i64),

    ColonEqual,
}
