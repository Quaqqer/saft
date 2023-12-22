#[derive(Debug)]
pub enum Token<'a> {
    Identifier(&'a str),

    ColonEqual,
}
