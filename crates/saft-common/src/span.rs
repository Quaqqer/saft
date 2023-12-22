use std::ops::Range;

#[derive(Debug)]
pub struct Spanned<T> {
    pub s: Range<usize>,
    pub v: T,
}

impl<T> Spanned<T> {
    pub fn new(v: T, s: Range<usize>) -> Self {
        Self { v, s }
    }
}
