use std::ops::Range;

#[derive(Clone, PartialEq, Debug)]
pub struct Span {
    pub r: Range<usize>,
}

impl Span {
    pub fn new(r: Range<usize>) -> Self {
        Self { r }
    }

    pub fn join(&self, other: &Span) -> Span {
        Span::new(usize::min(self.r.start, other.r.start)..usize::max(self.r.end, other.r.end))
    }
}

pub fn span(r: Range<usize>) -> Span {
    Span::new(r)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Spanned<T> {
    pub s: Span,
    pub v: T,
}

impl<T> Spanned<T> {
    pub fn new(v: T, s: Span) -> Self {
        Self { v, s }
    }
}
