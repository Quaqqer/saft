use std::{borrow::Borrow, ops::Range};

#[derive(Clone, PartialEq, Debug)]
pub struct Span {
    pub r: Range<usize>,
}

impl Span {
    pub fn new(r: Range<usize>) -> Self {
        Self { r }
    }

    pub fn join(&self, other: impl Borrow<Span>) -> Span {
        Span::new(
            usize::min(self.r.start, other.borrow().r.start)
                ..usize::max(self.r.end, other.borrow().r.end),
        )
    }

    pub fn spanned<T>(&self, v: T) -> Spanned<T> {
        Spanned::new(v, self.clone())
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

    pub fn map<F, U>(&self, f: F) -> Spanned<U>
    where
        F: Fn(&T) -> U,
    {
        Spanned::new(f(&self.v), self.s.clone())
    }
}

pub fn spanned<T>(v: T, r: Range<usize>) -> Spanned<T> {
    Spanned::new(v, Span::new(r))
}
