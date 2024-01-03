use saft_common::span::Span;
use std::str::CharIndices;

#[derive(Clone, Debug)]
pub struct Cursor<'a> {
    start: usize,
    src: &'a str,
    iter: CharIndices<'a>,
}

impl<'a> Cursor<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            start: 0,
            src,
            iter: src.char_indices(),
        }
    }
    pub fn peek(&self) -> Option<char> {
        self.iter.clone().peekable().peek().map(|(_, c)| c).cloned()
    }

    pub fn peek_n(&self, n: usize) -> Option<char> {
        let mut iter = self.iter.clone();

        for _ in 0..n - 1 {
            iter.next()?;
        }
        iter.next().map(|(_, c)| c)
    }

    pub fn eat_char(&mut self, c: char) -> bool {
        self.eat(|cc| cc == c)
    }

    pub fn eat<F>(&mut self, f: F) -> bool
    where
        F: Fn(char) -> bool,
    {
        let eat = self.peek().map_or(false, f);
        if eat {
            // Eat the char
            self.advance();
        }
        eat
    }

    pub fn eat_while<F>(&mut self, f: F)
    where
        F: Fn(char) -> bool,
    {
        while self.eat(&f) {}
    }

    pub fn eat_until<F>(&mut self, f: F)
    where
        F: Fn(char) -> bool,
    {
        loop {
            let Some(c) = self.peek() else { break };
            self.advance();
            if f(c) {
                break;
            };
        }
    }

    pub fn advance(&mut self) {
        self.iter.next().expect("Not allowed to advance beyond end");
    }

    pub fn span(&self) -> Span {
        Span::new(self.start..self.iter.offset())
    }

    pub fn offset(&self) -> usize {
        self.iter.offset()
    }

    pub fn str(&self) -> &'a str {
        &self.src[self.span().r]
    }

    pub fn restart(&mut self) {
        self.start = self.iter.offset();
    }
}
