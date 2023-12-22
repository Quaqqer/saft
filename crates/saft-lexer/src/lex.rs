use std::{
    ops::Range,
    str::{CharIndices, Chars},
};

use saft_common::span::Spanned;

use crate::token::Token;

pub struct Lexer<'a> {
    src: &'a str,
    iter: CharIndices<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            iter: src.char_indices(),
        }
    }

    pub fn eat_str(&mut self, s: &'static str) -> Option<Range<usize>> {
        let mut chars_iter = self.iter.clone();
        let mut s_iter = s.chars();

        let start = self.iter.offset();

        // Get next char from string
        while let Some(s_char) = s_iter.next() {
            if let Some((_, c_char)) = chars_iter.next()
                && c_char == s_char
            {
                // This is fine
            } else {
                return None;
            }
        }

        let end = chars_iter.offset();
        self.iter = chars_iter;

        Some(start..end)
    }

    pub fn eat_while<F>(&mut self, f: F) -> Range<usize>
    where
        F: Fn(char) -> bool,
    {
        let start = self.iter.offset();

        while let Some((_, c)) = self.iter.clone().peekable().peek()
            && f(*c)
        {
            self.iter
                .next()
                .expect("This char should have been eaten already");
        }

        let end = self.iter.offset();

        start..end
    }

    pub fn get_token(&mut self) -> Option<Spanned<Token<'a>>> {
        self.iter.next().and_then(|(c_offset, c)| match c {
            'a' if let Some(span) = self.eat_str("asd") => {
                todo!()
            }
            c if c.is_ascii_alphabetic() || c == '_' => {
                let rest = self.eat_while(|c| c.is_alphanumeric() || c == '_');
                let range = c_offset..rest.end;
                Some(Spanned::new(
                    Token::Identifier(&self.src[range.clone()]),
                    range,
                ))
            }
            _ => todo!(),
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Spanned<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.get_token()
    }
}
