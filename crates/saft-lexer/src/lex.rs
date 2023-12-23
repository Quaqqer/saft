use std::{ops::Range, str::CharIndices};

use saft_common::span::Spanned;

use crate::token::Token;

pub struct Lexer<'a> {
    src: &'a str,
    iter: CharIndices<'a>,

    start: usize,
    end: usize,
}

#[derive(Debug)]
pub enum Error {
    Eof,
    UnexpectedChar(char, Range<usize>),
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            iter: src.char_indices(),

            start: 0,
            end: 0,
        }
    }

    pub fn eat_str(&mut self, s: &'static str) -> bool {
        let mut chars_iter = self.iter.clone();
        let mut s_iter = s.chars();

        // Get next char from string
        while let Some(s_char) = s_iter.next() {
            if let Some((_, c_char)) = chars_iter.next()
                && c_char == s_char
            {
                // This is fine
            } else {
                return false;
            }
        }

        self.iter = chars_iter;
        self.end = self.iter.offset();
        true
    }

    pub fn eat_while<F>(&mut self, f: F) -> usize
    where
        F: Fn(char) -> bool,
    {
        while let Some((_, c)) = self.iter.clone().peekable().peek()
            && f(*c)
        {
            self.iter
                .next()
                .expect("This char should have been eaten already");
        }

        self.iter.offset()
    }

    pub fn get_token(&mut self) -> Result<Spanned<Token<'a>>, Error> {
        macro_rules! mktoken {
            ($lexer:expr, $tok:expr) => {
                Ok(Spanned::new($tok, $lexer.start..$lexer.end))
            };
        }

        use Token as T;

        self.iter
            .next()
            .ok_or(Error::Eof)
            .and_then(|(start, c)| match c {
                ':' if self.eat_str("=") => mktoken!(self, T::ColonEqual),

                c if c.is_ascii_alphabetic() || c == '_' => {
                    let end = self.eat_while(|c| c.is_alphanumeric() || c == '_');
                    let range = start..end;
                    Ok(Spanned::new(
                        Token::Identifier(&self.src[range.clone()]),
                        range,
                    ))
                }

                c if c.is_whitespace() => self.get_token(),

                _ => Err(Error::UnexpectedChar(c, start..start + 1)),
            })
    }
}
