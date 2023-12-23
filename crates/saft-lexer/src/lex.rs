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
pub enum Error<'a> {
    Eof,
    UnexpectedToken(&'a str, Range<usize>),
}

macro_rules! mktoken {
    ($lexer:expr, $tok:expr) => {
        Ok(Spanned::new($tok, $lexer.start..$lexer.end))
    };
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

    pub fn eat_while<F>(&mut self, f: &mut F) -> usize
    where
        F: FnMut(char) -> bool,
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
        use Token as T;

        self.iter
            .next()
            .ok_or(Error::Eof)
            .and_then(|(start, c)| match c {
                ':' if self.eat_str("=") => mktoken!(self, T::ColonEqual),

                c if c.is_ascii_alphabetic() || c == '_' => {
                    let end = self.eat_while(&mut |c| c.is_alphanumeric() || c == '_');
                    let range = start..end;
                    Ok(Spanned::new(
                        Token::Identifier(&self.src[range.clone()]),
                        range,
                    ))
                }

                c if c.is_numeric() => self.numeric(start),

                c if c.is_whitespace() => self.get_token(),

                _ => {
                    let end =
                        self.eat_while(&mut |c| !(c.is_whitespace() || ";(){}[]".contains(c)));
                    let range = start..end;
                    Err(Error::UnexpectedToken(&self.src[range.clone()], range))
                }
            })
    }

    fn numeric(&mut self, start: usize) -> Result<Spanned<Token<'a>>, Error> {
        let mut float = false;
        let end = self.eat_while(&mut |c| {
            if c.is_numeric() {
                true
            } else if c == '.' {
                if float {
                    false
                } else {
                    float = true;
                    true
                }
            } else {
                false
            }
        });

        let src = &self.src[start..end];

        if float {
            mktoken!(self, Token::Float(src.parse().unwrap()))
        } else {
            mktoken!(self, Token::Integer(src.parse().unwrap()))
        }
    }
}
