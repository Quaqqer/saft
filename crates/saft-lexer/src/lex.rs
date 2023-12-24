use std::{ops::Range, str::CharIndices};

use saft_common::span::Spanned;

use crate::token::Token;

pub struct Lexer<'a> {
    cursor: Cursor<'a>,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    Eof,
}

macro_rules! mktoken {
    ($cursor:expr, $tok:expr) => {
        Ok(Spanned::new($tok, $cursor.span()))
    };
}

#[derive(Clone, Debug)]
struct Cursor<'a> {
    start: usize,
    src: &'a str,
    iter: CharIndices<'a>,
}

impl<'a> Cursor<'a> {
    pub fn peek(&self) -> Option<char> {
        self.iter.clone().peekable().peek().map(|(_, c)| c).cloned()
    }

    pub fn peek_n(&self, n: usize) -> Option<char> {
        let mut iter = self.iter.clone();

        for _ in 0..n - 1 {
            if iter.next().is_none() {
                return None;
            }
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
        let eat = self.peek().map_or(false, |c| f(c));
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
        loop {
            if !self.eat(&f) {
                break;
            }
        }
    }

    pub fn advance(&mut self) {
        self.iter.next().expect("Not allowed to advance beyond end");
    }

    pub fn span(&self) -> Range<usize> {
        self.start..self.iter.offset()
    }

    pub fn str(&self) -> &'a str {
        &self.src[self.span()]
    }

    pub fn restart(&mut self) {
        self.start = self.iter.offset();
    }
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            cursor: Cursor {
                src,
                iter: src.char_indices(),
                start: 0,
            },
        }
    }

    fn eat_chars(cursor: &mut Cursor<'a>, s: &'static str) -> bool {
        let mut cur = cursor.clone();

        for c in s.chars() {
            if !cur.eat_char(c) {
                return false;
            }
        }

        *cursor = cur;
        true
    }

    pub fn next_token(&mut self) -> Result<Spanned<Token<'a>>, Error> {
        use Token as T;

        self.cursor.eat_while(|c| c.is_whitespace());
        self.cursor.restart();

        let mut cur = self.cursor.clone();

        let res = cur.peek().ok_or(Error::Eof).and_then(|c| match c {
            ':' if Self::eat_chars(&mut cur, ":=") => mktoken!(cur, T::ColonEqual),

            c if c.is_ascii_alphabetic() || c == '_' => {
                cur.advance();
                cur.eat_while(|c| c.is_ascii_alphanumeric() || c == '_');
                mktoken!(cur, T::Identifier(cur.str()))
            }

            c if c.is_numeric() => Self::eat_numeric(&mut cur),
            '.' if let Some(c) = cur.peek_n(2)
                && c.is_numeric() =>
            {
                Self::eat_numeric(&mut cur)
            }

            _ => {
                cur.eat_while(|c| !Self::is_delimiter(c));
                mktoken!(cur, T::Unknown)
            }
        });

        self.cursor = cur;

        res
    }

    fn is_delimiter(c: char) -> bool {
        match c {
            c if c.is_whitespace() => true,
            ';' | '(' | ')' | '{' | '}' | '[' | ']' => true,
            _ => false,
        }
    }

    fn eat_numeric(cur: &mut Cursor<'a>) -> Result<Spanned<Token<'a>>, Error> {
        cur.eat_while(|c| c.is_numeric());

        if cur.eat_char('.') {
            cur.eat_while(|c| c.is_numeric());

            mktoken!(
                cur,
                Token::Float(cur.str().parse().expect("Should have parsed a float"))
            )
        } else {
            mktoken!(
                cur,
                Token::Integer(cur.str().parse().expect("Should have parsed an integer"))
            )
        }
    }

    pub fn all_tokens(&mut self) -> Result<Vec<Spanned<Token>>, Error> {
        let mut vals = Vec::new();

        loop {
            match self.next_token() {
                Ok(st) => vals.push(st),
                Err(Error::Eof) => break,
            }
        }

        Ok(vals)
    }
}

#[cfg(test)]
mod test {
    use saft_common::span::Spanned;

    use crate::{lex::Lexer, token::Token};

    fn expect_spanned_tokens(src: &'static str, spanned_tokens: Vec<Spanned<Token<'static>>>) {
        assert_eq!(
            Lexer::new(src)
                .all_tokens()
                .expect("Got error when parsing tokens"),
            spanned_tokens
        );
    }

    fn expect_tokens(src: &'static str, tokens: Vec<Token<'static>>) {
        assert_eq!(
            Lexer::new(src)
                .all_tokens()
                .expect("Got error when parsing tokens")
                .iter()
                .map(|s| s.v.clone())
                .collect::<Vec<_>>(),
            tokens
        );
    }

    #[test]
    fn operators() {
        expect_spanned_tokens(" := ", vec![Spanned::new(Token::ColonEqual, 1..3)]);
    }

    #[test]
    fn integers() {
        expect_spanned_tokens("123", vec![Spanned::new(Token::Integer(123), 0..3)]);
        expect_spanned_tokens(
            " 1 3",
            vec![
                Spanned::new(Token::Integer(1), 1..2),
                Spanned::new(Token::Integer(3), 3..4),
            ],
        );
    }

    #[test]
    fn floats() {
        expect_spanned_tokens(
            "1.23 123. .123",
            vec![
                Spanned::new(Token::Float(1.23), 0..4),
                Spanned::new(Token::Float(123.0), 5..9),
                Spanned::new(Token::Float(0.123), 10..14),
            ],
        );
    }

    #[test]
    fn identifiers() {
        expect_spanned_tokens(
            "x f z _hej hej_ h_ej Hej hEj hej123 _123 _hej123",
            vec![
                Spanned::new(Token::Identifier("x"), 0..1),
                Spanned::new(Token::Identifier("f"), 2..3),
                Spanned::new(Token::Identifier("z"), 4..5),
                Spanned::new(Token::Identifier("_hej"), 6..10),
                Spanned::new(Token::Identifier("hej_"), 11..15),
                Spanned::new(Token::Identifier("h_ej"), 16..20),
                Spanned::new(Token::Identifier("Hej"), 21..24),
                Spanned::new(Token::Identifier("hEj"), 25..28),
                Spanned::new(Token::Identifier("hej123"), 29..35),
                Spanned::new(Token::Identifier("_123"), 36..40),
                Spanned::new(Token::Identifier("_hej123"), 41..48),
            ],
        )
    }
}
