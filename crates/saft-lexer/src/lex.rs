use std::{collections::VecDeque, str::CharIndices};

use saft_common::span::{Span, Spanned};

use crate::token::Token;

pub struct Lexer<'a> {
    cursor: Cursor<'a>,
    lookahead: VecDeque<Spanned<Token>>,
}

macro_rules! mktoken {
    ($cursor:expr, $tok:expr) => {
        Spanned::new($tok, $cursor.span())
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

    pub fn span(&self) -> Span {
        Span::new(self.start..self.iter.offset())
    }

    pub fn str(&self) -> &'a str {
        &self.src[self.span().r]
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
            lookahead: VecDeque::new(),
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

    pub fn next_token(&mut self) -> Spanned<Token> {
        self.lookahead
            .pop_front()
            .unwrap_or_else(|| self.advance_token())
    }

    fn advance_token(&mut self) -> Spanned<Token> {
        use Token as T;

        self.cursor.eat_while(|c| c.is_whitespace());
        self.cursor.restart();

        let mut cur = self.cursor.clone();

        let res = cur
            .peek()
            .map(|c| match c {
                ':' if Self::eat_chars(&mut cur, ":=") => mktoken!(cur, T::ColonEqual),

                c if c.is_ascii_alphabetic() || c == '_' => {
                    cur.advance();
                    cur.eat_while(|c| c.is_ascii_alphanumeric() || c == '_');

                    match cur.str() {
                        "nil" => mktoken!(cur, T::Nil),
                        s => mktoken!(cur, T::Identifier(s.into())),
                    }
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
            })
            .unwrap_or_else(|| mktoken!(cur, T::Eof));

        self.cursor = cur;

        res
    }

    pub fn peek(&mut self) -> Spanned<Token> {
        self.peek_n(1)
    }

    pub fn peek_n(&mut self, n: usize) -> Spanned<Token> {
        while self.lookahead.len() < n {
            let next_token = self.advance_token();
            self.lookahead.push_back(next_token);
        }

        self.lookahead[n - 1].clone()
    }

    fn is_delimiter(c: char) -> bool {
        match c {
            c if c.is_whitespace() => true,
            ';' | '(' | ')' | '{' | '}' | '[' | ']' => true,
            _ => false,
        }
    }

    fn eat_numeric(cur: &mut Cursor<'a>) -> Spanned<Token> {
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

    pub fn all_tokens(&mut self) -> Vec<Spanned<Token>> {
        let mut vals = Vec::new();

        loop {
            match self.next_token() {
                Spanned { v: Token::Eof, .. } => break,
                t => vals.push(t),
            }
        }

        vals
    }
}

#[cfg(test)]
mod test {
    use std::ops::Range;

    use saft_common::span::{span, Spanned};

    use crate::{lex::Lexer, token::Token};

    fn expect_spanned_tokens(src: &'static str, spanned_tokens: Vec<Spanned<Token>>) {
        assert_eq!(Lexer::new(src).all_tokens(), spanned_tokens);
    }

    fn spanned<'a>(t: Token, r: Range<usize>) -> Spanned<Token> {
        Spanned::new(t, span(r))
    }

    #[test]
    fn operators() {
        expect_spanned_tokens(" := ", vec![spanned(Token::ColonEqual, 1..3)]);
    }

    #[test]
    fn integers() {
        expect_spanned_tokens("123", vec![spanned(Token::Integer(123), 0..3)]);
        expect_spanned_tokens(
            " 1 3",
            vec![
                spanned(Token::Integer(1), 1..2),
                spanned(Token::Integer(3), 3..4),
            ],
        );
    }

    #[test]
    fn floats() {
        expect_spanned_tokens(
            "1.23 123. .123",
            vec![
                spanned(Token::Float(1.23), 0..4),
                spanned(Token::Float(123.0), 5..9),
                spanned(Token::Float(0.123), 10..14),
            ],
        );
    }

    #[test]
    fn identifiers() {
        expect_spanned_tokens(
            "x f z _hej hej_ h_ej Hej hEj hej123 _123 _hej123",
            vec![
                spanned(Token::Identifier("x".into()), 0..1),
                spanned(Token::Identifier("f".into()), 2..3),
                spanned(Token::Identifier("z".into()), 4..5),
                spanned(Token::Identifier("_hej".into()), 6..10),
                spanned(Token::Identifier("hej_".into()), 11..15),
                spanned(Token::Identifier("h_ej".into()), 16..20),
                spanned(Token::Identifier("Hej".into()), 21..24),
                spanned(Token::Identifier("hEj".into()), 25..28),
                spanned(Token::Identifier("hej123".into()), 29..35),
                spanned(Token::Identifier("_123".into()), 36..40),
                spanned(Token::Identifier("_hej123".into()), 41..48),
            ],
        )
    }

    #[test]
    fn lookahead() {
        let mut lexer = Lexer::new("hej 123 456.789");
        assert_eq!(lexer.peek_n(3), spanned(Token::Float(456.789), 8..15));
        assert_eq!(
            lexer.next_token(),
            spanned(Token::Identifier("hej".into()), 0..3)
        );
        assert_eq!(lexer.peek_n(2), spanned(Token::Float(456.789), 8..15));
        assert_eq!(lexer.lookahead.len(), 2);
    }

    #[test]
    fn whitespace_eof() {
        let mut lexer = Lexer::new("   \t\n ");
        assert_eq!(lexer.next_token(), spanned(Token::Eof, 6..6));
    }

    #[test]
    fn keywords() {
        expect_spanned_tokens("nil", vec![spanned(Token::Nil, 0..3)]);
    }
}
