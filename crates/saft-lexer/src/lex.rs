use crate::cursor::Cursor;

use saft_common::span::Spanned;

use crate::token::Token;

pub struct Lexer<'a> {
    cursor: Cursor<'a>,
}

macro_rules! mktoken {
    ($cursor:expr, $tok:expr) => {
        Spanned::new($tok, $cursor.span())
    };
}

macro_rules! eat_token {
    ($cursor:expr, $tok:expr) => {{
        $cursor.advance();
        mktoken!($cursor, $tok)
    }};
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            cursor: Cursor::new(src),
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

    pub fn next(&mut self) -> Spanned<Token> {
        use Token as T;

        self.cursor.eat_while(|c| c.is_whitespace());
        self.cursor.restart();

        let mut cur = self.cursor.clone();

        let res = cur
            .peek()
            .map(|c| match c {
                '(' => eat_token!(cur, T::LParen),
                ')' => eat_token!(cur, T::RParen),
                '{' => eat_token!(cur, T::LBrace),
                '}' => eat_token!(cur, T::RBrace),
                ',' => eat_token!(cur, T::Comma),
                ';' => eat_token!(cur, T::Semicolon),

                '=' => eat_token!(cur, T::Equal),
                '+' => eat_token!(cur, T::Plus),
                '-' => eat_token!(cur, T::Minus),
                '*' => eat_token!(cur, T::Star),
                '/' => eat_token!(cur, T::Slash),
                '^' => eat_token!(cur, T::Caret),

                ':' if Self::eat_chars(&mut cur, ":=") => mktoken!(cur, T::ColonEqual),

                c if c.is_ascii_alphabetic() || c == '_' => {
                    cur.advance();
                    cur.eat_while(|c| c.is_ascii_alphanumeric() || c == '_');

                    match cur.str() {
                        "nil" => mktoken!(cur, T::Nil),
                        "fn" => mktoken!(cur, T::Fn),
                        "return" => mktoken!(cur, T::Return),
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

    fn is_delimiter(c: char) -> bool {
        match c {
            c if c.is_whitespace() => true,
            ';' | '(' | ')' | '{' | '}' | '[' | ']' | ',' => true,
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
            match self.next() {
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

    use crate::{lex::Lexer, token::Token as T};

    fn expect_spanned_tokens(src: &'static str, spanned_tokens: Vec<Spanned<T>>) {
        assert_eq!(Lexer::new(src).all_tokens(), spanned_tokens);
    }

    fn spanned<'a>(t: T, r: Range<usize>) -> Spanned<T> {
        Spanned::new(t, span(r))
    }

    #[test]
    fn integers() {
        expect_spanned_tokens("123", vec![spanned(T::Integer(123), 0..3)]);
        expect_spanned_tokens(
            " 1 3",
            vec![spanned(T::Integer(1), 1..2), spanned(T::Integer(3), 3..4)],
        );
    }

    #[test]
    fn floats() {
        expect_spanned_tokens(
            "1.23 123. .123",
            vec![
                spanned(T::Float(1.23), 0..4),
                spanned(T::Float(123.0), 5..9),
                spanned(T::Float(0.123), 10..14),
            ],
        );
    }

    #[test]
    fn identifiers() {
        expect_spanned_tokens(
            "x f z _hej hej_ h_ej Hej hEj hej123 _123 _hej123",
            vec![
                spanned(T::Identifier("x".into()), 0..1),
                spanned(T::Identifier("f".into()), 2..3),
                spanned(T::Identifier("z".into()), 4..5),
                spanned(T::Identifier("_hej".into()), 6..10),
                spanned(T::Identifier("hej_".into()), 11..15),
                spanned(T::Identifier("h_ej".into()), 16..20),
                spanned(T::Identifier("Hej".into()), 21..24),
                spanned(T::Identifier("hEj".into()), 25..28),
                spanned(T::Identifier("hej123".into()), 29..35),
                spanned(T::Identifier("_123".into()), 36..40),
                spanned(T::Identifier("_hej123".into()), 41..48),
            ],
        )
    }

    #[test]
    fn whitespace_eof() {
        let mut lexer = Lexer::new("   \t\n ");
        assert_eq!(lexer.next(), spanned(T::Eof, 6..6));
    }

    #[test]
    fn keywords() {
        expect_spanned_tokens("nil", vec![spanned(T::Nil, 0..3)]);
    }

    #[test]
    fn operators() {
        expect_spanned_tokens(
            "= := + - * / ^",
            vec![
                spanned(T::Equal, 0..1),
                spanned(T::ColonEqual, 2..4),
                spanned(T::Plus, 5..6),
                spanned(T::Minus, 7..8),
                spanned(T::Star, 9..10),
                spanned(T::Slash, 11..12),
                spanned(T::Caret, 13..14),
            ],
        );
    }

    #[test]
    fn fn_() {
        expect_spanned_tokens(
            "fn test(a, b, c) {}",
            vec![
                spanned(T::Fn, 0..2),
                spanned(T::Identifier("test".into()), 3..7),
                spanned(T::LParen, 7..8),
                spanned(T::Identifier("a".into()), 8..9),
                spanned(T::Comma, 9..10),
                spanned(T::Identifier("b".into()), 11..12),
                spanned(T::Comma, 12..13),
                spanned(T::Identifier("c".into()), 14..15),
                spanned(T::RParen, 15..16),
                spanned(T::LBrace, 17..18),
                spanned(T::RBrace, 18..19),
            ],
        )
    }
}
