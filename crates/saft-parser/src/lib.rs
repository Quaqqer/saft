#![feature(let_chains)]

use std::collections::VecDeque;

use ast::{Expr, Item, Module, Statement};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use saft_ast as ast;
use saft_common::span::{Span, Spanned};
use saft_lexer::{lex::Lexer, token::Token};

mod prec {
    pub const NONE: i32 = 0;
    pub const ASSIGN: i32 = 1;
    pub const TERM: i32 = 2;
    pub const FACTOR: i32 = 3;
    pub const UNARY: i32 = 4;
    pub const EXP: i32 = 5;
    pub const CALL: i32 = 6;
    pub const PRIMARY: i32 = 5;
}

#[derive(Clone, Debug)]
pub enum Error {
    UnexpectedToken {
        got: Spanned<Token>,
        expected: String,
    },
}

impl Error {
    pub fn diagnostic<FileId>(&self, file_id: FileId) -> Diagnostic<FileId> {
        match self {
            Error::UnexpectedToken { got, expected } => Diagnostic::error()
                .with_message("Got an unexpected token")
                .with_labels(vec![Label::primary(file_id, got.s.r.clone()).with_message(
                    format!("Got {} but expected {}", got.v.describe(), expected),
                )]),
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    lookahead: VecDeque<Spanned<Token>>,
}

impl<'a> Parser<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            lexer: Lexer::new(s),
            lookahead: VecDeque::new(),
        }
    }

    pub fn parse_file(&'a mut self) -> Result<ast::Module, Error> {
        Ok(Module {
            stmts: self.parse_statements(Token::Eof)?,
        })
    }

    fn next(&mut self) -> Spanned<Token> {
        let t = self.peek();
        self.advance();
        t
    }

    fn eat(&mut self, t: Token) -> Result<Span, Error> {
        let st = self.peek();

        match st.v {
            v if v == t => {
                self.advance();
                Ok(st.s)
            }
            _ => Err(Error::UnexpectedToken {
                got: st,
                expected: t.describe().into(),
            }),
        }
    }

    fn eat_ident(&mut self) -> Result<Spanned<String>, Error> {
        let st = self.peek();

        match st.v {
            Token::Identifier(ident) => {
                self.advance();
                Ok(st.s.spanned(ident))
            }
            _ => Err(Error::UnexpectedToken {
                got: st,
                expected: Token::Identifier("".into()).describe().into(),
            }),
        }
    }

    fn try_eat(&mut self, t: Token) -> bool {
        let st = self.peek();

        match st.v {
            v if v == t => {
                self.eat(t).expect("Was peeked, should not happen");
                true
            }
            _ => false,
        }
    }

    fn advance(&mut self) {
        if self.lookahead.len() > 0 {
            self.lookahead.pop_front();
        } else {
            let _ = self.lexer.next();
        }
    }

    fn peek(&mut self) -> Spanned<Token> {
        self.peek_n(1)
    }

    fn peek_n(&mut self, n: usize) -> Spanned<Token> {
        assert!(n > 0);

        while self.lookahead.len() < n {
            self.lookahead.push_back(self.lexer.next());
        }

        self.lookahead[n - 1].clone()
    }

    pub fn parse_single_statment(&mut self) -> Result<Spanned<ast::Statement>, Error> {
        let s = self.parse_statement()?;
        let next = self.next();

        match &next.v {
            Token::Eof => Ok(s),
            Token::Semicolon => {
                self.eat(Token::Eof)?;
                Ok(s)
            }
            _ => Err(Error::UnexpectedToken {
                got: next,
                expected: "End of file or ';'".into(),
            }),
        }
    }

    pub fn parse_statements(&mut self, end: Token) -> Result<Vec<Spanned<Statement>>, Error> {
        let mut stmts = Vec::new();

        while self.peek().v != end {
            stmts.push(self.parse_statement()?);
            self.eat(Token::Semicolon)?;
        }

        Ok(stmts)
    }

    pub fn parse_statement(&mut self) -> Result<Spanned<ast::Statement>, Error> {
        let st = self.peek();

        match st.v {
            Token::Identifier(_) if self.peek_n(2).v == Token::ColonEqual => {
                let ident = self.eat_ident().expect("Already peeked");
                self.eat(Token::ColonEqual).expect("Already peeked");
                let expr = self.parse_expr()?;

                let s = ident.s.join(&expr.s);
                Ok(Spanned::new(Statement::Declare { ident, expr }, s))
            }

            Token::Return => {
                let start = self.eat(Token::Return)?;
                let expr = self.parse_expr()?;
                let s = start.join(&expr.s);
                Ok(Spanned::new(Statement::Return(expr), s))
            }

            Token::LParen
            | Token::Minus
            | Token::Identifier(_)
            | Token::Float(_)
            | Token::Integer(_)
            | Token::Nil
            | Token::String(_)
            | Token::LBracket => {
                let expr = self.parse_expr()?;
                let s = expr.s.clone();
                Ok(Spanned::new(Statement::Expr(expr), s))
            }

            Token::Fn => self.parse_fn(),

            _ => self.unexpected(st, "statement"),
        }
    }

    pub fn parse_expr(&mut self) -> Result<Spanned<Expr>, Error> {
        self.parse_precedence(prec::NONE)
    }

    fn parse_primary_expr(&mut self) -> Result<Spanned<Expr>, Error> {
        let st = self.next();
        let start = st.s.clone();
        match st.v {
            Token::Identifier(ident) => Ok(Spanned::new(
                Expr::Var(Spanned::new(ident.to_string(), st.s.clone())),
                st.s,
            )),
            Token::Float(f) => Ok(Spanned::new(Expr::Float(f), st.s)),
            Token::Integer(i) => Ok(Spanned::new(Expr::Integer(i), st.s)),
            Token::Nil => Ok(Spanned::new(Expr::Nil, st.s)),
            Token::String(s) => Ok(st.s.spanned(Expr::String(s))),
            Token::LParen => {
                let start = st.s;
                let inner = self.parse_expr()?;
                let end = self.eat(Token::RParen)?;
                Ok(Spanned::new(
                    Expr::Grouping(Box::new(inner)),
                    start.join(end),
                ))
            }
            Token::Minus => {
                let expr = self.parse_precedence(prec::UNARY + 1)?;
                let s = st.s.join(&expr.s);
                Ok(Spanned::new(Expr::Neg(Box::new(expr)), s))
            }
            Token::LBracket => {
                let mut exprs = Vec::new();
                loop {
                    if self.peek().v == Token::RBracket {
                        break;
                    }

                    exprs.push(self.parse_expr()?);

                    if !self.try_eat(Token::Comma) {
                        break;
                    }
                }
                let end = self.eat(Token::RBracket)?;
                let s = start.join(end);
                Ok(s.spanned(Expr::Vec(exprs)))
            }

            _ => self.unexpected(st, "expression"),
        }
    }

    fn unexpected<T>(&mut self, got: Spanned<Token>, expected: &'static str) -> Result<T, Error> {
        Err(Error::UnexpectedToken {
            got,
            expected: expected.into(),
        })
    }

    fn parse_precedence(&mut self, min_prec: i32) -> Result<Spanned<Expr>, Error> {
        let mut expr = self.parse_primary_expr()?;

        while let Some((post_prec, post_rule)) = Self::post_rule(&self.peek().v)
            && min_prec <= post_prec
        {
            expr = post_rule(expr, self)?;
        }

        Ok(expr)
    }

    /// Postfix and infix rules
    fn post_rule(
        t: &Token,
    ) -> Option<(
        i32,
        Box<dyn Fn(Spanned<Expr>, &mut Parser) -> Result<Spanned<Expr>, Error>>,
    )> {
        macro_rules! binop {
            ($prec:expr, $left_assoc:expr, $variant:path) => {
                Some((
                    $prec,
                    Box::new(|lhs, parser| {
                        parser.next();
                        let next_prec = if $left_assoc { $prec + 1 } else { $prec };
                        let rhs = parser.parse_precedence(next_prec)?;
                        let s = lhs.s.join(&rhs.s);
                        Ok(Spanned::new($variant(Box::new(lhs), Box::new(rhs)), s))
                    }),
                ))
            };
        }

        match t {
            Token::Equal => binop!(prec::ASSIGN, false, Expr::Assign),
            Token::Plus => binop!(prec::TERM, true, Expr::Add),
            Token::Minus => binop!(prec::TERM, true, Expr::Sub),
            Token::Star => binop!(prec::FACTOR, true, Expr::Mul),
            Token::Slash => binop!(prec::FACTOR, true, Expr::Div),
            Token::Caret => binop!(prec::EXP, false, Expr::Pow),
            Token::LParen => Some((
                prec::CALL,
                Box::new(|lhs, parser| {
                    parser.eat(Token::LParen)?;

                    let mut arguments = Vec::new();

                    while parser.peek().v != Token::RParen {
                        let arg = parser.parse_expr()?;
                        arguments.push(arg);

                        let ate_comma = parser.try_eat(Token::Comma);
                        if !ate_comma {
                            break;
                        }
                    }

                    let end = parser.eat(Token::RParen)?;
                    let s = lhs.s.join(end);

                    Ok(Spanned::new(Expr::Call(Box::new(lhs), arguments), s))
                }),
            )),
            Token::LBracket => Some((
                prec::CALL,
                Box::new(|lhs, parser| {
                    parser.eat(Token::LBracket)?;
                    let index = parser.parse_expr()?;
                    let end = parser.eat(Token::RBracket)?;

                    Ok(lhs
                        .s
                        .join(end)
                        .spanned(Expr::Index(Box::new(lhs), Box::new(index))))
                }),
            )),
            _ => None,
        }
    }

    fn parse_fn(&mut self) -> Result<Spanned<Statement>, Error> {
        let start = self.eat(Token::Fn)?;
        let ident = self.eat_ident()?;

        let mut params = Vec::new();

        self.eat(Token::LParen)?;
        while self.peek().v != Token::RParen {
            let param = self.eat_ident()?;
            params.push(param);

            if !self.try_eat(Token::Comma) {
                break;
            }
        }

        self.eat(Token::RParen)?;

        self.eat(Token::LBrace)?;

        let body = self.parse_statements(Token::RBrace)?;

        let end = self.eat(Token::RBrace)?;

        Ok(Spanned::new(
            Statement::Item(Item::Fn {
                ident,
                params,
                body,
            }),
            start.join(end),
        ))
    }
}

#[cfg(test)]
mod test {
    use saft_ast::Expr;
    use saft_common::span::{spanned, Spanned};
    use saft_lexer::token::Token;

    use crate::Parser;

    fn test_expr(s: &str, expected: &Spanned<Expr>) {
        let res = Parser::new(s).parse_expr().unwrap();
        assert_eq!(&res, expected);
    }

    #[test]
    fn lookahead() {
        let mut parser = Parser::new("a + b + c");
        assert_eq!(parser.peek(), spanned(Token::Identifier("a".into()), 0..1));
        assert_eq!(parser.peek_n(2), spanned(Token::Plus, 2..3));
        parser.eat_ident().unwrap();
        assert_eq!(parser.peek(), spanned(Token::Plus, 2..3));
    }

    #[test]
    fn precedence() {
        test_expr(
            "a + b + c",
            &spanned(
                Expr::Add(
                    Box::new(spanned(
                        Expr::Add(
                            Box::new(spanned(Expr::Var(spanned("a".into(), 0..1)), 0..1)),
                            Box::new(spanned(Expr::Var(spanned("b".into(), 4..5)), 4..5)),
                        ),
                        0..5,
                    )),
                    Box::new(spanned(Expr::Var(spanned("c".into(), 8..9)), 8..9)),
                ),
                0..9,
            ),
        );

        test_expr(
            "a + b * c",
            &spanned(
                Expr::Add(
                    Box::new(spanned(Expr::Var(spanned("a".into(), 0..1)), 0..1)),
                    Box::new(spanned(
                        Expr::Mul(
                            Box::new(spanned(Expr::Var(spanned("b".into(), 4..5)), 4..5)),
                            Box::new(spanned(Expr::Var(spanned("c".into(), 8..9)), 8..9)),
                        ),
                        4..9,
                    )),
                ),
                0..9,
            ),
        );
    }

    #[test]
    fn associativity() {
        test_expr(
            "a + b + c",
            &spanned(
                Expr::Add(
                    Box::new(spanned(
                        Expr::Add(
                            Box::new(spanned(Expr::Var(spanned("a".into(), 0..1)), 0..1)),
                            Box::new(spanned(Expr::Var(spanned("b".into(), 4..5)), 4..5)),
                        ),
                        0..5,
                    )),
                    Box::new(spanned(Expr::Var(spanned("c".into(), 8..9)), 8..9)),
                ),
                0..9,
            ),
        );

        test_expr(
            "a ^ b ^ c",
            &spanned(
                Expr::Pow(
                    Box::new(spanned(Expr::Var(spanned("a".into(), 0..1)), 0..1)),
                    Box::new(spanned(
                        Expr::Pow(
                            Box::new(spanned(Expr::Var(spanned("b".into(), 4..5)), 4..5)),
                            Box::new(spanned(Expr::Var(spanned("c".into(), 8..9)), 8..9)),
                        ),
                        4..9,
                    )),
                ),
                0..9,
            ),
        );
    }
}
