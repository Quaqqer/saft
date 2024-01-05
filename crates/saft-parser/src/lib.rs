#![feature(let_chains)]

use std::{borrow::Borrow, collections::VecDeque};

use ast::{Block, Expr, Item, Module, Statement};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use saft_ast as ast;
use saft_common::span::{Span, Spanned};
use saft_lexer::{lex::Lexer, token::Token};

macro_rules! unexpected {
    ($got:expr, $expected:expr) => {
        return Err(unexpected($got, $expected))
    };
}

fn unexpected(got: impl Borrow<Spanned<Token>>, expected: impl Into<String>) -> Error {
    Error::UnexpectedToken {
        got: got.borrow().clone(),
        expected: expected.into(),
    }
}

macro_rules! exotic {
    ($span:expr, $msg:expr) => {
        return Err(exotic($span, $msg))
    };
}

fn exotic(span: impl Borrow<Span>, expected: impl Into<String>) -> Error {
    Error::Exotic(span.borrow().spanned(expected.into()))
}

mod prec {
    pub const NONE: i32 = 0;
    pub const ASSIGN: i32 = 1;
    pub const OR: i32 = 2;
    pub const AND: i32 = 3;
    pub const EQUALITY: i32 = 4;
    pub const COMPARISON: i32 = 5;
    pub const TERM: i32 = 6;
    pub const FACTOR: i32 = 7;
    pub const UNARY: i32 = 8;
    pub const EXP: i32 = 9;
    pub const CALL: i32 = 10;
    pub const _PRIMARY: i32 = 11;
}

#[derive(Clone, Debug)]
pub enum Error {
    UnexpectedToken {
        got: Spanned<Token>,
        expected: String,
    },
    Exotic(Spanned<String>),
}

impl Error {
    pub fn diagnostic<FileId>(self, file_id: FileId) -> Diagnostic<FileId> {
        match self {
            Error::UnexpectedToken { got, expected } => Diagnostic::error()
                .with_message("Got an unexpected token")
                .with_labels(vec![Label::primary(file_id, got.s.r).with_message(
                    format!("Got {} but expected {}", got.v.describe(), expected),
                )]),
            Error::Exotic(sm) => Diagnostic::error()
                .with_message(sm.v)
                .with_labels(vec![Label::primary(file_id, sm.s.r)]),
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
            _ => unexpected!(st, t.describe()),
        }
    }

    fn eat_msg(&mut self, t: Token, msg: impl Into<String>) -> Result<Span, Error> {
        let st = self.peek();

        match st.v {
            v if v == t => {
                self.advance();
                Ok(st.s)
            }
            _ => exotic!(st.s, msg),
        }
    }

    fn eat_ident(&mut self) -> Result<Spanned<String>, Error> {
        let st = self.peek();

        match st.v {
            Token::Identifier(ident) => {
                self.advance();
                Ok(st.s.spanned(ident))
            }
            _ => unexpected!(st, Token::Identifier("".into()).describe()),
        }
    }

    fn try_eat(&mut self, t: Token) -> Option<Span> {
        let st = self.peek();

        match st.v {
            v if v == t => {
                self.eat(t).expect("Was peeked, should not happen");
                Some(st.s)
            }
            _ => None,
        }
    }

    fn advance(&mut self) {
        if !self.lookahead.is_empty() {
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
            _ => unexpected!(next, "end of file or ';'"),
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

    pub fn parse_block(&mut self, start: Option<Span>) -> Result<Spanned<Expr>, Error> {
        let start = match start {
            Some(s) => s,
            None => self.eat(Token::LBrace)?,
        };

        let mut stmts = Vec::new();

        while self.peek().v != Token::RBrace {
            let stmt = self.parse_statement()?;

            match &stmt.v {
                Statement::Expr(e) => {
                    if let Some(end) = self.try_eat(Token::RBrace) {
                        let s = start.join(end);

                        return Ok(s.spanned(Expr::Block(Block {
                            stmts,
                            tail: Some(Box::new(e.clone())),
                        })));
                    }
                }
                Statement::Item(_) => {}
                _ => {
                    self.eat_msg(Token::Semicolon, "Expected a ';' after a statement")?;
                }
            }

            stmts.push(stmt);
        }

        let end = self.eat(Token::RBrace)?;

        let s = start.join(end);

        Ok(s.spanned(Expr::Block(Block { stmts, tail: None })))
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
            | Token::LBracket
            | Token::True
            | Token::False
            | Token::Bang
            | Token::LBrace => {
                let expr = self.parse_expr()?;
                Ok(expr.s.clone().spanned(Statement::Expr(expr)))
            }

            Token::Fn => self.parse_fn(),

            _ => unexpected!(st, "statement"),
        }
    }

    pub fn parse_expr(&mut self) -> Result<Spanned<Expr>, Error> {
        self.parse_precedence(prec::NONE)
    }

    fn parse_primary_expr(&mut self) -> Result<Spanned<Expr>, Error> {
        let st = self.next();
        let start = st.s.clone();
        use Token as T;
        match st.v {
            T::Identifier(ident) => Ok(Spanned::new(
                Expr::Var(Spanned::new(ident.to_string(), st.s.clone())),
                st.s,
            )),
            T::Float(f) => Ok(Spanned::new(Expr::Float(f), st.s)),
            T::Integer(i) => Ok(Spanned::new(Expr::Integer(i), st.s)),
            T::Nil => Ok(Spanned::new(Expr::Nil, st.s)),
            T::String(s) => Ok(st.s.spanned(Expr::String(s))),
            T::LParen => {
                let start = st.s;
                let inner = self.parse_expr()?;
                let end = self.eat(T::RParen)?;
                Ok(Spanned::new(
                    Expr::Grouping(Box::new(inner)),
                    start.join(end),
                ))
            }
            T::Minus => {
                let expr = self.parse_precedence(prec::UNARY + 1)?;
                let s = st.s.join(&expr.s);
                Ok(Spanned::new(Expr::Neg(Box::new(expr)), s))
            }
            T::Bang => {
                let expr = self.parse_precedence(prec::UNARY + 1)?;
                let s = st.s.join(&expr.s);
                Ok(Spanned::new(Expr::Not(Box::new(expr)), s))
            }
            T::LBracket => {
                let mut exprs = Vec::new();
                loop {
                    if self.peek().v == T::RBracket {
                        break;
                    }

                    exprs.push(self.parse_expr()?);

                    if self.try_eat(T::Comma).is_none() {
                        break;
                    }
                }
                let end = self.eat(T::RBracket)?;
                let s = start.join(end);
                Ok(s.spanned(Expr::Vec(exprs)))
            }
            T::LBrace => {
                self.parse_block(Some(start))
            }
            T::True => Ok(st.s.spanned(Expr::Bool(true))),
            T::False => Ok(st.s.spanned(Expr::Bool(false))),

            _ => unexpected!(st, "expression"),
        }
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
    #[allow(clippy::type_complexity)]
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
            Token::Equal => binop!(prec::ASSIGN, true, Expr::Assign),

            Token::Or => binop!(prec::OR, true, Expr::Or),

            Token::And => binop!(prec::AND, true, Expr::And),

            Token::EqualEqual => binop!(prec::EQUALITY, true, Expr::Eq),
            Token::BangEqual => binop!(prec::EQUALITY, true, Expr::Ne),

            Token::Less => binop!(prec::COMPARISON, true, Expr::Lt),
            Token::LessEqual => binop!(prec::COMPARISON, true, Expr::Le),
            Token::Greater => binop!(prec::COMPARISON, true, Expr::Gt),
            Token::GreaterEqual => binop!(prec::COMPARISON, true, Expr::Ge),

            Token::Plus => binop!(prec::TERM, true, Expr::Add),
            Token::Minus => binop!(prec::TERM, true, Expr::Sub),

            Token::Star => binop!(prec::FACTOR, true, Expr::Mul),
            Token::Slash => binop!(prec::FACTOR, true, Expr::Div),
            Token::SlashSlash => binop!(prec::FACTOR, true, Expr::IDiv),

            Token::Caret => binop!(prec::EXP, false, Expr::Pow),

            Token::LParen => Some((
                prec::CALL,
                Box::new(|lhs, parser| {
                    parser.eat(Token::LParen)?;

                    let mut arguments = Vec::new();

                    while parser.peek().v != Token::RParen {
                        let arg = parser.parse_expr()?;
                        arguments.push(arg);

                        let ate_comma = parser.try_eat(Token::Comma).is_some();
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

            if self.try_eat(Token::Comma).is_none() {
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
