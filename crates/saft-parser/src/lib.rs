#![feature(let_chains)]

use std::{collections::HashMap, rc::Rc};

use ast::{Expr, Module, Statement};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use saft_ast as ast;
use saft_common::span::Spanned;
use saft_lexer::{lex::Lexer, token::Token};

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
    infixes: HashMap<&'a str, (i32, bool, Rc<dyn Fn(Spanned<Expr>, Spanned<Expr>) -> Expr>)>,
}

impl<'a> Parser<'a> {
    pub fn new(s: &'a str) -> Self {
        let mut parser = Self {
            lexer: Lexer::new(s),
            infixes: HashMap::new(),
        };

        parser.insert_infix("=", 14, |lhs, rhs| {
            Expr::Assign(Box::new(lhs), Box::new(rhs))
        });

        parser
    }
    pub fn parse_file(&'a mut self) -> Result<ast::Module, Error> {
        let mut stmts = Vec::<Spanned<ast::Statement>>::new();

        let peeked_token = self.lexer.peek().v;
        while peeked_token != Token::Eof {
            stmts.push(self.parse_statement()?);
        }

        Ok(Module { stmts })
    }

    fn eat(&mut self, t: Token) -> Result<(), Error> {
        let st = self.lexer.next_token();
        match st.v {
            v if v == t => Ok(()),
            _ => Err(Error::UnexpectedToken {
                got: st,
                expected: t.describe(),
            }),
        }
    }

    pub fn parse_single_statment(&mut self) -> Result<Spanned<ast::Statement>, Error> {
        let s = self.parse_statement()?;
        self.eat(Token::Eof)?;
        Ok(s)
    }

    pub fn parse_statement(&mut self) -> Result<Spanned<ast::Statement>, Error> {
        let st = self.lexer.peek();
        match st.v {
            Token::Identifier(ident) if self.lexer.peek_n(2).v == Token::ColonEqual => {
                let ident_t = self.lexer.next_token();
                let _colon_equals = self.lexer.next_token();
                let expr = self.parse_expr()?;
                let expr_s = expr.s.clone();

                Ok(Spanned::new(
                    Statement::Declare {
                        ident: Spanned::new(ident, ident_t.s.clone()),
                        expr,
                    },
                    ident_t.s.join(&expr_s),
                ))
            }
            Token::Identifier(_) | Token::Float(_) | Token::Integer(_) | Token::Nil => {
                let expr = self.parse_expr()?;
                let s = expr.s.clone();
                Ok(Spanned::new(Statement::Expr(expr), s))
            }

            Token::Operator(_) | Token::ColonEqual | Token::Unknown | Token::Eof => {
                self.unexpected(st, "statement")
            }
        }
    }

    pub fn parse_expr(&mut self) -> Result<Spanned<ast::Expr>, Error> {
        let lhs = self.parse_primary_expr()?;
        self.parse_expr_infix(lhs, 0)
    }

    pub fn parse_expr_infix(
        &mut self,
        mut lhs: Spanned<ast::Expr>,
        min_prec: i32,
    ) -> Result<Spanned<ast::Expr>, Error> {
        while let Token::Operator(op) = self.lexer.peek().v
            && let Some((prec, _, f)) = self.infixes.get(op.as_str()).cloned()
            && prec >= min_prec
        {
            self.lexer.next_token();

            let mut rhs = self.parse_primary_expr()?;

            while let Token::Operator(op) = self.lexer.peek().v
                && let Some((prec2, left2, _)) = self.infixes.get(op.as_str()).cloned()
                && (prec < prec2 || (left2 && prec == prec2))
            {
                rhs = self.parse_expr_infix(rhs, prec + if prec2 < prec { 1 } else { 0 })?;
            }
            let s = lhs.s.join(&rhs.s);
            lhs = Spanned::new(f(lhs, rhs), s);
        }

        Ok(lhs)
    }

    pub fn parse_primary_expr(&mut self) -> Result<Spanned<ast::Expr>, Error> {
        let st = self.lexer.next_token();
        match st.v {
            Token::Identifier(ident) => Ok(Spanned::new(
                Expr::Var(Spanned::new(ident.to_string(), st.s.clone())),
                st.s,
            )),
            Token::Float(f) => Ok(Spanned::new(Expr::Float(f), st.s)),
            Token::Integer(i) => Ok(Spanned::new(Expr::Integer(i), st.s)),
            Token::Nil => Ok(Spanned::new(Expr::Nil, st.s)),
            Token::Unknown | Token::Eof | Token::ColonEqual | Token::Operator(..) => {
                self.unexpected(st, "expression")
            }
        }
    }

    fn unexpected<T>(&mut self, got: Spanned<Token>, expected: &'static str) -> Result<T, Error> {
        Err(Error::UnexpectedToken {
            got,
            expected: expected.into(),
        })
    }

    fn insert_infix<F>(&mut self, operator: &'static str, precedence: i32, create_expr: F)
    where
        F: Fn(Spanned<Expr>, Spanned<Expr>) -> Expr + 'static,
    {
        self.infixes
            .insert(operator, (precedence, true, Rc::new(create_expr)));
    }
}
