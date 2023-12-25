use ast::{Expr, Module, Statement};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use saft_ast as ast;
use saft_common::span::Spanned;
use saft_lexer::{lex::Lexer, token::Token};

#[derive(Clone, Debug)]
pub enum Error {
    UnexpectedToken {
        got: Spanned<Token>,
        expected: &'static str,
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
}

pub trait Describeable<'a> {
    fn describe(&self) -> &'a str;
}

impl<'a> Describeable<'a> for Token {
    fn describe(&self) -> &'a str {
        match self {
            Token::Unknown => "unknown token",
            Token::Eof => "end of file",
            Token::Identifier(_) => "identifier",
            Token::Float(_) => "float",
            Token::Integer(_) => "integer",
            Token::ColonEqual => "':='",
            Token::Nil => "'nil'",
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            lexer: Lexer::new(s),
        }
    }
    pub fn parse_file(&'a mut self) -> Result<ast::Module, Error> {
        let mut stmts = Vec::<Spanned<ast::Statement>>::new();

        let peeked_token = self.lexer.peek().v;
        while peeked_token != Token::Eof {
            stmts.push(self.parse_statement()?);
        }

        Ok(Module { stmts })
    }

    pub fn parse_statement(&mut self) -> Result<Spanned<ast::Statement>, Error> {
        let st = self.lexer.peek();
        match st.v {
            Token::Identifier(_) | Token::Float(_) | Token::Integer(_) => {
                let expr = self.parse_expr()?;
                let s = expr.s.clone();
                Ok(Spanned::new(Statement::Expr(expr), s))
            }

            Token::ColonEqual | Token::Unknown | Token::Eof | Token::Nil => {
                self.unexpected(st, "statement")
            }
        }
    }

    fn stmt_recover(&mut self) {
        todo!()
    }

    pub fn parse_expr(&mut self) -> Result<Spanned<ast::Expr>, Error> {
        let st = self.lexer.peek();
        match st.v {
            Token::Identifier(ident) => {
                self.lexer.next_token();
                Ok(Spanned::new(Expr::Var(ident.to_string()), st.s))
            }
            Token::Float(f) => {
                self.lexer.next_token();
                Ok(Spanned::new(Expr::Float(f), st.s))
            }
            Token::Integer(i) => {
                self.lexer.next_token();
                Ok(Spanned::new(Expr::Integer(i), st.s))
            }
            Token::Nil => {
                self.lexer.next_token();
                Ok(Spanned::new(Expr::Nil, st.s))
            }
            Token::Unknown | Token::Eof | Token::ColonEqual => self.unexpected(st, "expression"),
        }
    }

    fn unexpected<T>(&mut self, got: Spanned<Token>, expected: &'static str) -> Result<T, Error> {
        Err(Error::UnexpectedToken { got, expected })
    }
}
