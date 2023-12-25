use ast::{Expr, Module, Statement};
use saft_ast as ast;
use saft_common::span::Spanned;
use saft_lexer::{lex::Lexer, token::Token};

#[derive(Clone, Debug)]
pub enum Error<'a> {
    UnexpectedToken {
        got: Spanned<Token<'a>>,
        expected: &'static str,
    },
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

pub trait Describeable<'a> {
    fn describe(&self) -> &'a str;
}

impl<'a> Describeable<'a> for Token<'a> {
    fn describe(&self) -> &'a str {
        match self {
            Token::Unknown => "unknown token",
            Token::Eof => "end of file",
            Token::Identifier(_) => "identifier",
            Token::Float(_) => "float",
            Token::Integer(_) => "integer",
            Token::ColonEqual => "':='",
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

        while self.lexer.peek().v != Token::Eof {
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

            Token::ColonEqual | Token::Unknown | Token::Eof => self.unexpected(st, "statement"),
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
            Token::Unknown | Token::Eof | Token::ColonEqual => self.unexpected(st, "expression"),
        }
    }

    fn unexpected<T>(&mut self, got: Spanned<Token<'a>>, expected: &'static str) -> Result<T, ()> {
        Err(Error::UnexpectedToken { got, expected })
    }
}
