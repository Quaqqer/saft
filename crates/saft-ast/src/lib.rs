use saft_common::span::Spanned;

pub type Ident = String;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub stmts: Vec<Spanned<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expr(Spanned<Expr>),
    Declare {
        ident: Spanned<Ident>,
        expr: Spanned<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(Spanned<Ident>),
    Integer(i64),
    Float(f64),
    Nil,
}
