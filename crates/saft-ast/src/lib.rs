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
    Item(Item),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(Spanned<Ident>),
    Integer(i64),
    Float(f64),
    Nil,
    Grouping(Box<Spanned<Expr>>),

    Assign(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Add(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Sub(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Mul(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Div(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Pow(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
}

impl Expr {
    pub fn describe(&self) -> &'static str {
        match self {
            Expr::Var(..) => "variable",
            Expr::Integer(..) => "integer",
            Expr::Float(..) => "float",
            Expr::Nil => "nil",
            Expr::Assign(..) => "assignment",
            Expr::Add(..) => "addition",
            Expr::Sub(..) => "subtraction",
            Expr::Mul(..) => "multiplication",
            Expr::Div(..) => "division",
            Expr::Pow(_, _) => "pow",
            Expr::Grouping(_) => "grouping",
        }
    }
}

impl Statement {
    pub fn describe(&self) -> &'static str {
        match self {
            Statement::Expr(..) => "expression statement",
            Statement::Declare { .. } => "variable declaration",
            Statement::Item(item) => match item {
                Item::Fn { .. } => "function declaration",
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Fn {
        ident: Spanned<Ident>,
        params: Vec<Spanned<Ident>>,
        body: Vec<Spanned<Statement>>,
    },
}
