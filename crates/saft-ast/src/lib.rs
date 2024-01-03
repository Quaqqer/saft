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
    Return(Spanned<Expr>),
    Item(Item),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(Spanned<Ident>),
    Integer(i64),
    Float(f64),
    String(String),
    Nil,
    Grouping(Box<Spanned<Expr>>),
    Vec(Vec<Spanned<Expr>>),

    Call(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),

    Neg(Box<Spanned<Expr>>),
    Assign(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Add(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Sub(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Mul(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Div(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Pow(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Index(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
}

impl Expr {
    pub fn describe(&self) -> &'static str {
        use Expr::*;
        match self {
            Var(..) => "variable",
            Integer(..) => "integer",
            Float(..) => "float",
            Nil => "nil",
            Assign(..) => "assignment",
            Add(..) => "addition",
            Sub(..) => "subtraction",
            Mul(..) => "multiplication",
            Div(..) => "division",
            Pow(..) => "pow",
            Grouping(..) => "grouping",
            Call(..) => "call",
            Neg(..) => "negation",
            String(..) => "string",
            Index(..) => "index",
            Vec(..) => "vec",
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
            Statement::Return(_) => "return",
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
