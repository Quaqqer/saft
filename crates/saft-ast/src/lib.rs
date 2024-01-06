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
pub struct Block {
    pub stmts: Vec<Spanned<Statement>>,
    pub tail: Option<Box<Spanned<Expr>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Nil,
    Bool(bool),
    Float(f64),
    Integer(i64),
    String(String),

    Var(Spanned<Ident>),
    Vec(Vec<Spanned<Expr>>),

    Grouping(Box<Spanned<Expr>>),
    Block(Spanned<Block>),

    If(
        Box<Spanned<Expr>>,
        Spanned<Block>,
        Option<Box<Spanned<Expr>>>,
    ),
    Loop(Vec<Spanned<Statement>>),
    Break(Box<Spanned<Expr>>),

    Neg(Box<Spanned<Expr>>),
    Not(Box<Spanned<Expr>>),
    Assign(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Add(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Sub(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Mul(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Div(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    IDiv(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Pow(Box<Spanned<Expr>>, Box<Spanned<Expr>>),

    And(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Or(Box<Spanned<Expr>>, Box<Spanned<Expr>>),

    Lt(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Le(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Gt(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Ge(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Eq(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Ne(Box<Spanned<Expr>>, Box<Spanned<Expr>>),

    Call(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),
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
            Bool(..) => "bool",
            And(..) => "and",
            Or(..) => "or",
            Lt(..) => "less than",
            Le(..) => "less or equal",
            Gt(..) => "greater than",
            Ge(..) => "greater or equal",
            Eq(..) => "equal",
            Ne(..) => "not equal",
            IDiv(..) => "integer division",
            Not(..) => "not",
            Block(..) => "block",
            If(..) => "if expression",
            Loop(_) => "loop",
            Break(_) => "break",
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
        body: Spanned<Block>,
    },
}
