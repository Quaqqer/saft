use saft_common::span::Spanned;

#[derive(Debug)]
pub struct Ident(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ref {
    Item(ItemRef),
    Var(VarRef),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemRef(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarRef(pub usize);

#[derive(Debug)]
pub struct Module<Builtin> {
    pub items: Vec<Spanned<Item<Builtin>>>,
    pub stmts: Vec<Spanned<Stmt>>,
}

#[derive(Debug)]
pub enum Item<Builtin> {
    Function(Function),
    Builtin(Builtin),
}

#[derive(Debug)]
pub struct Function {
    pub params: Vec<Spanned<VarRef>>,
    pub body: Spanned<Block>,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Spanned<Expr>),
    Declare(Spanned<VarRef>, Spanned<Expr>),
    Return(Spanned<Expr>),
}

#[derive(Debug)]
pub enum Expr {
    Nil,
    Bool(bool),
    Float(f64),
    Integer(i64),
    Var(Ref),
    String(String),
    Vec(Vec<Spanned<Expr>>),

    Grouping(Box<Spanned<Expr>>),

    Block(Box<Spanned<Block>>),

    If(Spanned<If>),
    Loop(Box<UntailBlock>),
    Break(Box<Option<Spanned<Expr>>>),

    Unary(Box<Spanned<Expr>>, UnaryOp),
    Binary(Box<Spanned<Expr>>, Box<Spanned<Expr>>, BinaryOp),
    Assign(Box<Spanned<LExpr>>, Box<Spanned<Expr>>),

    Call(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),
    Index(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
}

#[derive(Debug)]
pub enum LExpr {
    Index(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Var(VarRef),
}

#[derive(Debug)]
pub struct If {
    pub cond: Box<Spanned<Expr>>,
    pub body: Box<Spanned<Block>>,
    pub else_: Box<Option<Spanned<Else>>>,
}

#[derive(Debug)]
pub enum Else {
    Block(Spanned<Block>),
    If(Spanned<If>),
}

#[derive(Debug)]
pub enum UnaryOp {
    Plus,
    Negate,
    Not,
}

#[derive(Debug)]
pub enum BinaryOp {
    Or,

    And,

    Eq,
    Ne,

    Lt,
    Le,
    Gt,
    Ge,

    Mul,
    Div,
    IDiv,

    Add,
    Sub,

    Pow,
}

#[derive(Debug)]
pub struct UntailBlock(pub Vec<Spanned<Stmt>>);

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Spanned<Stmt>>,
    pub tail: Option<Spanned<Expr>>,
}
