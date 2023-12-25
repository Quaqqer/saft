pub type Ident<'a> = &'a str;

pub struct Module<'a> {
    pub stmts: Vec<Statement<'a>>,
}

pub enum Statement<'a> {
    Assign { lhs: Expr<'a>, rhs: Expr<'a> },
}

pub enum Expr<'a> {
    Var(Ident<'a>),
    Integer(i64),
    Float(f64),
}
