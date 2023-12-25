use saft_ast as ast;
use saft_lexer::lex::Lexer;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn parse_file(&mut self) -> ast::Module {
        todo!()
    }

    pub fn parse_statement(&mut self) -> ast::Statement {
        todo!()
    }

    pub fn parse_expr(&mut self) -> ast::Expr {
        todo!()
    }
}
