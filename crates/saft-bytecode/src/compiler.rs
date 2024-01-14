use saft_ast as ast;
use saft_common::span::Spanned;

use crate::{chunk::Chunk, op::Op};

enum Error {}

struct Env {
    base: usize,
}

impl Env {
    pub fn new(base: usize) -> Self {
        Self { base }
    }
}

struct Compiler {
    //
    stack_i: usize,
    envs: Vec<Env>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            stack_i: 0,
            envs: vec![Env::new(0)],
        }
    }

    pub fn compile_module(&mut self, module: &ast::Module) -> Result<Chunk, Error> {
        let mut chunk = Chunk::new();

        for stmt in &module.stmts {
            self.compile_stmt(stmt, &mut chunk)?
        }

        Ok(chunk)
    }

    fn compile_fn(&mut self, function: &ast::Function) -> Result<Chunk, Error> {
        todo!()
    }

    fn compile_stmt(
        &mut self,
        stmt: &Spanned<ast::Statement>,
        chunk: &mut Chunk,
    ) -> Result<(), Error> {
        match &stmt.v {
            ast::Statement::Expr(e) => {
                self.compile_expr(e, chunk)?;
                chunk.emit(Op::Pop);
            }
            ast::Statement::Declare { ident, expr } => {
                self.compile_expr(expr, chunk)?;
                todo!("Declare the variable in the scopes");
            }
            ast::Statement::Return(e) => {
                self.compile_expr(e, chunk)?;
                chunk.emit(Op::Return);
            }
            ast::Statement::Item(ast::Item::Function(fun)) => {
                let fn_chunk = self.compile_fn(fun)?;
                todo!("create function resource and push function value");
            }
        }

        Ok(())
    }

    fn compile_block(
        &mut self,
        block: &Spanned<ast::Block>,
        chunk: &mut Chunk,
    ) -> Result<(), Error> {
        self.enter_scope();
        for stmt in &block.v.stmts {
            self.compile_stmt(stmt, chunk)?;
        }

        if let Some(tail) = &block.v.tail {
            self.compile_expr(tail, chunk)?;
        } else {
            chunk.emit(Op::Nil);
        }

        self.exit_scope_trailing(chunk);

        Ok(())
    }

    fn compile_expr(&mut self, expr: &Spanned<ast::Expr>, chunk: &mut Chunk) -> Result<(), Error> {
        match &expr.v {
            ast::Expr::Nil => chunk.emit(Op::Nil),
            ast::Expr::Bool(b) => chunk.emit(Op::Bool(*b)),
            ast::Expr::Float(f) => chunk.emit(Op::Float(*f)),
            ast::Expr::Integer(i) => chunk.emit(Op::Integer(*i)),
            ast::Expr::String(s) => chunk.emit(Op::String(s.clone())),
            ast::Expr::Var(ident) => {
                let i = self.lookup(ident)?;
                todo!()
            }
            ast::Expr::Vec(exprs) => {
                for expr in exprs {
                    self.compile_expr(expr, chunk)?;
                }
                chunk.emit(Op::Vec(exprs.len()));
            }
            ast::Expr::Grouping(box e) => self.compile_expr(e, chunk)?,
            ast::Expr::Block(block) => self.compile_block(block, chunk)?,
            ast::Expr::If(cond, body, else_) => {
                self.compile_expr(cond, chunk)?;
                let else_jump = chunk.emit_i(Op::JmpFalse(0));
                self.compile_block(body, chunk)?;
                let end_jump = chunk.emit_i(Op::Jmp(0));

                // Else
                let else_offset = chunk.end();
                self.patch_jump(else_jump, else_offset, chunk);
                if let Some(box else_) = else_ {
                    self.compile_expr(else_, chunk)?;
                } else {
                    chunk.emit(Op::Nil);
                }

                let end_offset = chunk.end();
                self.patch_jump(end_jump, end_offset, chunk);
            }
            ast::Expr::Loop(_) => todo!(),
            ast::Expr::Break(_) => todo!(),
            ast::Expr::Neg(_) => todo!(),
            ast::Expr::Not(e) => {
                self.compile_expr(e, chunk)?;
                chunk.emit(Op::Not);
            }
            ast::Expr::Assign(_, _) => todo!(),
            ast::Expr::Add(lhs, rhs) => self.binary(chunk, lhs, rhs, Op::Add)?,
            ast::Expr::Sub(lhs, rhs) => self.binary(chunk, lhs, rhs, Op::Sub)?,
            ast::Expr::Mul(lhs, rhs) => self.binary(chunk, lhs, rhs, Op::Mul)?,
            ast::Expr::Div(lhs, rhs) => self.binary(chunk, lhs, rhs, Op::Div)?,
            ast::Expr::IDiv(lhs, rhs) => self.binary(chunk, lhs, rhs, Op::IDiv)?,
            ast::Expr::Pow(lhs, rhs) => self.binary(chunk, lhs, rhs, Op::Pow)?,
            ast::Expr::And(lhs, rhs) => self.binary(chunk, lhs, rhs, Op::And)?,
            ast::Expr::Or(lhs, rhs) => self.binary(chunk, lhs, rhs, Op::Or)?,
            ast::Expr::Lt(lhs, rhs) => self.binary(chunk, lhs, rhs, Op::Lt)?,
            ast::Expr::Le(lhs, rhs) => self.binary(chunk, lhs, rhs, Op::Le)?,
            ast::Expr::Gt(lhs, rhs) => self.binary(chunk, lhs, rhs, Op::Gt)?,
            ast::Expr::Ge(lhs, rhs) => self.binary(chunk, lhs, rhs, Op::Ge)?,
            ast::Expr::Eq(lhs, rhs) => self.binary(chunk, lhs, rhs, Op::Eq)?,
            ast::Expr::Ne(lhs, rhs) => self.binary(chunk, lhs, rhs, Op::Ne)?,

            ast::Expr::Call(callable, args) => {
                self.compile_expr(callable, chunk)?;
                for arg in args {
                    self.compile_expr(arg, chunk)?;
                }
                chunk.emit(Op::Call(args.len()));
            }
            ast::Expr::Index(indexable, index) => {
                self.compile_expr(indexable, chunk)?;
                self.compile_expr(index, chunk)?;
                chunk.emit(Op::Index);
            }
        }

        Ok(())
    }

    fn enter_scope(&mut self) {
        self.envs.push(Env::new(self.stack_i));
    }

    fn exit_scope(&mut self, chunk: &mut Chunk) {
        let env = self.envs.pop().unwrap();
        for i in 0..self.stack_i - env.base {
            chunk.emit(Op::Pop);
        }
    }

    fn exit_scope_trailing(&mut self, chunk: &mut Chunk) {
        let env = self.envs.pop().unwrap();
        let decls = self.stack_i - env.base;
        chunk.emit(Op::TrailPop(decls))
    }

    fn binary(
        &mut self,
        chunk: &mut Chunk,
        lhs: &Spanned<ast::Expr>,
        rhs: &Spanned<ast::Expr>,
        op: Op,
    ) -> Result<(), Error> {
        self.compile_expr(lhs, chunk)?;
        self.compile_expr(rhs, chunk)?;
        chunk.emit(Op::Add);
        Ok(())
    }

    fn patch_jump(&self, jump_i: usize, target: usize, chunk: &mut Chunk) {
        chunk.ops[jump_i] = match chunk.ops[jump_i] {
            Op::JmpFalse(_) => Op::JmpFalse(target),
            _ => panic!("Tried patching something else than a jump"),
        }
    }

    fn lookup(&self, ident: &Spanned<String>) -> Result<usize, Error> {
        todo!()
    }
}
