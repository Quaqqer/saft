use std::borrow::Borrow;

use saft_common::span::{Spanned, Span};
use saft_ir as ir;

use crate::{chunk::Chunk, op::Op};

pub enum Error {}

struct Env {
    base: usize,
}

impl Env {
    pub fn new(base: usize) -> Self {
        Self { base }
    }
}

pub struct Compiler {
    stack_i: usize,
    envs: Vec<Env>,
}

impl Compiler {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            stack_i: 0,
            envs: vec![Env::new(0)],
        }
    }

    pub fn compile_module(&mut self, module: &ir::Module) -> Result<Chunk, Error> {
        let mut chunk = Chunk::new();

        for stmt in &module.stmts {
            self.compile_stmt(stmt, &mut chunk)?
        }

        Ok(chunk)
    }

    fn compile_fn(&mut self, function: &ir::Function) -> Result<Chunk, Error> {
        todo!()
    }

    fn compile_stmt(&mut self, stmt: &Spanned<ir::Stmt>, chunk: &mut Chunk) -> Result<(), Error> {
        match &stmt.v {
            ir::Stmt::Expr(e) => {
                self.compile_expr(e, chunk)?;
                chunk.emit(Op::Pop, &stmt.s);
            }
            ir::Stmt::Declare(ident, expr) => {
                self.compile_expr(expr, chunk)?;
                todo!("Declare the variable in the scopes");
            }
            ir::Stmt::Return(e) => {
                self.compile_expr(e, chunk)?;
                chunk.emit(Op::Return, &stmt.s);
            }
        }

        Ok(())
    }

    fn compile_block(
        &mut self,
        block: &Spanned<ir::Block>,
        chunk: &mut Chunk,
    ) -> Result<(), Error> {
        self.enter_scope();
        for stmt in &block.v.stmts {
            self.compile_stmt(stmt, chunk)?;
        }

        if let Some(tail) = &block.v.tail {
            self.compile_expr(tail, chunk)?;
        } else {
            chunk.emit(Op::Nil, &block.s);
        }

        self.exit_scope_trailing(chunk, &block.s);

        Ok(())
    }

    fn compile_expr(&mut self, expr: &Spanned<ir::Expr>, chunk: &mut Chunk) -> Result<(), Error> {
        let s = &expr.s;
        match &expr.v {
            ir::Expr::Nil => chunk.emit(Op::Nil, s),
            ir::Expr::Bool(b) => chunk.emit(Op::Bool(*b), s),
            ir::Expr::Float(f) => chunk.emit(Op::Float(*f), s),
            ir::Expr::Integer(i) => chunk.emit(Op::Integer(*i), s),
            ir::Expr::String(string) => chunk.emit(Op::String(string.clone()), s),
            ir::Expr::Var(ident) => match ident {
                ir::Ref::Item(_) => todo!(),
                ir::Ref::Var(var_ref) => {
                    let i = self.lookup(*var_ref)?;
                    todo!("{}", i)
                }
            },
            ir::Expr::Vec(exprs) => {
                for expr in exprs {
                    self.compile_expr(expr, chunk)?;
                }
                chunk.emit(Op::Vec(exprs.len()), s);
            }
            ir::Expr::Grouping(box e) => self.compile_expr(e, chunk)?,
            ir::Expr::Block(block) => self.compile_block(block, chunk)?,
            ir::Expr::If(if_) => self.compile_if(if_, chunk)?,
            ir::Expr::Loop(_) => todo!(),
            ir::Expr::Break(_) => todo!(),
            ir::Expr::Unary(expr, op) => {
                self.compile_expr(expr, chunk)?;

                match op {
                    ir::UnaryOp::Plus => {}
                    ir::UnaryOp::Negate => chunk.emit(Op::Negate, s),
                    ir::UnaryOp::Not => chunk.emit(Op::Not, s),
                };
            }
            ir::Expr::Assign(_, _) => todo!(),
            ir::Expr::Binary(lhs, rhs, op) => {
                let op = match op {
                    ir::BinaryOp::Or => Op::Or,
                    ir::BinaryOp::And => Op::And,
                    ir::BinaryOp::Eq => Op::Eq,
                    ir::BinaryOp::Ne => Op::Ne,
                    ir::BinaryOp::Lt => Op::Lt,
                    ir::BinaryOp::Le => Op::Le,
                    ir::BinaryOp::Gt => Op::Gt,
                    ir::BinaryOp::Ge => Op::Ge,
                    ir::BinaryOp::Mul => Op::Mul,
                    ir::BinaryOp::Div => Op::Div,
                    ir::BinaryOp::IDiv => Op::IDiv,
                    ir::BinaryOp::Add => Op::Add,
                    ir::BinaryOp::Sub => Op::Sub,
                    ir::BinaryOp::Pow => Op::Pow,
                };
                self.binary(chunk, lhs, rhs, op, s)?
            }

            ir::Expr::Call(callable, args) => {
                self.compile_expr(callable, chunk)?;
                for arg in args {
                    self.compile_expr(arg, chunk)?;
                }
                chunk.emit(Op::Call(args.len()), s);
            }
            ir::Expr::Index(indexable, index) => {
                self.compile_expr(indexable, chunk)?;
                self.compile_expr(index, chunk)?;
                chunk.emit(Op::Index, s);
            }
        }

        Ok(())
    }

    fn compile_if(
        &mut self,
        if_: &Spanned<ir::If>,
        chunk: &mut Chunk,
    ) -> Result<(), Error> {
        let ir::If { cond, body, else_ } = &if_.v;
        let s = &if_.s;
        self.compile_expr(cond, chunk)?;
        let else_jump = chunk.emit_i(Op::JmpFalse(0), s);
        self.compile_block(body, chunk)?;
        let end_jump = chunk.emit_i(Op::Jmp(0), s);

        // Else
        let else_offset = chunk.end();
        self.patch_jump(else_jump, else_offset, chunk);
        if let box Some(else_) = else_ {
            match &else_ {
                ir::Else::If(if_) => self.compile_if(if_, chunk)?,
                ir::Else::Block(block) => self.compile_block(block, chunk)?,
            }
            // self.compile_expr(else_, chunk)?;
        } else {
            chunk.emit(Op::Nil, s);
        }

        let end_offset = chunk.end();
        self.patch_jump(end_jump, end_offset, chunk);

        Ok(())
    }

    fn enter_scope(&mut self) {
        self.envs.push(Env::new(self.stack_i));
    }

    fn exit_scope(&mut self, chunk: &mut Chunk, span: impl Borrow<Span>) {
        let env = self.envs.pop().unwrap();
        for i in 0..self.stack_i - env.base {
            chunk.emit(Op::Pop, span.borrow());
        }
    }

    fn exit_scope_trailing(&mut self, chunk: &mut Chunk, span: impl Borrow<Span>) {
        let env = self.envs.pop().unwrap();
        let decls = self.stack_i - env.base;
        chunk.emit(Op::TrailPop(decls), span)
    }

    fn binary(
        &mut self,
        chunk: &mut Chunk,
        lhs: &Spanned<ir::Expr>,
        rhs: &Spanned<ir::Expr>,
        op: Op,
        span: impl Borrow<Span>,
    ) -> Result<(), Error> {
        self.compile_expr(lhs, chunk)?;
        self.compile_expr(rhs, chunk)?;
        chunk.emit(op, span);
        Ok(())
    }

    fn patch_jump(&self, jump_i: usize, target: usize, chunk: &mut Chunk) {
        let op = chunk.get_mut_op(jump_i).unwrap();
        *op = match op {
            Op::JmpFalse(_) => Op::JmpFalse(target),
            _ => panic!("Tried patching something else than a jump"),
        }
    }

    fn lookup(&self, ref_: ir::VarRef) -> Result<usize, Error> {
        todo!()
    }
}
