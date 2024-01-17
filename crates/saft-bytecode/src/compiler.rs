use std::{borrow::Borrow, collections::HashMap, rc::Rc};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use saft_common::span::{Span, Spanned};
use saft_ir as ir;

use crate::{chunk::Chunk, item::Item, op::Op, value::SaftFunction};

pub enum Error {
    Exotic {
        message: String,
        span: Span,
        note: Option<String>,
    },
}

impl Error {
    pub fn diagnostic<FileId>(&self, file_id: FileId) -> Diagnostic<FileId> {
        match self {
            Error::Exotic {
                message,
                span,
                note,
            } => Diagnostic::error().with_message(message).with_labels({
                let mut label = Label::primary(file_id, span.r.clone());
                if let Some(note) = note {
                    label = label.with_message(note);
                };
                vec![label]
            }),
        }
    }
}

#[allow(unused)]
macro_rules! exotic {
    ($msg:expr, $span:expr) => {
        return Err(Error::Exotic {
            message: $msg.into(),
            span: $span.clone(),
            note: None,
        })
    };

    ($msg:expr, $span:expr, $note:expr) => {
        return Err(Error::Exotic {
            message: $msg.into(),
            span: $span.clone(),
            note: Some($note.into()),
        })
    };
}

struct Scope {
    stack_base: usize,
}

impl Scope {
    pub fn new(stack_base: usize) -> Self {
        Self { stack_base }
    }
}

pub struct Compiler {
    stack_i: usize,
    scopes: Vec<Scope>,
    ref_offsets: HashMap<ir::VarRef, usize>,
    items: Vec<Item>,
}

impl Compiler {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            stack_i: 0,
            scopes: vec![Scope::new(0)],
            ref_offsets: HashMap::new(),
            items: vec![],
        }
    }

    pub fn compile_module(&mut self, module: &ir::Module) -> Result<(Chunk, Vec<Item>), Error> {
        let mut chunk = Chunk::new();

        let mut items = module
            .items
            .iter()
            .map(|item| {
                Ok::<_, Error>(match &item.v {
                    ir::Item::Function(function) => {
                        Item::SaftFunction(self.compile_fn(item.s.spanned(function))?)
                    }
                })
            })
            .try_collect::<Vec<_>>()?;

        self.items.append(&mut items);

        for stmt in &module.stmts {
            self.compile_stmt_(stmt, &mut chunk)?
        }

        Ok((chunk, self.items.clone()))
    }

    fn compile_fn(&mut self, function: Spanned<&ir::Function>) -> Result<SaftFunction, Error> {
        fn inner(
            compiler: &mut Compiler,
            function: Spanned<&ir::Function>,
        ) -> Result<SaftFunction, Error> {
            let Spanned { s, v: function } = function;
            let ir::Function { params, body } = function;

            let mut chunk = Chunk::new();
            for param in params {
                compiler.declare(param.v);
            }

            compiler.compile_block(body, &mut chunk)?;
            chunk.emit(Op::Return, s);

            Ok(SaftFunction {
                arity: params.len(),
                chunk: Rc::new(chunk),
            })
        }

        let prev_i = self.stack_i;

        let res = inner(self, function);

        self.stack_i = prev_i;

        res
    }

    pub fn compile_stmt(&mut self, stmt: &Spanned<ir::Stmt>) -> Result<Chunk, Error> {
        let mut chunk = Chunk::new();
        self.compile_stmt_(stmt, &mut chunk)?;
        Ok(chunk)
    }

    fn compile_stmt_(&mut self, stmt: &Spanned<ir::Stmt>, chunk: &mut Chunk) -> Result<(), Error> {
        match &stmt.v {
            ir::Stmt::Expr(e) => {
                self.compile_expr_(e, chunk)?;
                chunk.emit(Op::Pop, &stmt.s);
            }
            ir::Stmt::Declare(ident, expr) => {
                self.compile_expr_(expr, chunk)?;
                self.declare(ident.v);
            }
            ir::Stmt::Return(e) => {
                self.compile_expr_(e, chunk)?;
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
            self.compile_stmt_(stmt, chunk)?;
        }

        if let Some(tail) = &block.v.tail {
            self.compile_expr_(tail, chunk)?;
        } else {
            chunk.emit(Op::Nil, &block.s);
        }

        self.exit_scope_trailing(chunk, &block.s);

        Ok(())
    }

    pub fn compile_expr(&mut self, expr: &Spanned<ir::Expr>) -> Result<Chunk, Error> {
        let mut chunk = Chunk::new();
        self.compile_expr_(expr, &mut chunk)?;
        Ok(chunk)
    }

    fn compile_expr_(&mut self, expr: &Spanned<ir::Expr>, chunk: &mut Chunk) -> Result<(), Error> {
        let s = &expr.s;
        match &expr.v {
            ir::Expr::Nil => chunk.emit(Op::Nil, s),
            ir::Expr::Bool(b) => chunk.emit(Op::Bool(*b), s),
            ir::Expr::Float(f) => chunk.emit(Op::Float(*f), s),
            ir::Expr::Integer(i) => chunk.emit(Op::Integer(*i), s),
            ir::Expr::String(string) => chunk.emit(Op::String(string.clone()), s),
            ir::Expr::Var(ident) => match ident {
                ir::Ref::Item(item_ref) => chunk.emit(Op::Item(item_ref.0), s),
                ir::Ref::Var(var_ref) => {
                    let i = self.lookup(*var_ref)?;
                    chunk.emit(Op::Var(i), s);
                }
            },
            ir::Expr::Vec(exprs) => {
                for expr in exprs {
                    self.compile_expr_(expr, chunk)?;
                }
                chunk.emit(Op::Vec(exprs.len()), s);
            }
            ir::Expr::Grouping(box e) => self.compile_expr_(e, chunk)?,
            ir::Expr::Block(block) => self.compile_block(block, chunk)?,
            ir::Expr::If(if_) => self.compile_if(if_, chunk)?,
            ir::Expr::Loop(_) => todo!(),
            ir::Expr::Break(_) => todo!(),
            ir::Expr::Unary(expr, op) => {
                self.compile_expr_(expr, chunk)?;

                match op {
                    ir::UnaryOp::Plus => {}
                    ir::UnaryOp::Negate => chunk.emit(Op::Negate, s),
                    ir::UnaryOp::Not => chunk.emit(Op::Not, s),
                };
            }
            ir::Expr::Assign(lexpr, expr) => match &lexpr.v {
                ir::LExpr::Index(indexable, index) => {
                    self.compile_expr_(indexable, chunk)?;
                    self.compile_expr_(index, chunk)?;
                    self.compile_expr_(expr, chunk)?;
                    chunk.emit(Op::AssignIndexable, s);
                }
                ir::LExpr::Var(ref_) => {
                    self.compile_expr_(expr, chunk)?;
                    chunk.emit(Op::Assign(ref_.0), s);
                }
            },
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
                self.compile_expr_(callable, chunk)?;
                for arg in args {
                    self.compile_expr_(arg, chunk)?;
                }
                chunk.emit(Op::Call(args.len()), s);
            }
            ir::Expr::Index(indexable, index) => {
                self.compile_expr_(indexable, chunk)?;
                self.compile_expr_(index, chunk)?;
                chunk.emit(Op::Index, s);
            }
        }

        Ok(())
    }

    fn compile_if(&mut self, if_: &Spanned<ir::If>, chunk: &mut Chunk) -> Result<(), Error> {
        let ir::If { cond, body, else_ } = &if_.v;
        let s = &if_.s;
        self.compile_expr_(cond, chunk)?;
        let else_jump = chunk.emit_i(Op::JmpFalse(0), s);
        self.compile_block(body, chunk)?;
        let end_jump = chunk.emit_i(Op::Jmp(0), s);

        // Else
        let else_offset = chunk.end();
        self.patch_jump(else_jump, else_offset, chunk);
        if let box Some(else_) = else_ {
            match &else_.v {
                ir::Else::If(if_) => self.compile_if(if_, chunk)?,
                ir::Else::Block(block) => self.compile_block(block, chunk)?,
            }
        } else {
            chunk.emit(Op::Nil, s);
        }

        let end_offset = chunk.end();
        self.patch_jump(end_jump, end_offset, chunk);

        Ok(())
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Scope::new(self.stack_i));
    }

    #[allow(unused)]
    fn exit_scope(&mut self, chunk: &mut Chunk, span: impl Borrow<Span>) {
        let env = self.scopes.pop().unwrap();
        chunk.emit(Op::PopN(self.stack_i - env.stack_base), span);
    }

    fn exit_scope_trailing(&mut self, chunk: &mut Chunk, span: impl Borrow<Span>) {
        let env = self.scopes.pop().unwrap();
        let decls = self.stack_i - env.stack_base;
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
        self.compile_expr_(lhs, chunk)?;
        self.compile_expr_(rhs, chunk)?;
        chunk.emit(op, span);
        Ok(())
    }

    fn patch_jump(&self, jump_i: usize, target: usize, chunk: &mut Chunk) {
        let op = chunk.get_mut_op(jump_i).unwrap();
        *op = match op {
            Op::JmpFalse(_) => Op::JmpFalse(target),
            Op::JmpTrue(_) => Op::JmpTrue(target),
            Op::Jmp(_) => Op::Jmp(target),
            _ => panic!("Tried patching something else than a jump"),
        }
    }

    fn lookup(&self, ref_: ir::VarRef) -> Result<usize, Error> {
        Ok(*self.ref_offsets.get(&ref_).unwrap())
    }

    fn declare(&mut self, ident: ir::VarRef) {
        self.ref_offsets.insert(ident, self.stack_i);
        self.stack_i += 1;
    }
}
