use std::rc::Rc;

use saft_common::span::Span;

use crate::{chunk::Chunk, num::Num, op::Op, value::Value};

struct CallFrame {
    i: usize,
    chunk: Rc<Chunk>,
    stack_base: usize,
}

impl CallFrame {
    fn new(chunk: Rc<Chunk>, stack_base: usize) -> Self {
        Self {
            i: 0,
            chunk,
            stack_base,
        }
    }
}

pub enum Error {
    Exotic {
        message: String,
        span: Span,
        note: Option<String>,
    },
}

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

pub struct Vm {
    call_stack: Vec<CallFrame>,
    stack: Vec<Value>,
    items: Vec<VmItem>,
}

impl Vm {
    pub fn new(items: Vec<VmItem>) -> Self {
        Self {
            call_stack: Vec::new(),
            stack: Vec::new(),
            items,
        }
    }
}

impl Vm {
    pub fn interpret_chunk(&mut self, chunk: Rc<Chunk>) {
        self.call_stack.push(CallFrame::new(chunk, 0));

        self.run();
    }

    fn run(&mut self) -> Result<(), Error> {
        let call_frame = self.call_stack.last().unwrap();
        while call_frame.i < call_frame.chunk.end() {
            let op = call_frame.chunk.get_op(call_frame.i).unwrap();
            let s = call_frame.chunk.get_span(call_frame.i).unwrap();

            self.eval_op(op, s)?;
        }

        Ok(())
    }

    fn eval_op(&mut self, op: &Op, s: &Span) -> Result<(), Error> {
        match op {
            Op::Pop => {
                self.stack.pop().unwrap();
            }
            Op::Return => todo!(),
            Op::Nil => self.stack.push(Value::Nil),
            Op::Bool(b) => self.stack.push(Value::Num(Num::Bool(*b))),
            Op::Float(f) => self.stack.push(Value::Num(Num::Float(*f))),
            Op::Integer(i) => self.stack.push(Value::Num(Num::Int(*i))),
            Op::String(_) => todo!(),
            Op::Var(stack_ptr) => {
                let cpy =
                    self.stack[self.call_stack.last().unwrap().stack_base + stack_ptr].clone();
                self.stack.push(cpy)
            }
            Op::JmpFalse(_) => {}
            Op::JmpTrue(_) => todo!(),
            Op::Jmp(_) => todo!(),
            Op::Not => self.unary(|a| a.not(), s, "not")?,
            Op::Negate => self.unary(|a| a.neg(), s, "negation")?,
            Op::Add => self.binop(|a, b| a.add(&b), s, "add")?,
            Op::Pow => self.binop(|a, b| a.pow(&b), s, "pow")?,
            Op::IDiv => self.binop(|a, b| a.idiv(&b), s, "integer division")?,
            Op::Div => self.binop(|a, b| a.div(&b), s, "division")?,
            Op::Mul => self.binop(|a, b| a.mul(&b), s, "multiplication")?,
            Op::Sub => self.binop(|a, b| a.sub(&b), s, "subtraction")?,
            Op::And => self.binop(|a, b| a.add(&b), s, "addition")?,
            Op::Or => self.binop(|a, b| a.or(&b).map(Into::into), s, "or")?,
            Op::Lt => self.binop(|a, b| a.lt(&b).map(Into::into), s, "less than")?,
            Op::Le => self.binop(|a, b| a.le(&b).map(Into::into), s, "less or equal")?,
            Op::Gt => self.binop(|a, b| a.gt(&b).map(Into::into), s, "greater than")?,
            Op::Ge => self.binop(|a, b| a.ge(&b).map(Into::into), s, "greater or equal")?,
            Op::Eq => self.binop(|a, b| a.eq(&b).map(Into::into), s, "equal")?,
            Op::Ne => self.binop(|a, b| a.ne(&b).map(Into::into), s, "not equal")?,
            Op::TrailPop(_) => todo!(),
            Op::Call(_) => todo!(),
            Op::Index => todo!(),
            Op::Vec(_) => todo!(),
        }

        Ok(())
    }

    fn unary<F>(&mut self, f: F, s: &Span, op_type: &'static str) -> Result<(), Error>
    where
        F: Fn(Value) -> Option<Value>,
    {
        let val = self.stack.pop().unwrap();
        match f(val) {
            Some(val) => self.stack.push(val),
            None => exotic!(
                "Unary error",
                s,
                format!(
                    "Operation '{}' not supported for type {}",
                    op_type,
                    val.ty().name()
                )
            ),
        }
        Ok(())
    }

    fn binop<F>(&mut self, f: F, s: &Span, op_type: &'static str) -> Result<(), Error>
    where
        F: Fn(Value, Value) -> Option<Value>,
    {
        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();
        match f(lhs, rhs) {
            Some(val) => self.stack.push(val),
            None => exotic!(
                "Binary error",
                s,
                format!(
                    "Binary operation '{}' not supported for types '{}' and '{}'",
                    op_type,
                    lhs.ty().name(),
                    rhs.ty().name()
                )
            ),
        }
        Ok(())
    }
}

pub enum VmItem {
    SaftFunction(SaftFunction),
}

struct SaftFunction {
    pub arity: usize,
    pub body: Rc<Chunk>,
}
