use std::rc::Rc;

use codespan_reporting::diagnostic::{Diagnostic, Label};
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
    pub fn interpret_chunk(&mut self, chunk: Rc<Chunk>) -> Result<(), Error> {
        self.call_stack.push(CallFrame::new(chunk, 0));

        self.run()
    }

    pub fn interpret_expr(&mut self, chunk: Rc<Chunk>) -> Result<Value, Error> {
        self.interpret_chunk(chunk)?;
        Ok(self.pop())
    }

    fn run(&mut self) -> Result<(), Error> {
        while {
            let call_frame = self.call_stack.last().unwrap();
            call_frame.i < call_frame.chunk.end()
        } {
            let call_frame = self.call_stack.last().unwrap();
            let op = call_frame.chunk.get_op(call_frame.i).unwrap().clone();
            let s = call_frame.chunk.get_span(call_frame.i).unwrap().clone();

            self.eval_op(&op, &s)?;
        }

        Ok(())
    }

    fn eval_op(&mut self, op: &Op, s: &Span) -> Result<(), Error> {
        match op {
            Op::Pop => {
                self.pop();
            }
            Op::PopN(n) => {
                self.stack.truncate(self.stack.len() - n);
            }
            Op::Return => todo!(),
            Op::Nil => self.push(Value::Nil),
            Op::Bool(b) => self.push(Value::Num(Num::Bool(*b))),
            Op::Float(f) => self.push(Value::Num(Num::Float(*f))),
            Op::Integer(i) => self.push(Value::Num(Num::Int(*i))),
            Op::String(_) => todo!(),
            Op::Var(stack_ptr) => {
                let cpy =
                    self.stack[self.call_stack.last().unwrap().stack_base + stack_ptr].clone();
                self.push(cpy)
            }
            Op::JmpFalse(i) => {
                if let Value::Num(Num::Bool(b)) = self.stack.pop().unwrap() {
                    if !b {
                        self.call_stack.last_mut().unwrap().i = *i;
                        return Ok(());
                    }
                } else {
                    exotic!("If error", s, "Cannot use a non-bool value as a condition");
                }
            }
            Op::JmpTrue(i) => {
                if let Value::Num(Num::Bool(b)) = self.stack.pop().unwrap() {
                    if b {
                        self.call_stack.last_mut().unwrap().i = *i;
                        return Ok(());
                    }
                } else {
                    exotic!("If error", s, "Cannot use a non-bool value as a condition");
                }
            }
            Op::Jmp(i) => {
                self.call_stack.last_mut().unwrap().i = *i;
                return Ok(());
            }
            Op::Not => self.unary(|a| a.not(), s, "not")?,
            Op::Negate => self.unary(|a| a.neg(), s, "negation")?,
            Op::Add => self.binop(|a, b| a.add(b), s, "add")?,
            Op::Pow => self.binop(|a, b| a.pow(b), s, "pow")?,
            Op::IDiv => self.binop(|a, b| a.idiv(b), s, "integer division")?,
            Op::Div => self.binop(|a, b| a.div(b), s, "division")?,
            Op::Mul => self.binop(|a, b| a.mul(b), s, "multiplication")?,
            Op::Sub => self.binop(|a, b| a.sub(b), s, "subtraction")?,
            Op::And => self.binop(|a, b| a.add(b), s, "addition")?,
            Op::Or => self.binop(|a, b| a.or(b).map(Into::into), s, "or")?,
            Op::Lt => self.binop(|a, b| a.lt(b).map(Into::into), s, "less than")?,
            Op::Le => self.binop(|a, b| a.le(b).map(Into::into), s, "less or equal")?,
            Op::Gt => self.binop(|a, b| a.gt(b).map(Into::into), s, "greater than")?,
            Op::Ge => self.binop(|a, b| a.ge(b).map(Into::into), s, "greater or equal")?,
            Op::Eq => self.binop(|a, b| a.eq(b).map(Into::into), s, "equal")?,
            Op::Ne => self.binop(|a, b| a.ne(b).map(Into::into), s, "not equal")?,
            Op::TrailPop(n) => {
                let v = self.stack.pop().unwrap();
                for _ in 0..*n {
                    self.stack.pop().unwrap();
                }
                self.push(v);
            }
            Op::Call(_) => {}
            Op::Index => {
                let index = self.pop();
                let indexable = self.pop();
                match indexable.index(&index) {
                    Some(v) => self.push(v.clone()),
                    None => exotic!(
                        "Unindexable",
                        s,
                        format!(
                            "Cannot index '{}' by '{}'",
                            indexable.ty().name(),
                            index.ty().name()
                        )
                    ),
                }
            }
            Op::Vec(_) => todo!(),
            Op::Assign(i) => {
                self.stack[*i] = self.stack.last().unwrap().clone();
            }
            Op::AssignIndexable => {
                let value = self.pop();
                let index = self.pop();
                let indexable = self.pop();
                let success = indexable.index_assign(&index, value);

                if !success {
                    exotic!(
                        "Unassignable",
                        s,
                        format!(
                            "Cannot assign to '{}' indexed by '{}'",
                            indexable.ty().name(),
                            index.ty().name()
                        )
                    );
                };
            }
        }

        self.call_stack.last_mut().unwrap().i += 1;

        Ok(())
    }

    fn push(&mut self, v: Value) {
        self.stack.push(v);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn peek(&self) -> &Value {
        self.stack.last().unwrap()
    }

    fn unary<F>(&mut self, f: F, s: &Span, op_type: &'static str) -> Result<(), Error>
    where
        F: Fn(&Value) -> Option<Value>,
    {
        let val = self.pop();
        match f(&val) {
            Some(val) => self.push(val),
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
        F: Fn(&Value, &Value) -> Option<Value>,
    {
        let rhs = self.pop();
        let lhs = self.pop();
        match f(&lhs, &rhs) {
            Some(val) => self.push(val),
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

    pub fn get_stack(&self) -> &Vec<Value> {
        &self.stack
    }
}

pub enum VmItem {
    SaftFunction(SaftFunction),
}

struct SaftFunction {
    pub arity: usize,
    pub body: Rc<Chunk>,
}
