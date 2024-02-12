use std::rc::Rc;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use saft_syntax::span::Span;

use crate::{
    chunk::Chunk,
    constant::Constant,
    num::Num,
    op::Op,
    value::{Function, IndexRes, Value},
};

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
}

impl Vm {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            call_stack: Vec::new(),
            stack: Vec::new(),
        }
    }
}

impl Vm {
    pub fn interpret_chunk(
        &mut self,
        chunk: Rc<Chunk>,
        constants: &[Constant],
    ) -> Result<(), Error> {
        self.call_stack.push(CallFrame::new(chunk, 0));

        let res = self.run(constants);

        self.call_stack.pop().unwrap();
        assert!(self.call_stack.is_empty());

        res
    }

    pub fn interpret_expr(
        &mut self,
        chunk: Rc<Chunk>,
        constants: &[Constant],
    ) -> Result<Value, Error> {
        self.interpret_chunk(chunk, constants)?;
        Ok(self.pop())
    }

    fn run(&mut self, constants: &[Constant]) -> Result<(), Error> {
        while {
            let call_frame = self.call_stack.last().unwrap();
            call_frame.i < call_frame.chunk.end()
        } {
            let call_frame = self.call_stack.last().unwrap();
            let op = call_frame.chunk.get_op(call_frame.i).unwrap().clone();
            let s = call_frame.chunk.get_span(call_frame.i).unwrap().clone();

            self.eval_op(&op, &s, constants)?;
        }

        Ok(())
    }

    fn eval_op(&mut self, op: &Op, s: &Span, constants: &[Constant]) -> Result<(), Error> {
        match op {
            Op::Pop => {
                self.pop();
            }
            Op::PopN(n) => {
                self.stack.truncate(self.stack.len() - n);
            }
            Op::Return => {
                let call_frame = self.call_stack.pop().unwrap();
                let ret = self.pop();
                self.stack.truncate(call_frame.stack_base);
                self.push(ret);

                return Ok(());
            }
            Op::Nil => self.push(Value::Nil),
            Op::Bool(b) => self.push(Value::Num(Num::Bool(*b))),
            Op::Float(f) => self.push(Value::Num(Num::Float(*f))),
            Op::Integer(i) => self.push(Value::Num(Num::Int(*i))),
            Op::String(s) => self.push(Value::String(s.clone())),
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
            Op::Call(n_args) => {
                let args = self.popn(*n_args);
                let fun = self.pop();

                match fun {
                    Value::Function(Function::SaftFunction(fun)) => {
                        if fun.arity != args.len() {
                            exotic!(
                                "Wrong parameters",
                                s,
                                format!(
                                    "Function expected {} arguments but got {}",
                                    fun.arity,
                                    args.len()
                                )
                            );
                        }

                        self.call_stack.last_mut().unwrap().i += 1;
                        self.enter_frame(fun.chunk.clone());
                        for arg in args {
                            self.push(arg);
                        }

                        return Ok(());
                    }
                    Value::Function(Function::NativeFunction(fun)) => {
                        let res = (fun.f)(self, args, s.clone())?;
                        self.push(res);
                    }
                    _ => {
                        exotic!(
                            "Uncallable",
                            s,
                            format!("Value of type '{}' is not callable", fun.ty().name())
                        )
                    }
                }
            }
            Op::Index => {
                let index = self.pop();
                let indexable = self.pop();
                match indexable.index(&index) {
                    IndexRes::Unindexable => exotic!(
                        "Unindexable",
                        s,
                        format!(
                            "Cannot index '{}' by '{}'",
                            indexable.ty().name(),
                            index.ty().name()
                        )
                    ),
                    IndexRes::OutOfBounds => exotic!(
                        "Out of bounds",
                        s,
                        format!(
                            "Cannot index '{}' by '{}'",
                            indexable.ty().name(),
                            index.ty().name()
                        )
                    ),
                    IndexRes::NonExistant => todo!(),
                    IndexRes::Value(v) => self.push(v.clone()),
                }
            }
            Op::Vec(n) => {
                let elems = self.popn(*n);
                self.push(Value::Vec(elems));
            }
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
            Op::Constant(ref_) => match &constants[*ref_] {
                Constant::Function(fun) => {
                    self.push(Value::Function(fun.clone()));
                }
            },
        }

        self.call_stack.last_mut().unwrap().i += 1;

        Ok(())
    }

    pub(crate) fn push(&mut self, v: Value) {
        self.stack.push(v);
    }

    pub(crate) fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    pub(crate) fn popn(&mut self, n: usize) -> Vec<Value> {
        self.stack.split_off(self.stack.len() - n)
    }

    #[allow(unused)]
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

    fn enter_frame(&mut self, chunk: Rc<Chunk>) {
        let stack_base = self.stack.len();
        self.call_stack.push(CallFrame {
            i: 0,
            stack_base,
            chunk,
        })
    }
}
