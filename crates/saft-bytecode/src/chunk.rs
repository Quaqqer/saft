use std::borrow::Borrow;

use saft_common::span::Span;

use crate::op::Op;

#[derive(Debug)]
pub struct Chunk {
    ops: Vec<Op>,
    spans: Vec<Span>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            ops: Vec::new(),
            spans: Vec::new(),
        }
    }

    pub fn emit(&mut self, op: Op, span: impl Borrow<Span>) {
        self.ops.push(op);
        self.spans.push(span.borrow().clone());
    }

    pub fn emit_i(&mut self, op: Op, span: impl Borrow<Span>) -> usize {
        let i = self.ops.len();
        self.ops.push(op);
        self.spans.push(span.borrow().clone());
        i
    }

    pub fn end(&self) -> usize {
        self.ops.len()
    }

    pub fn get_op(&self, i: usize) -> Option<&Op> {
        self.ops.get(i)
    }

    pub fn get_mut_op(&mut self, i: usize) -> Option<&mut Op> {
        self.ops.get_mut(i)
    }

    pub fn get_span(&self, i: usize) -> Option<&Span> {
        self.spans.get(i)
    }
}
