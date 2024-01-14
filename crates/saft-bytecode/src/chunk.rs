use crate::op::Op;

pub struct Chunk {
    pub ops: Vec<Op>,
}

impl Chunk {
    pub fn new() -> Self {
        Self { ops: Vec::new() }
    }

    pub fn emit(&mut self, op: Op) {
        self.ops.push(op);
    }

    pub fn emit_i(&mut self, op: Op) -> usize {
        let i = self.ops.len();
        self.ops.push(op);
        i
    }

    pub fn end(&self) -> usize {
        self.ops.len()
    }
}
