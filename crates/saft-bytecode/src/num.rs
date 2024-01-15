use crate::value::Cast;

#[derive(Debug, Clone, Copy)]
pub enum Num {
    Bool(bool),
    Int(i64),
    Float(f64),
}

impl Num {
    fn binary_upcast(&self, rhs: &Num) -> (Num, Num) {
        match (self, rhs) {
            (Num::Bool(a), Num::Bool(b)) => (Num::Int(*a as i64), Num::Int(*b as i64)),

            (Num::Int(_), Num::Int(_)) => (*self, *rhs),
            (Num::Float(_), Num::Float(_)) => (*self, *rhs),

            (Num::Bool(a), Num::Int(_)) => (Num::Int(*a as i64), *rhs),
            (Num::Bool(a), Num::Float(_)) => (Num::Float(*a as i64 as f64), *rhs),
            (Num::Int(a), Num::Float(_)) => (Num::Float(*a as f64), *rhs),

            (Num::Int(_), Num::Bool(b)) => (*self, Num::Int(*b as i64)),
            (Num::Float(_), Num::Bool(b)) => (*self, Num::Float(*b as i64 as f64)),
            (Num::Float(_), Num::Int(b)) => (*self, Num::Float(*b as f64)),
        }
    }

    pub fn or(&self, rhs: &Num) -> Option<bool> {
        match (self, rhs) {
            (Num::Bool(a), Num::Bool(b)) => Some(*a || *b),
            _ => None,
        }
    }

    pub fn and(&self, rhs: &Num) -> Option<bool> {
        match (self, rhs) {
            (Num::Bool(a), Num::Bool(b)) => Some(*a && *b),
            _ => None,
        }
    }

    pub fn eq(&self, rhs: &Num) -> bool {
        match self.binary_upcast(rhs) {
            (Num::Int(a), Num::Int(b)) => a == b,
            (Num::Float(a), Num::Float(b)) => a == b,
            _ => panic!("Should not be possible after upcast"),
        }
    }

    pub fn ne(&self, rhs: &Num) -> bool {
        !self.eq(rhs)
    }

    pub fn lt(&self, rhs: &Num) -> bool {
        match self.binary_upcast(rhs) {
            (Num::Int(a), Num::Int(b)) => a < b,
            (Num::Float(a), Num::Float(b)) => a < b,
            _ => panic!("Should not be possible after upcast"),
        }
    }

    pub fn le(&self, rhs: &Num) -> bool {
        match self.binary_upcast(rhs) {
            (Num::Int(a), Num::Int(b)) => a <= b,
            (Num::Float(a), Num::Float(b)) => a <= b,
            _ => panic!("Should not be possible after upcast"),
        }
    }

    pub fn gt(&self, rhs: &Num) -> bool {
        match self.binary_upcast(rhs) {
            (Num::Int(a), Num::Int(b)) => a > b,
            (Num::Float(a), Num::Float(b)) => a > b,
            _ => panic!("Should not be possible after upcast"),
        }
    }

    pub fn ge(&self, rhs: &Num) -> bool {
        match self.binary_upcast(rhs) {
            (Num::Int(a), Num::Int(b)) => a >= b,
            (Num::Float(a), Num::Float(b)) => a >= b,
            _ => panic!("Should not be possible after upcast"),
        }
    }

    pub fn mul(&self, rhs: &Num) -> Num {
        match self.binary_upcast(rhs) {
            (Num::Int(a), Num::Int(b)) => Num::Int(a * b),
            (Num::Float(a), Num::Float(b)) => Num::Float(a * b),
            _ => panic!("Should not be possible after upcast"),
        }
    }

    pub fn div(&self, rhs: &Num) -> Num {
        match self.binary_upcast(rhs) {
            (Num::Int(a), Num::Int(b)) => Num::Float(a as f64 / b as f64),
            (Num::Float(a), Num::Float(b)) => Num::Float(a / b),
            _ => panic!("Should not be possible after upcast"),
        }
    }

    pub fn idiv(&self, rhs: &Num) -> Num {
        match self.binary_upcast(rhs) {
            (Num::Int(a), Num::Int(b)) => Num::Int(a / b),
            (Num::Float(a), Num::Float(b)) => Num::Int((a / b) as i64),
            _ => panic!("Should not be possible after upcast"),
        }
    }

    pub fn add(&self, rhs: &Num) -> Num {
        match self.binary_upcast(rhs) {
            (Num::Int(a), Num::Int(b)) => Num::Int(a + b),
            (Num::Float(a), Num::Float(b)) => Num::Float(a + b),
            _ => panic!("Should not be possible after upcast"),
        }
    }

    pub fn sub(&self, rhs: &Num) -> Num {
        match self.binary_upcast(rhs) {
            (Num::Int(a), Num::Int(b)) => Num::Int(a - b),
            (Num::Float(a), Num::Float(b)) => Num::Float(a - b),
            _ => panic!("Should not be possible after upcast"),
        }
    }

    pub fn pow(&self, rhs: &Num) -> Num {
        match self.binary_upcast(rhs) {
            (Num::Int(a), Num::Int(b)) => Num::Int(a.pow(b as u32)),
            (Num::Float(a), Num::Float(b)) => Num::Float(a.powf(b)),
            _ => panic!("Should not be possible after upcast"),
        }
    }

    pub fn not(&self) -> Option<bool> {
        match self {
            Num::Bool(b) => Some(!b),
            _ => None,
        }
    }

    pub fn neg(&self) -> Num {
        match self {
            Num::Bool(b) => Num::Int((!*b) as i64),
            Num::Int(i) => Num::Int(-*i),
            Num::Float(f) => Num::Float(-*f),
        }
    }
}

impl From<f64> for Num {
    fn from(value: f64) -> Self {
        Num::Float(value)
    }
}

impl From<i64> for Num {
    fn from(value: i64) -> Self {
        Num::Int(value)
    }
}

impl From<bool> for Num {
    fn from(value: bool) -> Self {
        Num::Bool(value)
    }
}

impl Cast<bool> for Num {
    fn name() -> String {
        "bool".into()
    }

    fn cast(&self) -> Option<bool> {
        match self {
            Num::Bool(b) => Some(*b),
            _ => None,
        }
    }
}

impl Cast<i64> for Num {
    fn name() -> String {
        "int".into()
    }

    fn cast(&self) -> Option<i64> {
        match self {
            Num::Int(i) => Some(*i),
            _ => None,
        }
    }
}

impl Cast<f64> for Num {
    fn name() -> String {
        "float".into()
    }

    fn cast(&self) -> Option<f64> {
        match self {
            Num::Float(f) => Some(*f),
            _ => None,
        }
    }
}
