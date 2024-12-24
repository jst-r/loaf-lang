use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct Module {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub exported: bool,
    pub locals: Vec<Local>,
    pub body: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub struct Local {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Constant(i32),
    Add,
    Subtract,
    Multiply,
    Divide,
    Drop,
    Return,
    LocalSet(String),
    LocalGet(String),
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
    I32,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
        }
    }
}
