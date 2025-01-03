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
    pub ty: WasmType,
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
pub enum WasmType {
    I32,
    F32,
}

impl Display for WasmType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WasmType::I32 => write!(f, "i32"),
            WasmType::F32 => write!(f, "f32"),
        }
    }
}
