use crate::parser::{BinOp, Expr, PrefixOp};

#[derive(Debug)]
pub struct Module {
    functions: Vec<Function>,
}
#[derive(Debug)]
pub struct Function {
    name: String,
    exported: bool,
    body: Vec<Instruction>,
}
#[derive(Debug)]
pub enum Instruction {
    Constant(i32),
    Add,
    Subtract,
    Multiply,
    Divide,
}

pub fn compile(expr: Expr) -> Module {
    let mut body = vec![];
    compile_expr(expr, &mut body);
    Module {
        functions: vec![Function {
            name: "main".to_string(),
            exported: true,
            body,
        }],
    }
}

pub fn compile_expr(expr: Expr, res: &mut Vec<Instruction>) {
    match expr {
        Expr::Integer(i) => res.push(Instruction::Constant(i)),
        Expr::BinOp { lhs, op, rhs } => {
            compile_expr(*rhs, res);
            compile_expr(*lhs, res);

            match op {
                BinOp::Add => res.push(Instruction::Add),
                BinOp::Subtract => res.push(Instruction::Subtract),
                BinOp::Multiply => res.push(Instruction::Multiply),
                BinOp::Divide => res.push(Instruction::Divide),
            }
        }
        Expr::Prefix { op, rhs } => {
            compile_expr(*rhs, res);

            match op {
                PrefixOp::Negate => {
                    res.push(Instruction::Constant(-1));
                    res.push(Instruction::Subtract)
                }
            }
        }
    }
}

impl Module {
    pub fn as_wat(&self) -> String {
        let mut res = String::new();
        res.push_str("(module\n");

        for func in &self.functions {
            res.push_str(&func.as_wat());
        }
        res.push_str(")\n");

        res
    }
}

impl Function {
    pub fn as_wat(&self) -> String {
        let mut res = String::new();
        res.push_str("(func $");
        res.push_str(&self.name);
        res.push_str(" (result i32)\n");
        for instr in &self.body {
            res.push_str(&instr.as_wat());
        }
        res.push_str(")\n");
        if self.exported {
            res.push_str(format!("(export \"{0}\" (func ${0}))\n", self.name).as_str());
        }

        res
    }
}

impl Instruction {
    pub fn as_wat(&self) -> String {
        use Instruction::*;
        match self {
            Constant(i) => format!("\ti32.const {}\n", i),
            Add => "\ti32.add\n".to_string(),
            Subtract => "\ti32.sub\n".to_string(),
            Multiply => "\ti32.mul\n".to_string(),
            Divide => "\ti32.div_s\n".to_string(),
        }
    }
}
