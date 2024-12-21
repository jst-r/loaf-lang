use std::fmt::Display;

use crate::parser::{BinOp, Expr, PrefixOp};

#[derive(Debug)]
pub struct Module {
    functions: Vec<Function>,
}

pub struct Local {
    name: String,
    ty: Type,
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

pub enum Type {
    I32,
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
            compile_expr(*lhs, res);
            compile_expr(*rhs, res);

            match op {
                BinOp::Add => res.push(Instruction::Add),
                BinOp::Subtract => res.push(Instruction::Subtract),
                BinOp::Multiply => res.push(Instruction::Multiply),
                BinOp::Divide => res.push(Instruction::Divide),
            }
        }
        Expr::Prefix { op, rhs } => match op {
            PrefixOp::Negate => {
                res.push(Instruction::Constant(0));
                compile_expr(*rhs, res);
                res.push(Instruction::Subtract);
            }
        },
    }
}

pub struct WatFormatter {
    indent: usize,
    res: String,
}

impl WatFormatter {
    pub fn new() -> Self {
        Self {
            indent: 0,
            res: String::new(),
        }
    }

    pub fn format(&mut self, module: &Module) -> String {
        self.format_module(module);
        self.res.clone()
    }

    fn format_module(&mut self, module: &Module) {
        self.push_line("(module");
        self.indent += 2;

        for func in &module.functions {
            self.format_function(func);
        }

        self.indent -= 2;
        self.push_line(")");
    }

    fn format_function(&mut self, func: &Function) {
        self.push_line(format!("(func ${} (result i32)", func.name));
        self.indent += 2;
        for instr in &func.body {
            self.format_instruction(instr);
        }

        self.indent -= 2;
        self.push_line(")");

        if func.exported {
            self.push_line(format!("(export \"{}\" (func ${}))", func.name, func.name));
        }
    }

    fn format_instruction(&mut self, instr: &Instruction) {
        use Instruction::*;

        let line = match instr {
            Constant(i) => format!("i32.const {}", i),
            Add => "i32.add".to_string(),
            Subtract => "i32.sub".to_string(),
            Multiply => "i32.mul".to_string(),
            Divide => "i32.div_s".to_string(),
        };

        self.push_line(line);
    }

    fn push_line<T: Display>(&mut self, line: T) {
        self.res
            .push_str(&format!("{:indent$}{}\n", "", line, indent = self.indent));
    }
}

impl Module {
    pub fn as_wat(&self) -> String {
        let mut wat = WatFormatter::new();
        wat.format(self)
    }
}
