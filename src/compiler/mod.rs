use std::fmt::Display;

use crate::parser::{BinOp, Expr, PrefixOp, Statement};

#[derive(Debug, Clone)]
pub struct Module {
    functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    exported: bool,
    locals: Vec<Local>,
    body: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub struct Local {
    name: String,
    ty: Type,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Constant(i32),
    Add,
    Subtract,
    Multiply,
    Divide,
    Drop,
    LocalSet(String),
    LocalGet(String),
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
    I32,
}

pub struct Compiler {
    module: Module,
    function: Option<Function>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            module: Module { functions: vec![] },
            function: None,
        }
    }

    pub fn compile(&mut self, prog: Vec<Statement>) -> Module {
        self.function("main".to_string(), true, prog);
        self.module.clone()
    }

    pub fn statement(&mut self, stmt: Statement) {
        match stmt {
            Statement::Expr(expr) => self.expr(expr),
            Statement::VarDeclaration { name, value } => {
                self.var_declaration(name, value);
            }
        }
    }

    pub fn var_declaration(&mut self, name: String, value: Expr) {
        self.function.as_mut().unwrap().locals.push(Local {
            name: name.clone(),
            ty: Type::I32,
        });

        self.expr(value);
        self.push_instruction(Instruction::LocalSet(name));
    }

    pub fn function(&mut self, name: String, exported: bool, stmts: Vec<Statement>) {
        self.function = Some(Function {
            name,
            exported,
            locals: vec![],
            body: vec![],
        });

        for stmt in stmts {
            self.statement(stmt);
        }

        self.module.functions.push(self.function.take().unwrap());
    }

    pub fn expr(&mut self, expr: Expr) {
        match expr {
            Expr::Integer(i) => self.push_instruction(Instruction::Constant(i)),
            Expr::BinOp { lhs, op, rhs } => {
                self.expr(*lhs);
                self.expr(*rhs);

                match op {
                    BinOp::Add => self.push_instruction(Instruction::Add),
                    BinOp::Subtract => self.push_instruction(Instruction::Subtract),
                    BinOp::Multiply => self.push_instruction(Instruction::Multiply),
                    BinOp::Divide => self.push_instruction(Instruction::Divide),
                }
            }
            Expr::Prefix { op, rhs } => match op {
                PrefixOp::Negate => {
                    self.push_instruction(Instruction::Constant(0));
                    self.expr(*rhs);
                    self.push_instruction(Instruction::Subtract);
                }
            },
            Expr::Identifier(name) => self.push_instruction(Instruction::LocalGet(name)),
        }
    }

    pub fn push_instruction(&mut self, instr: Instruction) {
        self.function.as_mut().unwrap().body.push(instr);
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
        Expr::Identifier(name) => {}
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
        for local in &func.locals {
            self.push_line(format!("(local ${} {})", local.name, local.ty));
        }
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
            Drop => "drop".to_string(),
            LocalSet(name) => format!("local.set ${name}"),
            LocalGet(name) => format!("local.get ${name}"),
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

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
        }
    }
}
