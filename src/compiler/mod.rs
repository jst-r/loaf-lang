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
    Return,
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
            Statement::Expr(expr) => {
                self.expr(expr);
                self.push_instruction(Instruction::Drop);
            }
            Statement::VarDeclaration { name, value } => {
                self.var_declaration(name, value);
            }
            Statement::Return { value } => {
                self.expr(value);
                self.push_instruction(Instruction::Return);
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
                let op_instr = match op {
                    BinOp::Assign | BinOp::AddAssign | BinOp::SubtractAssign => {
                        self.assignment(*lhs, op, *rhs);
                        return;
                    }
                    BinOp::Add => Instruction::Add,
                    BinOp::Subtract => Instruction::Subtract,
                    BinOp::Multiply => Instruction::Multiply,
                    BinOp::Divide => Instruction::Divide,
                };

                self.expr(*lhs);
                self.expr(*rhs);
                self.push_instruction(op_instr);
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

    fn assignment(&mut self, target: Expr, op: BinOp, val: Expr) {
        if let Expr::Identifier(name) = target {
            match op {
                BinOp::Assign => {
                    self.expr(val);
                    self.push_instruction(Instruction::LocalSet(name.clone()));
                }
                BinOp::AddAssign => {
                    self.push_instruction(Instruction::LocalGet(name.clone()));
                    self.expr(val);
                    self.push_instruction(Instruction::Add);
                    self.push_instruction(Instruction::LocalSet(name.clone()));
                }
                BinOp::SubtractAssign => {
                    self.push_instruction(Instruction::LocalGet(name.clone()));
                    self.expr(val);
                    self.push_instruction(Instruction::Subtract);
                    self.push_instruction(Instruction::LocalSet(name.clone()));
                }
                _ => panic!("Unexpected assignment operator: {:?}", op),
            }
            self.push_instruction(Instruction::LocalGet(name)); // needed for associativity
        } else {
            panic!("Expected identifier, found: {:?}", target);
        }
    }

    pub fn push_instruction(&mut self, instr: Instruction) {
        self.function.as_mut().unwrap().body.push(instr);
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
        let name = if func.exported {
            format!("(export \"{}\")", func.name)
        } else {
            format!("${}", func.name)
        };

        self.push_line(format!("(func {name} (result i32)"));
        self.indent += 2;
        for local in &func.locals {
            self.push_line(format!("(local ${} {})", local.name, local.ty));
        }
        for instr in &func.body {
            self.format_instruction(instr);
        }

        self.indent -= 2;
        self.push_line(")");
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
            Return => "return".to_string(),
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
