mod wasm;

use std::{fmt::Display, vec};

use wasm::{Function, Instruction, Module, Type};

use crate::parser::{BinOp, Expr, PrefixOp, Statement};

pub struct Compiler {
    pub module: Module,
    pub function: Option<Function>,
    pub locals: Vec<Local>,
    pub scope_depth: usize,
    pub next_local_id: usize,
}

#[derive(Debug, Clone)]
pub struct Local {
    pub id: usize,
    pub name: String,
    pub ty: Type,
    pub depth: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            module: Module { functions: vec![] },
            locals: vec![],
            scope_depth: 0,
            function: None,
            next_local_id: 0,
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
        self.expr(value); // compile the value first to prevent use before declaration

        let local = Local {
            id: self.next_local_id,
            name: name.clone(),
            ty: Type::I32,
            depth: self.scope_depth,
        };
        self.next_local_id += 1;

        self.locals.push(local.clone());

        self.function
            .as_mut()
            .unwrap()
            .locals
            .push(local.as_wasm_local());

        self.push_instruction(Instruction::LocalSet(local.wasm_local_name()));
    }

    pub fn resolve_local(&mut self, name: String) -> String {
        for local in self.locals.iter().rev() {
            if local.name == name {
                return local.wasm_local_name();
            }
        }
        panic!("Local not found: {}", name);
    }

    pub fn function(&mut self, name: String, exported: bool, stmts: Vec<Statement>) {
        self.function = Some(Function {
            name,
            exported,
            body: vec![],
            locals: vec![],
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
            Expr::Identifier(name) => {
                let wasm_name = self.resolve_local(name.clone());
                self.push_instruction(Instruction::LocalGet(wasm_name));
            }
            Expr::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts {
                    self.statement(stmt);
                }
                self.end_scope();
            }
        }
    }

    fn assignment(&mut self, target: Expr, op: BinOp, val: Expr) {
        if let Expr::Identifier(name) = target {
            let name = self.resolve_local(name.clone());
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

    pub fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    pub fn end_scope(&mut self) {
        self.scope_depth -= 1;
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

impl Local {
    pub fn as_wasm_local(&self) -> wasm::Local {
        wasm::Local {
            name: self.wasm_local_name(),
            ty: self.ty.clone(),
        }
    }

    pub fn wasm_local_name(&self) -> String {
        format!("{}_{}", self.name, self.id)
    }
}
