use pest::{iterators::Pairs, pratt_parser::PrattParser, Parser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct LoafParser;

#[derive(Debug)]
pub enum Statement {
    Expr(Expr),
    VarDeclaration { name: String, value: Expr },
    Return { value: Expr },
}

#[derive(Debug)]
pub enum Expr {
    Identifier(String),
    Integer(i32),
    BinOp {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
    Prefix {
        op: PrefixOp,
        rhs: Box<Expr>,
    },
    Block(Vec<Statement>),
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Assign,
    AddAssign,
    SubtractAssign,
}

#[derive(Debug)]
pub enum PrefixOp {
    Negate,
}

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        PrattParser::new()
        .op(Op::infix(assign, Right) | Op::infix(add_assign, Right) | Op::infix(subtract_assign, Right))
            .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
            .op(Op::prefix(negate))
    };
}

pub fn parse(input: &str) -> Result<Vec<Statement>, pest::error::Error<Rule>> {
    let pairs = LoafParser::parse(Rule::program, input)?;

    Ok(parse_statements(pairs))
}

pub fn parse_statements(pairs: Pairs<Rule>) -> Vec<Statement> {
    let mut stmts = vec![];

    for pair in pairs {
        match pair.as_rule() {
            Rule::var_declaration => {
                let mut inner = pair.into_inner();
                let name = inner.next().unwrap().as_str().to_string();
                let value = parse_expr(inner.next().unwrap().into_inner());
                stmts.push(Statement::VarDeclaration { name, value });
            }
            Rule::expr_stmt => {
                stmts.push(Statement::Expr(parse_expr(pair.into_inner())));
            }

            Rule::return_stmt => {
                let mut inner = pair.into_inner();
                let value = parse_expr(inner.next().unwrap().into_inner());
                stmts.push(Statement::Return { value });
            }

            Rule::EOI => {}
            rule => unreachable!("Expected statement, found rule: {:?}", rule),
        }
    }
    stmts
}

pub fn parse_expr(pairs: Pairs<Rule>) -> Expr {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::integer => Expr::Integer(primary.as_str().parse().unwrap()),
            Rule::expr => parse_expr(primary.into_inner()),
            Rule::identifier => Expr::Identifier(primary.as_str().to_string()),
            Rule::block_expression => Expr::Block(parse_statements(primary.into_inner())),
            rule => unreachable!("Expected atom, found rule: {:?}", rule),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::negate => Expr::Prefix {
                op: PrefixOp::Negate,
                rhs: Box::new(rhs),
            },
            rule => unreachable!("Expected prefix, found rule: {:?}", rule),
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::add => BinOp::Add,
                Rule::subtract => BinOp::Subtract,
                Rule::multiply => BinOp::Multiply,
                Rule::divide => BinOp::Divide,
                Rule::assign => BinOp::Assign,
                Rule::add_assign => BinOp::AddAssign,
                Rule::subtract_assign => BinOp::SubtractAssign,
                rule => unreachable!("Expected bin_op, found rule: {:?}", rule),
            };

            Expr::BinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        })
        .parse(pairs)
}
