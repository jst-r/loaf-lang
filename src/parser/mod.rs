use pest::{
    iterators::{Pair, Pairs},
    pratt_parser::PrattParser,
    Parser,
};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct LoafParser;

#[derive(Debug)]
pub enum Statement {
    Expr(Expr),
    VarDeclaration { name: String, value: Expr },
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
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
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
            .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
            .op(Op::prefix(negate))
    };
}

pub fn parse(input: &str) -> Result<Vec<Statement>, pest::error::Error<Rule>> {
    let mut stmts = vec![];
    let pairs = LoafParser::parse(Rule::program, input)?;

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
            Rule::EOI => {}
            rule => unreachable!("Expected statement, found rule: {:?}", rule),
        }
    }

    Ok(stmts)
}

pub fn parse_expr(pairs: Pairs<Rule>) -> Expr {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::integer => Expr::Integer(primary.as_str().parse().unwrap()),
            Rule::expr => parse_expr(primary.into_inner()),
            Rule::identifier => Expr::Identifier(primary.as_str().to_string()),
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
