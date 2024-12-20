use std::io::BufRead;

mod parser;

use parser::{parse, LoafParser, Rule};
use pest::Parser;

fn main() {}

fn _repl() {
    for line in std::io::stdin().lock().lines() {
        let line = line.unwrap();
        match parse(&line) {
            Ok(expr) => {
                println!("{:?}", expr);
            }
            Err(err) => println!("{:?}", err),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{eval_expr, parse};

    #[test]
    fn test_eval_expr() {
        let cases = [
            ("1", 1),
            ("1 + 1", 2),
            ("1 + 2 * 3", 7),
            ("4 / 2 + 2 * 3", 8),
            ("(1 + 2) * 3", 9),
            ("-1 + 2", 1),
            ("(1 + 2) * -3", -9),
        ];

        for (input, expected) in cases {
            let expr = parse(input).unwrap();
            assert_eq!(eval_expr(expr), expected);
        }
    }
}
