use std::io::BufRead;

mod compiler;
mod parser;
mod runtime;

use compiler::compile;
use parser::parse;
use runtime::run;

fn main() {
    _repl();
}

fn _repl() {
    for line in std::io::stdin().lock().lines() {
        let line = line.unwrap();
        match parse(&line) {
            Ok(expr) => {
                let wat = compile(expr).as_wat();
                println!("{}", wat);
                run(&wat).unwrap();
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
