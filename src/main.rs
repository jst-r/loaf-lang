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
                println!("{}", &wat);
                println!("{:?}", run(&wat));
            }
            Err(err) => println!("{:?}", err),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{compiler::compile, parser::parse, runtime::run};

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
            let wat = compile(expr).as_wat();
            let res = run(&wat).unwrap();
            assert_eq!(
                res, expected,
                "case: {}\nresult: {}\nexpected: {}",
                input, res, expected
            );
        }
    }
}
