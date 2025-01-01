mod compiler;
mod parser;
mod runtime;

use compiler::Compiler;
use parser::parse;
use runtime::run;

const SOURCE: &str = r#"
{
let a = 1;
a += 1;
};
return a;
"#;

fn main() {
    let mut comp = Compiler::new();
    let prog = parse(SOURCE).unwrap();
    println!("{:?}", prog);
    let wat = comp.compile(prog).as_wat();
    println!("{}", &wat);
    println!("{:?}", run(&wat));
}

// fn _repl() {
//     for line in std::io::stdin().lock().lines() {
//         let line = line.unwrap();
//         match parse(&line) {
//             Ok(expr) => {
//                 let wat = compile(expr).as_wat();
//                 println!("{}", &wat);
//                 println!("{:?}", run(&wat));
//             }
//             Err(err) => println!("{:?}", err),
//         }
//     }
// }

// #[cfg(test)]
// mod tests {
//     use crate::{compiler::compile, parser::parse, runtime::run};

//     #[test]
//     fn test_eval_expr() {
//         let cases = [
//             ("1", 1),
//             ("1 + 1", 2),
//             ("1 + 2 * 3", 7),
//             ("4 / 2 + 2 * 3", 8),
//             ("(1 + 2) * 3", 9),
//             ("-1 + 2", 1),
//             ("(1 + 2) * -3", -9),
//         ];

// for (input, expected) in cases {
//     let expr = parse(input).unwrap();
//     let wat = compile(expr).as_wat();
//     let res = run(&wat).unwrap();
//     assert_eq!(
//         res, expected,
//         "case: {}\nresult: {}\nexpected: {}",
//         input, res, expected
//     );
// }
//     }
// }
