// use std::fmt;
pub mod error;
pub mod lexer;
pub mod parser;

fn main() {
    println!("Hello, world!");
}

// Expr ::= Primitive | (<name> Expr*) | ([<name> Expr]* Expr)
//          | (f {<name>*} Expr) | (def <name> Expr)
// Primitive ::= Symbol | String | Number | Boolean

// enum Expr {
//     Call(String, Vec<Expr>),
//     Let(Vec<(String, Expr)>, Box<Expr>),
//     Def(String, Box<Expr>),
//     Symbol(String),
//     Str(String),
//     Number(f64),
//     List(Vec<Expr>),
// }
//
// struct Scanner {
//     position: usize,
//     tokens: Vec<String>,
// }
//
// impl Scanner {
//     pub fn new(tokens: Vec<String>) -> Self {
//         Self {
//             position: 0,
//             tokens,
//         }
//     }
//
//     pub fn position(&self) -> usize {
//         self.position
//     }
//
//     pub fn peek(&self) -> Option<&String> {
//         self.tokens.get(self.position)
//     }
//
//     pub fn is_done(&self) -> bool {
//         self.position == self.tokens.len()
//     }
//
//     pub fn take(&mut self) -> Option<&String> {
//         match self.tokens.get(self.position) {
//             Some(token) => {
//                 self.position += 1;
//                 Some(token)
//             }
//             None => None,
//         }
//     }
// }

// fn parse_expr(scanner: &Scanner) -> Result<Expr, Err> {
//     let (token, expr) =
// }

// impl fmt::Display for Expr {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             Expr::Symbol(s) => write!(f, ":{}", s),
//             Expr::Str(s) => write!(f, "\"{}\"", s),
//             Expr::Number(n) => write!(f, "{}", n),
//             Expr::List(v) => {
//                 write!(f, "(list ")?;
//                 for (i, elem) in v.iter().enumerate() {
//                     write!(f, "{}", elem)?;
//                     if i + 1 < v.len() {
//                         write!(f, ", ")?;
//                     }
//                 }
//                 write!(f, ")")
//             }
//         }
//     }
// }
//
// #[test]
// fn test_expr_display() {
//     assert_eq!(
//         ":test".to_string(),
//         format!("{}", Expr::Symbol("test".to_string()))
//     );
//     assert_eq!(
//         "\"test\"".to_string(),
//         format!("{}", Expr::Str("test".to_string()))
//     );
//     assert_eq!("2.15".to_string(), format!("{}", Expr::Number(2.15)));
//     assert_eq!("2.0".to_string(), format!("{}", Expr::Number(2.0)));
// }
