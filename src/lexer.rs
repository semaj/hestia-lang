// use regex::{Error, Regex};

use crate::error::HestiaErr;

enum Closeable {
    OpenParen,
    OpenSquareParen,
    OpenSquigglyParen,
    Quote,
}

enum Token {
    CloseParen,
    CloseSquareParen,
    CloseSquigglyParen,
    Slash,
    Func,
    Def,
    True,
    False,
    Number(f64),
    Str(String),
    Symbol(String),
    Closeable(Closeable),
}

struct AnnotatedToken {
    token: Token,
    line: usize,
    col_start: usize,
    col_end: usize,
}

struct Lexer {
    pub position: usize,
    line: usize,
    col: usize,
    chars: Vec<char>,
    state: LexerState,
    stack: Vec<Closeable>,
}

enum LexerState {
    Normal,
    Identifier,
    StringLiteral,
    SymbolLiteral,
    NumericLiteral,
    NumericLiteralAfterDot,
    Comment,
}

fn is_invalid_symbol_char(c: &char) -> bool {
    match c {
        '/' => true,
        ' ' => true,
        '\n' => true,
        '\r' => true,
        '\t' => true,
        '\\' => true,
        '|' => true,
        '@' => true,
        '_' => true,
        '#' => true,
        '(' => true,
        ')' => true,
        ']' => true,
        '[' => true,
        '{' => true,
        '}' => true,
        ':' => true,
        ';' => true,
        '\'' => true,
        '.' => true,
        '`' => true,
        '"' => true,
        _ => false,
    }
}

impl Lexer {
    pub fn new(chars: Vec<char>) -> Self {
        Self {
            position: 0,
            line: 0,
            col: 0,
            state: LexerState::Normal,
            stack: Vec::new(),
            chars,
        }
    }

    fn check_space_char(&self, c: &char) -> Result<(), HestiaErr> {
        match c {
            '\t' => Err(HestiaErr::Syntax(
                self.line,
                self.col,
                "tab character is not allowed".to_string(),
            )),
            '\r' => Err(HestiaErr::Syntax(
                self.line,
                self.col,
                "windows return character is not allowed".to_string(),
            )),
        }
    }

    // fn take_normal(&mut self) -> Option<AnnotatedToken> {}
    //
    // fn take_identifier(&mut self) -> Option<AnnotatedToken> {}
    //
    // fn take_string(&mut self) -> Option<AnnotatedToken> {}

    // Assumes first `:` has been peeked
    fn take_symbol(&mut self) -> Result<AnnotatedToken, HestiaErr> {
        let token = Vec::new();
        let line = self.line;
        let col_start = self.col;
        self.take()?;
        loop {
            match self.peek()? {
                ':' => {
                    let r = AnnotatedToken {
                        token: Token::Symbol(token.iter().collect()),
                        line,
                        col_start,
                        col_end: self.col,
                    };
                    self.take()?;
                    return Ok(r);
                }
                c => {
                    if is_invalid_symbol_char(c) {
                        HestiaErr::Syntax(
                            self.line,
                            self.col,
                            format!("invalid character {} in symbol", c),
                        )
                    }
                    token.push(c);
                }
            }
            self.take()?;
        }
    }

    // fn take_numeric(&mut self) -> Option<AnnotatedToken> {}
    //
    // fn take_comment(&mut self) -> Option<AnnotatedToken> {}

    // Step normal
    pub fn step(&mut self) -> Result<AnnotatedToken, HestiaErr> {
        match self.peek()? {
            '\n' => {
                self.take()?;
                self.line += 1;
                self.col = 0;
                self.step()
            }
            ' ' => {
                self.take()?;
                self.col += 1;
                self.step()
            }
            '(' => {
                self.stack.push(Closeable::OpenParen);
                let r = AnnotatedToken {
                    token: Token::Closeable(Closeable::OpenParen),
                    line: self.line,
                    col_start: self.col,
                    col_end: self.col,
                };
                self.take()?;
                Ok(r)
            }
            ')' => {
                match self.stack.pop() {
                    Some(Closeable::OpenParen) => Ok(()),
                    Some(p) => Err(HestiaErr::Syntax(
                        self.line,
                        self.col,
                        // TODO: implement Display for Closeable
                        format!("tried to close with ) but dangling open {}", p),
                    )),
                    None => Err(HestiaErr::Syntax(
                        self.line,
                        self.col,
                        "tried to close with ) but no open parens".to_string(),
                    )),
                }?;
                let r = AnnotatedToken {
                    token: Token::Closeable(Closeable::OpenParen),
                    line: self.line,
                    col_start: self.col,
                    col_end: self.col,
                };
                self.take()?;
                Ok(Token::CloseParen)
            }
            '[' => {
                self.take()?;
                self.stack.push(Closeable::OpenSquareParen);
                Ok(Token::Closeable(Closeable::OpenSquareParen))
            }
            ']' => {
                match self.stack.pop() {
                    Some(Closeable::OpenSquareParen) => (),
                    Some(p) => HestiaErr::Syntax(
                        self.line,
                        self.col,
                        // TODO: implement Display for Closeable
                        format!("tried to close with ] but dangling open {}", p),
                    ),
                    None => HestiaErr::Syntax(
                        self.line,
                        self.col,
                        "tried to close with ] but no open parens".to_string(),
                    ),
                }?;
                self.take()?;
                Ok(Token::CloseSquareParen)
            }
            '{' => {
                self.take()?;
                self.stack.push(Closeable::OpenSquigglyParen);
                Ok(Token::Closeable(Closeable::OpenSquigglyParen))
            }
            '}' => {
                match self.stack.pop() {
                    Some(Closeable::OpenSquareParen) => (),
                    Some(p) => HestiaErr::Syntax(
                        self.line,
                        self.col,
                        // TODO: implement Display for Closeable
                        format!("tried to close with }} but dangling open {}", p),
                    ),
                    None => HestiaErr::Syntax(
                        self.line,
                        self.col,
                        "tried to close with }} but no open parens".to_string(),
                    ),
                }?;
                self.take()?;
                Ok(Token::CloseSquigglyParen)
            }
            'f' => {
                self.take()?;
                Ok(Token::Func)
            }
            ':' => {
                // self.take()?;
                // self.state = LexerState::SymbolLiteral;
                self.take_symbol()
                // self.state
            }
            '"' => {
                self.take()?;
            }
            c => {
                self.check_space_char(c)?;
                self.take()?;
                // TODO: return error
                self.step()
            }
        }
    }

    fn peek(&self) -> Result<&char, HestiaErr> {
        match self.chars.get(self.position) {
            Some(c) => Ok(c),
            None => Err(HestiaErr::Internal(
                "attempting to step when lexer is done".to_string(),
            )),
        }
    }

    fn is_done(&self) -> bool {
        // TODO: when done, check if there are dangling parens in stack
        self.position >= self.tokens.len()
    }

    fn take(&mut self) -> Result<&char, HestiaErr> {
        match self.chars.get(self.position) {
            Some(c) => {
                self.position += 1;
                Ok(c)
            }
            None => HestiaErr::Internal("attempting to step when lexer is done".to_string()),
        }
    }
}

// fn tokenize(raw: String) -> Result<Vec<AnnotatedToken, HestiaErr>> {
//     let lexer = Lexer::new(raw.chars().collect());
// }
//
// fn to_token(raw_token: String) -> Result<Token, HestiaErr> {
//     match raw_token.as_str() {
//         "(" => Ok(Token::OpenParen),
//         ")" => Ok(Token::CloseParen),
//         "[" => Ok(Token::OpenSquareParen),
//         "]" => Ok(Token::CloseSquareParen),
//         "{" => Ok(Token::OpenSquigglyParen),
//         "}" => Ok(Token::CloseSquigglyParen),
//         "f" => Ok(Token::Func),
//         "def" => Ok(Token::Def),
//         "\"" => Ok(Token::Quote),
//         "true" => Ok(Token::True),
//         "false" => Ok(Token::False),
//         _ => to_primitive_token(raw_token),
//     }
// }
//
// // fn tokenize(expr: String) -> Vec<Token> {
// //     expr.split_whitespace().map(|x| x.to_string()).collect()
// // }
// //
// // #[test]
// // fn test_tokenize() {
// //     let cases = vec![
// //         ("(+ 1 2)", vec!["(", "+", "1", "2", ")"]),
// //         ("  (  + 1 (2   )", vec!["(", "+", "1", "(", "2", ")"]),
// //         (
// //             "(+  +1 (2 2+   +)   ",
// //             vec!["(", "+", "+1", "(", "2", "2+", "+", ")"],
// //         ),
// //     ];
// //     for case in cases {
// //         let got = clean(case.0.to_string()).map(|x| tokenize(x));
// //         let expected = case.1.into_iter().map(|x| x.to_string()).collect();
// //         assert_eq!(Ok(expected), got);
// //     }
// // }
//
// fn clean(expr: String) -> Result<String, Error> {
//     let r1 = Regex::new(r"\s*\(\s*")?;
//     let r2 = Regex::new(r"\s*\)\s*")?;
//     let r3 = Regex::new(r"\s\s+")?;
//     let result1 = r1.replace_all(&expr.trim(), " ( ");
//     let result2 = r2.replace_all(&result1, " ) ");
//     let result3 = r3.replace_all(&result2, " ");
//     Ok(result3.to_string())
// }
//
// #[test]
// fn test_clean() {
//     assert_eq!(Ok(" ( ) ".to_string()), clean(" (   ) ".to_string()));
//     assert_eq!(
//         Ok(" ( + 1 2 ) ".to_string()),
//         clean("( + 1    2   )".to_string())
//     );
//     assert_eq!(
//         Ok(" ( + ) ".to_string()),
//         clean("     (   +  )      ".to_string())
//     );
//     assert_eq!(Ok("".to_string()), clean("  ".to_string()));
//     assert_eq!(Ok("".to_string()), clean(" ".to_string()));
//     assert_eq!(
//         Ok(" ( + ( + 1 2 ) ) ".to_string()),
//         clean("(+(+ 1 2))".to_string())
//     );
// }
