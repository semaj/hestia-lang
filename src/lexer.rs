// use regex::{Error, Regex};

use crate::error::HestiaErr;

#[derive(Debug, PartialEq)]
enum Closeable {
    OpenParen,
    OpenSquareParen,
    OpenSquigglyParen,
}

#[derive(Debug, PartialEq)]
enum Token {
    CloseParen,
    CloseSquareParen,
    CloseSquigglyParen,
    Chain,
    Hash, // Comment
    Identifier(String),
    Comment(String),
    Number(f64),
    Str(String),
    Closeable(Closeable),
}

#[derive(Debug, PartialEq)]
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
    NumericLiteral,
    NumericLiteralAfterDot,
    Comment,
}

static NUMBER_CHARS: [&'static char; 10] =
    [&'1', &'2', &'3', &'4', &'5', &'6', &'7', &'8', &'9', &'0'];

static IDENTIFIER_CHARS: [&'static char; 71] = [
    &'a', &'b', &'c', &'d', &'e', &'f', &'g', &'h', &'i', &'j', &'k', &'l', &'m', &'n', &'o', &'p',
    &'q', &'r', &'s', &'t', &'u', &'v', &'w', &'x', &'y', &'z', &'A', &'B', &'C', &'D', &'E', &'F',
    &'G', &'H', &'I', &'J', &'K', &'L', &'M', &'N', &'O', &'P', &'Q', &'R', &'S', &'T', &'U', &'V',
    &'W', &'X', &'Y', &'Z', &'1', &'2', &'3', &'4', &'5', &'6', &'7', &'8', &'9', &'0', &'-', &'!',
    &'?', &'&', &'|', &'`', &'<', &'>', &'+',
];

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

    fn is_space_char(&self, c: &char) -> Result<bool, HestiaErr> {
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
            ' ' => Ok(true),
            '\n' => Ok(true),
            _ => Ok(false),
        }
    }

    fn take_identifier(&mut self) -> Result<AnnotatedToken, HestiaErr> {
        let mut token: Vec<char> = Vec::new();
        let line = self.line;
        let col_start = self.col;
        let mut c = self.peek()?;
        if !IDENTIFIER_CHARS.contains(&c) || c == &'-' {
            return Err(HestiaErr::Syntax(
                self.line,
                self.col,
                format!("invalid first character {} in identifier", c),
            ));
        }
        loop {
            c = self.peek()?;
            if !IDENTIFIER_CHARS.contains(&c) {
                return Ok(AnnotatedToken {
                    token: Token::Identifier(token.iter().collect()),
                    line,
                    col_start,
                    col_end: self.col - 1,
                });
            }
            token.push(c.clone());
            self.take()?;
        }
    }

    fn take_str(&mut self) -> Result<AnnotatedToken, HestiaErr> {
        let mut token: Vec<char> = Vec::new();
        let line = self.line;
        let col_start = self.col;
        self.take()?;
        loop {
            match self.peek()? {
                '"' => {
                    let r = AnnotatedToken {
                        token: Token::Str(token.iter().collect()),
                        line,
                        col_start,
                        col_end: self.col,
                    };
                    self.take()?;
                    return Ok(r);
                }
                c => {
                    token.push(c.clone());
                }
            }
            self.take()?;
        }
    }

    fn take_number(&mut self) -> Result<AnnotatedToken, HestiaErr> {
        let mut token: Vec<char> = Vec::new();
        let line = self.line;
        let col_start = self.col;
        let mut c = self.peek()?;
        if !NUMBER_CHARS.contains(&c) && c != &'-' {
            return Err(HestiaErr::Syntax(
                line,
                col_start + 1,
                format!("invalid character {} in number", c),
            ));
        }
        let mut seen_dot = false;
        let mut first = true;
        loop {
            c = self.peek()?;
            match c {
                '-' => {
                    if !first {
                        return Err(HestiaErr::Syntax(
                            self.line,
                            self.col,
                            "minus sign in non-first position of number".to_string(),
                        ));
                    }
                }
                '.' => {
                    if seen_dot {
                        return Err(HestiaErr::Syntax(
                            self.line,
                            self.col,
                            "second dot in number".to_string(),
                        ));
                    }
                    seen_dot = true;
                }
                c => {
                    if !NUMBER_CHARS.contains(&c) {
                        return match token.iter().collect::<String>().parse::<f64>() {
                            Ok(num) => Ok(AnnotatedToken {
                                token: Token::Number(num),
                                line,
                                col_start,
                                col_end: self.col - 1,
                            }),
                            Err(_) => Err(HestiaErr::Syntax(
                                line,
                                col_start,
                                "failed to parse number".to_string(),
                            )),
                        };
                    }
                }
            }
            first = false;
            token.push(c.clone());
            self.take()?;
        }
    }

    fn take_comment(&mut self) -> Result<AnnotatedToken, HestiaErr> {
        let mut token: Vec<char> = Vec::new();
        let line = self.line;
        let col_start = self.col;
        self.take()?;
        loop {
            let c = self.peek()?;
            if self.is_space_char(c)? {
                if c == &'\n' {
                    return Ok(AnnotatedToken {
                        token: Token::Comment(token.iter().collect()),
                        line,
                        col_start,
                        col_end: self.col - 1,
                    });
                }
            }
            token.push(c.clone());
            self.take()?;
        }
    }

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
                self.step()
            }
            '(' => {
                // self.stack.push(Closeable::OpenParen);
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
                // match self.stack.pop() {
                //     Some(Closeable::OpenParen) => Ok(()),
                //     Some(p) => Err(HestiaErr::Syntax(
                //         self.line,
                //         self.col,
                //         // TODO: implement Display for Closeable
                //         format!("tried to close with ) but dangling open {}", p),
                //     )),
                //     None => Err(HestiaErr::Syntax(
                //         self.line,
                //         self.col,
                //         "tried to close with ) but no open parens".to_string(),
                //     )),
                // }?;
                let r = AnnotatedToken {
                    token: Token::CloseParen,
                    line: self.line,
                    col_start: self.col,
                    col_end: self.col,
                };
                self.take()?;
                Ok(r)
            }
            '[' => {
                // self.stack.push(Closeable::OpenSquareParen);
                let r = AnnotatedToken {
                    token: Token::Closeable(Closeable::OpenSquareParen),
                    line: self.line,
                    col_start: self.col,
                    col_end: self.col,
                };
                self.take()?;
                Ok(r)
            }
            ']' => {
                // match self.stack.pop() {
                //     Some(Closeable::OpenSquareParen) => Ok(()),
                //     Some(p) => Err(HestiaErr::Syntax(
                //         self.line,
                //         self.col,
                //         // TODO: implement Display for Closeable
                //         format!("tried to close with ] but dangling open {}", p),
                //     )),
                //     None => Err(HestiaErr::Syntax(
                //         self.line,
                //         self.col,
                //         "tried to close with ] but no open parens".to_string(),
                //     )),
                // }?;
                let r = AnnotatedToken {
                    token: Token::CloseSquareParen,
                    line: self.line,
                    col_start: self.col,
                    col_end: self.col,
                };
                self.take()?;
                Ok(r)
            }
            '{' => {
                let r = AnnotatedToken {
                    token: Token::Closeable(Closeable::OpenSquigglyParen),
                    line: self.line,
                    col_start: self.col,
                    col_end: self.col,
                };
                self.take()?;
                // self.stack.push(Closeable::OpenSquigglyParen);
                Ok(r)
            }
            '}' => {
                // match self.stack.pop() {
                //     Some(Closeable::OpenSquareParen) => Ok(()),
                //     Some(p) => Err(HestiaErr::Syntax(
                //         self.line,
                //         self.col,
                //         // TODO: implement Display for Closeable
                //         format!("tried to close with }} but dangling open {}", p),
                //     )),
                //     None => Err(HestiaErr::Syntax(
                //         self.line,
                //         self.col,
                //         "tried to close with }} but no open parens".to_string(),
                //     )),
                // }?;
                let r = AnnotatedToken {
                    token: Token::CloseSquigglyParen,
                    line: self.line,
                    col_start: self.col,
                    col_end: self.col,
                };
                self.take()?;
                Ok(r)
            }
            '"' => self.take_str(),
            ':' => {
                let r = AnnotatedToken {
                    token: Token::Chain,
                    line: self.line,
                    col_start: self.col,
                    col_end: self.col + 1,
                };
                self.take()?;
                match self.peek()? {
                    '>' => {
                        self.take()?;
                        Ok(r)
                    }
                    c => Err(HestiaErr::Syntax(
                        self.line,
                        self.col,
                        format!("expected chain operator :>, got :{}", c),
                    )),
                }
            }
            '#' => self.take_comment(),
            c => {
                self.is_space_char(c)?;
                if NUMBER_CHARS.contains(&c) || c == &'-' {
                    self.take_number()
                } else {
                    self.take_identifier()
                }
            }
        }
    }

    fn peek(&self) -> Result<&char, HestiaErr> {
        match self.chars.get(self.position) {
            Some(c) => Ok(c),
            None => Err(HestiaErr::Syntax(
                self.line,
                self.col,
                "unexpected end-of-file".to_string(),
            )),
        }
    }

    fn is_done(&self) -> bool {
        // TODO: when done, check if there are dangling parens in stack?
        self.position >= self.chars.len()
    }

    fn take(&mut self) -> Result<&char, HestiaErr> {
        match self.chars.get(self.position) {
            Some(c) => {
                self.position += 1;
                self.col += 1;
                Ok(c)
            }
            None => Err(HestiaErr::Syntax(
                self.line,
                self.col,
                "unexpected end-of-file".to_string(),
            )),
        }
    }
}

fn tokenize(raw: String) -> Result<Vec<AnnotatedToken>, HestiaErr> {
    let mut lexer = Lexer::new(raw.chars().collect());
    let mut tokens = Vec::new();
    while !lexer.is_done() {
        let token = lexer.step()?;
        tokens.push(token);
    }
    Ok(tokens)
}
#[test]
fn test_tokenize() {
    let cases = vec![(
        "(+ 1 -2)",
        vec![
            AnnotatedToken {
                token: Token::Closeable(Closeable::OpenParen),
                line: 0,
                col_start: 0,
                col_end: 0,
            },
            AnnotatedToken {
                token: Token::Identifier("+".to_string()),
                line: 0,
                col_start: 1,
                col_end: 1,
            },
            AnnotatedToken {
                token: Token::Number(1.0),
                line: 0,
                col_start: 3,
                col_end: 3,
            },
            AnnotatedToken {
                token: Token::Number(-2.0),
                line: 0,
                col_start: 5,
                col_end: 6,
            },
            AnnotatedToken {
                token: Token::CloseParen,
                line: 0,
                col_start: 7,
                col_end: 7,
            },
        ],
    )];
    for case in cases {
        let expected: Result<Vec<AnnotatedToken>, HestiaErr> = Ok(case.1);
        let got = tokenize(case.0.to_string());
        assert_eq!(expected, got);
    }
}
