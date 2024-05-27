use crate::error::HestiaErr;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Closeable {
    OpenParen,
    OpenSquareParen,
    OpenSquigglyParen,
    Pipe,
}

impl Closeable {
    pub fn closer(&self) -> Token {
        match self {
            Closeable::OpenParen => Token::CloseParen,
            Closeable::OpenSquareParen => Token::CloseSquareParen,
            Closeable::OpenSquigglyParen => Token::CloseSquigglyParen,
            Closeable::Pipe => Token::Closeable(Closeable::Pipe),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    CloseParen,
    CloseSquareParen,
    CloseSquigglyParen,
    Colon,
    Identifier(String),
    Boolean(bool),
    Comment(String),
    Integer(i64),
    Float(f64),
    Str(String),
    Symbol(String),
    Closeable(Closeable),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::CloseParen => write!(f, ")"),
            Token::CloseSquareParen => write!(f, "]"),
            Token::CloseSquigglyParen => write!(f, "}}"),
            Token::Colon => write!(f, ":"),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::Boolean(s) => write!(f, "{}", s),
            Token::Comment(s) => write!(f, "#{}", s),
            Token::Integer(n) => write!(f, "{}", n),
            Token::Float(n) => write!(f, "{}", n),
            Token::Str(s) => write!(f, "\"{}\"", s),
            Token::Symbol(s) => write!(f, "'{}", s),
            Token::Closeable(Closeable::OpenParen) => write!(f, "("),
            Token::Closeable(Closeable::OpenSquareParen) => write!(f, "["),
            Token::Closeable(Closeable::OpenSquigglyParen) => write!(f, "{{"),
            Token::Closeable(Closeable::Pipe) => write!(f, "|"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AnnotatedToken {
    pub token: Token,
    pub line: usize,
    pub col_start: usize,
    pub col_end: usize,
}

pub struct Lexer {
    position: usize,
    line: usize,
    col: usize,
    chars: Vec<char>,
}

static NUMBER_CHARS: [&char; 10] = [&'1', &'2', &'3', &'4', &'5', &'6', &'7', &'8', &'9', &'0'];

static IDENTIFIER_CHARS: [&char; 67] = [
    &'a', &'b', &'c', &'d', &'e', &'f', &'g', &'h', &'i', &'j', &'k', &'l', &'m', &'n', &'o', &'p',
    &'q', &'r', &'s', &'t', &'u', &'v', &'w', &'x', &'y', &'z', &'A', &'B', &'C', &'D', &'E', &'F',
    &'G', &'H', &'I', &'J', &'K', &'L', &'M', &'N', &'O', &'P', &'Q', &'R', &'S', &'T', &'U', &'V',
    &'W', &'X', &'Y', &'Z', &'1', &'2', &'3', &'4', &'5', &'6', &'7', &'8', &'9', &'0', &'-', &'!',
    &'?', &'<', &'>',
];

impl Lexer {
    pub fn new(chars: Vec<char>) -> Self {
        Self {
            position: 0,
            line: 0,
            col: 0,
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
            ',' => Ok(true),
            ' ' => Ok(true),
            '\n' => Ok(true),
            _ => Ok(false),
        }
    }

    fn take_identifier_or_boolean(&mut self) -> Result<AnnotatedToken, HestiaErr> {
        let mut token: Vec<char> = Vec::new();
        let line = self.line;
        let col_start = self.col;
        let c = self.peek_char()?;
        if !IDENTIFIER_CHARS.contains(&c) || c == &'-' {
            return Err(HestiaErr::Syntax(
                self.line,
                self.col,
                format!("invalid first character {} in identifier", c),
            ));
        }
        loop {
            if self.is_done() || !IDENTIFIER_CHARS.contains(&self.peek_char()?) {
                let complete = token.iter().collect();
                let token = if complete == "true" {
                    Token::Boolean(true)
                } else if complete == "false" {
                    Token::Boolean(false)
                } else {
                    Token::Identifier(complete)
                };
                return Ok(AnnotatedToken {
                    token,
                    line,
                    col_start,
                    col_end: self.col - 1,
                });
            }
            token.push(*self.take()?);
        }
    }

    fn take_str(&mut self) -> Result<AnnotatedToken, HestiaErr> {
        let mut token: Vec<char> = Vec::new();
        let line = self.line;
        let col_start = self.col;
        self.take()?;
        loop {
            match self.peek_char()? {
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
                    token.push(*c);
                }
            }
            self.take()?;
        }
    }

    fn take_symbol(&mut self) -> Result<AnnotatedToken, HestiaErr> {
        let mut token: Vec<char> = Vec::new();
        let line = self.line;
        let col_start = self.col;
        self.take()?; // Take the quote
        if !IDENTIFIER_CHARS.contains(&self.peek_char()?) {
            return Err(HestiaErr::Syntax(
                line,
                col_start,
                "invalid empty symbol".to_string(),
            ));
        }
        loop {
            if self.is_done() || !IDENTIFIER_CHARS.contains(&self.peek_char()?) {
                let complete = token.iter().collect();
                let token = Token::Symbol(complete);
                return Ok(AnnotatedToken {
                    token,
                    line,
                    col_start,
                    col_end: self.col - 1,
                });
            }
            token.push(*self.take()?);
        }
    }

    fn take_number(&mut self) -> Result<AnnotatedToken, HestiaErr> {
        let mut token: Vec<char> = Vec::new();
        let line = self.line;
        let col_start = self.col;
        let mut c = self.peek_char()?;
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
            if self.is_done() {
                if seen_dot {
                    return match token.iter().collect::<String>().parse::<f64>() {
                        Ok(num) => Ok(AnnotatedToken {
                            token: Token::Float(num),
                            line,
                            col_start,
                            col_end: self.col - 1,
                        }),
                        Err(_) => Err(HestiaErr::Syntax(
                            line,
                            col_start,
                            "failed to parse float".to_string(),
                        )),
                    };
                }
                return match token.iter().collect::<String>().parse::<i64>() {
                    Ok(num) => Ok(AnnotatedToken {
                        token: Token::Integer(num),
                        line,
                        col_start,
                        col_end: self.col - 1,
                    }),
                    Err(_) => Err(HestiaErr::Syntax(
                        line,
                        col_start,
                        "failed to parse integer".to_string(),
                    )),
                };
            }
            c = self.peek_char()?;
            match c {
                ',' => {
                    self.take()?;
                    continue;
                }
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
                        if seen_dot {
                            return match token.iter().collect::<String>().parse::<f64>() {
                                Ok(num) => Ok(AnnotatedToken {
                                    token: Token::Float(num),
                                    line,
                                    col_start,
                                    col_end: self.col - 1,
                                }),
                                Err(_) => Err(HestiaErr::Syntax(
                                    line,
                                    col_start,
                                    "failed to parse float".to_string(),
                                )),
                            };
                        }
                        return match token.iter().collect::<String>().parse::<i64>() {
                            Ok(num) => Ok(AnnotatedToken {
                                token: Token::Integer(num),
                                line,
                                col_start,
                                col_end: self.col - 1,
                            }),
                            Err(_) => Err(HestiaErr::Syntax(
                                line,
                                col_start,
                                "failed to parse integer".to_string(),
                            )),
                        };
                    }
                }
            }
            first = false;
            token.push(*self.take()?);
        }
    }

    fn take_comment(&mut self) -> Result<AnnotatedToken, HestiaErr> {
        let mut token: Vec<char> = Vec::new();
        let line = self.line;
        let col_start = self.col;
        self.take()?;
        loop {
            if self.is_done() {
                return Ok(AnnotatedToken {
                    token: Token::Comment(token.iter().collect()),
                    line,
                    col_start,
                    col_end: self.col - 1,
                });
            }
            let c = self.peek_char()?;
            if self.is_space_char(c)? && c == &'\n' {
                return Ok(AnnotatedToken {
                    token: Token::Comment(token.iter().collect()),
                    line,
                    col_start,
                    col_end: self.col - 1,
                });
            }
            token.push(*self.take()?);
        }
    }

    // Clear whitespace and comments
    pub fn clean_up(&mut self) -> Result<(), HestiaErr> {
        if let Ok(x) = self.peek_char() {
            match x {
                '\n' => {
                    self.take()?;
                    self.line += 1;
                    self.col = 0;
                    self.clean_up()
                }
                ' ' => {
                    self.take()?;
                    self.clean_up()
                }
                ',' => {
                    self.take()?;
                    self.clean_up()
                }
                '#' => {
                    self.take_comment()?;
                    self.clean_up()
                }
                _ => Ok(()),
            }
        } else {
            Ok(())
        }
    }

    // Step normal
    pub fn step(&mut self) -> Result<AnnotatedToken, HestiaErr> {
        match self.peek_char()? {
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
            ',' => {
                self.take()?;
                self.step()
            }
            '(' => {
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
                Ok(r)
            }
            '}' => {
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
            '\'' => self.take_symbol(),
            '|' => {
                let r = AnnotatedToken {
                    token: Token::Closeable(Closeable::Pipe),
                    line: self.line,
                    col_start: self.col,
                    col_end: self.col,
                };
                self.take()?;
                Ok(r)
            }
            ':' => {
                let r = AnnotatedToken {
                    token: Token::Colon,
                    line: self.line,
                    col_start: self.col,
                    col_end: self.col,
                };
                self.take()?;
                Ok(r)
            }
            '#' => {
                self.take_comment()?;
                self.step()
            }
            c => {
                self.is_space_char(c)?;
                if NUMBER_CHARS.contains(&c) || c == &'-' {
                    self.take_number()
                } else {
                    self.take_identifier_or_boolean()
                }
            }
        }
    }

    fn peek_char(&self) -> Result<&char, HestiaErr> {
        match self.chars.get(self.position) {
            Some(c) => Ok(c),
            None => Err(HestiaErr::Syntax(
                self.line,
                self.col,
                "peeking, unexpected end-of-file".to_string(),
            )),
        }
    }

    pub fn is_done(&self) -> bool {
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
                "taking, unexpected end-of-file".to_string(),
            )),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::*;
    use pretty_assertions::assert_eq;
    fn tokenize_all(raw: String) -> Result<Vec<AnnotatedToken>, HestiaErr> {
        let mut lexer = Lexer::new(raw.chars().collect());
        let mut tokens = Vec::new();
        while !lexer.is_done() {
            let token = lexer.step()?;
            lexer.clean_up()?;
            tokens.push(token);
        }
        Ok(tokens)
    }

    #[test]
    fn test_tokenize() {
        let cases = vec![
            (
                "help",
                vec![AnnotatedToken {
                    token: Token::Identifier("help".to_string()),
                    line: 0,
                    col_start: 0,
                    col_end: 3,
                }]
            ),
            (
                "1.12",
                vec![AnnotatedToken {
                    token: Token::Float(1.12),
                    line: 0,
                    col_start: 0,
                    col_end: 3,
                }]
            ),
            (
                "1.12 # 1.13",
                vec![AnnotatedToken {
                    token: Token::Float(1.12),
                    line: 0,
                    col_start: 0,
                    col_end: 3,
                }]
            ),
            (
                "(add 1 -2)",
                vec![
                AnnotatedToken {
                    token: Token::Closeable(Closeable::OpenParen),
                    line: 0,
                    col_start: 0,
                    col_end: 0,
                },
                AnnotatedToken {
                    token: Token::Identifier("add".to_string()),
                    line: 0,
                    col_start: 1,
                    col_end: 3,
                },
                AnnotatedToken {
                    token: Token::Integer(1),
                    line: 0,
                    col_start: 5,
                    col_end: 5,
                },
                AnnotatedToken {
                    token: Token::Integer(-2),
                    line: 0,
                    col_start: 7,
                    col_end: 8,
                },
                AnnotatedToken {
                    token: Token::CloseParen,
                    line: 0,
                    col_start: 9,
                    col_end: 9,
                },
                ],
                ),
                (
                    "(let ([y 12]) # comment \n{|x| (predicate-though? -42.8 (concat \"x\" (to-string y) x))})",
                    vec![
                    AnnotatedToken {
                        token: Token::Closeable(Closeable::OpenParen),
                        line: 0,
                        col_start: 0,
                        col_end: 0,
                    },
                    AnnotatedToken {
                        token: Token::Identifier("let".to_string()),
                        line: 0,
                        col_start: 1,
                        col_end: 3,
                    },
                    AnnotatedToken {
                        token: Token::Closeable(Closeable::OpenParen),
                        line: 0,
                        col_start: 5,
                        col_end: 5,
                    },
                    AnnotatedToken {
                        token: Token::Closeable(Closeable::OpenSquareParen),
                        line: 0,
                        col_start: 6,
                        col_end: 6,
                    },
                    AnnotatedToken {
                        token: Token::Identifier("y".to_string()),
                        line: 0,
                        col_start: 7,
                        col_end: 7,
                    },
                    AnnotatedToken {
                        token: Token::Integer(12),
                        line: 0,
                        col_start: 9,
                        col_end: 10,
                    },
                    AnnotatedToken {
                        token: Token::CloseSquareParen,
                        line: 0,
                        col_start: 11,
                        col_end: 11,
                    },
                    AnnotatedToken {
                        token: Token::CloseParen,
                        line: 0,
                        col_start: 12,
                        col_end: 12,
                    },
                    AnnotatedToken {
                        token: Token::Closeable(Closeable::OpenSquigglyParen),
                        line: 1,
                        col_start: 0,
                        col_end: 0,
                    },
                    AnnotatedToken {
                        token: Token::Closeable(Closeable::Pipe),
                        line: 1,
                        col_start: 1,
                        col_end: 1,
                    },
                    AnnotatedToken {
                        token: Token::Identifier("x".to_string()),
                        line: 1,
                        col_start: 2,
                        col_end: 2,
                    },
                    AnnotatedToken {
                        token: Token::Closeable(Closeable::Pipe),
                        line: 1,
                        col_start: 3,
                        col_end: 3,
                    },
                    AnnotatedToken {
                        token: Token::Closeable(Closeable::OpenParen),
                        line: 1,
                        col_start: 5,
                        col_end: 5,
                    },
                    AnnotatedToken {
                        token: Token::Identifier("predicate-though?".to_string()),
                        line: 1,
                        col_start: 6,
                        col_end: 22,
                    },
                    AnnotatedToken {
                        token: Token::Float(-42.8),
                        line: 1,
                        col_start: 24,
                        col_end: 28,
                    },
                    AnnotatedToken {
                        token: Token::Closeable(Closeable::OpenParen),
                        line: 1,
                        col_start: 30,
                        col_end: 30,
                    },
                    AnnotatedToken {
                        token: Token::Identifier("concat".to_string()),
                        line: 1,
                        col_start: 31,
                        col_end: 36,
                    },
                    AnnotatedToken {
                        token: Token::Str("x".to_string()),
                        line: 1,
                        col_start: 38,
                        col_end: 40,
                    },
                    AnnotatedToken {
                        token: Token::Closeable(Closeable::OpenParen),
                        line: 1,
                        col_start: 42,
                        col_end: 42,
                    },
                    AnnotatedToken {
                        token: Token::Identifier("to-string".to_string()),
                        line: 1,
                        col_start: 43,
                        col_end: 51,
                    },
                    AnnotatedToken {
                        token: Token::Identifier("y".to_string()),
                        line: 1,
                        col_start: 53,
                        col_end: 53,
                    },
                    AnnotatedToken {
                        token: Token::CloseParen,
                        line: 1,
                        col_start: 54,
                        col_end: 54,
                    },
                    AnnotatedToken {
                        token: Token::Identifier("x".to_string()),
                        line: 1,
                        col_start: 56,
                        col_end: 56,
                    },
                    AnnotatedToken {
                        token: Token::CloseParen,
                        line: 1,
                        col_start: 57,
                        col_end: 57,
                    },
                    AnnotatedToken {
                        token: Token::CloseParen,
                        line: 1,
                        col_start: 58,
                        col_end: 58,
                    },
                    AnnotatedToken {
                        token: Token::CloseSquigglyParen,
                        line: 1,
                        col_start: 59,
                        col_end: 59,
                    },
                    AnnotatedToken {
                        token: Token::CloseParen,
                        line: 1,
                        col_start: 60,
                        col_end: 60,
                    },
                    ],
                    ),
                    ];
        for case in cases {
            let got = tokenize_all(case.0.to_string());
            assert_eq!(Ok(case.1), got);
        }
    }
}
