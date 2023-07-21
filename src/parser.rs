use crate::error::HestiaErr;
use crate::lexer::{AnnotatedToken, Closeable, Lexer, Token};

#[derive(Debug, PartialEq)]
pub enum Expr {
    // Chain,
    Number(f64),
    Boolean(bool),
    Str(String),
    And(Vec<Expr>),
    Or(Vec<Expr>),
    Xor(Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Func(Vec<String>, Box<Expr>),
    Call(String, Vec<Expr>),
    Let(Vec<(String, Expr)>, Box<Expr>),
    Def(String, Box<Expr>),
    Identifier(String),
}

// I want symbols and a hash literal

struct Parser {
    lexer: Lexer,
    tokens: Vec<AnnotatedToken>,
    cursor: usize,
}

pub fn parse(raw: String) -> Result<Expr, HestiaErr> {
    let lexer = Lexer::new(raw.chars().collect());
    Parser::new(lexer).parse()
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self {
            lexer,
            tokens: Vec::new(),
            cursor: 0,
        }
    }

    pub fn peek(&mut self) -> Result<AnnotatedToken, HestiaErr> {
        // if self.tokens.is_empty() {
        //     self.tokens.push(self.lexer.step()?);
        // }
        match self.tokens.get(self.cursor) {
            Some(token) => Ok(token.clone()),
            None => Err(HestiaErr::Internal(
                "tokens should be non-empty".to_string(),
            )),
        }
    }

    pub fn forward_expect(
        &mut self,
        token: Token,
        context: &str,
    ) -> Result<AnnotatedToken, HestiaErr> {
        let t = self.forward()?;
        if t.token != token {
            return Err(HestiaErr::Syntax(
                t.line,
                t.col_start,
                format!("in {} expecting {}, got {}", context, token, t.token),
            ));
        }
        Ok(t)
    }

    pub fn forward(&mut self) -> Result<AnnotatedToken, HestiaErr> {
        if self.tokens.len() == 0 || self.cursor > self.tokens.len() - 1 {
            self.tokens.push(self.lexer.step()?);
        }
        let result = self.peek()?;
        self.cursor += 1;
        Ok(result)
    }

    pub fn back(&mut self) -> Result<AnnotatedToken, HestiaErr> {
        if self.cursor == 0 {
            return Err(HestiaErr::Internal(
                "attempting to step parser back with cursor 0".to_string(),
            ));
        }
        self.cursor -= 1;
        self.peek()
    }

    fn parse_let(&mut self) -> Result<Expr, HestiaErr> {
        self.forward_expect(Token::Closeable(Closeable::OpenParen), "let")?;
        let mut bindings = Vec::new();
        loop {
            let next = self.forward()?;
            match next.token {
                Token::CloseParen => break,
                Token::Closeable(Closeable::OpenSquareParen) => {
                    let maybe_identifier = self.forward()?;
                    let identifier = match maybe_identifier.token {
                        Token::Identifier(s) => Ok(s),
                        _ => Err(HestiaErr::Syntax(
                            maybe_identifier.line,
                            maybe_identifier.col_start,
                            format!(
                                "expected identifier in let binding, got {}",
                                maybe_identifier.token
                            ),
                        )),
                    }?;
                    let bound_expr = self.parse()?;
                    bindings.push((identifier, bound_expr));
                    self.forward_expect(Token::CloseSquareParen, "let")?;
                }
                _ => {
                    return Err(HestiaErr::Syntax(
                        next.line,
                        next.col_start,
                        format!("unexpected {} in let", next.token),
                    ))
                }
            }
        }
        let body = self.parse()?;
        Ok(Expr::Let(bindings, Box::new(body)))
    }

    // Assumes we just parsed an open paren
    pub fn parse_complex(&mut self) -> Result<Expr, HestiaErr> {
        let next = self.forward()?;
        match next.token {
            Token::Identifier(s) => match s.as_str() {
                "if" => Ok(Expr::If(
                    Box::new(self.parse()?),
                    Box::new(self.parse()?),
                    Box::new(self.parse()?),
                )),
                "def" => {
                    let next = self.forward()?;
                    let s = match next.token {
                        Token::Identifier(s) => Ok(s),
                        _ => Err(HestiaErr::Syntax(
                            next.line,
                            next.col_start,
                            format!("expected identifier in def, got `{}`", next.token),
                        )),
                    }?;
                    let body = self.parse()?;
                    Ok(Expr::Def(s, Box::new(body)))
                }
                "let" => self.parse_let(),
                identifier => {
                    let mut arguments = Vec::new();
                    loop {
                        let next = self.forward()?;
                        match next.token {
                            Token::CloseParen => break,
                            _ => {
                                self.back()?;
                                arguments.push(self.parse()?);
                            }
                        }
                    }
                    match identifier {
                        "and" => Ok(Expr::And(arguments)),
                        "or" => Ok(Expr::Or(arguments)),
                        "xor" => Ok(Expr::Xor(arguments)),
                        other => Ok(Expr::Call(other.to_string(), arguments)),
                    }
                }
            },
            _ => Err(HestiaErr::Syntax(
                next.line,
                next.col_start,
                format!(
                    "unexpected `{}` after `{}`",
                    next.token,
                    Token::Closeable(Closeable::OpenParen)
                ),
            )),
        }
    }

    fn parse_function(&mut self) -> Result<Expr, HestiaErr> {
        self.forward_expect(Token::Closeable(Closeable::Pipe), "function")?;
        let mut arguments = Vec::new();
        loop {
            let next = self.forward()?;
            match next.token {
                Token::Closeable(Closeable::Pipe) => break,
                Token::Identifier(s) => arguments.push(s),
                _ => {
                    return Err(HestiaErr::Syntax(
                        next.line,
                        next.col_start,
                        format!(
                            "expected identifier or closing pipe in function arguments, got {}",
                            next.token
                        ),
                    ))
                }
            }
        }
        let body = self.parse()?;
        Ok(Expr::Func(arguments, Box::new(body)))
    }

    pub fn parse(&mut self) -> Result<Expr, HestiaErr> {
        let next = self.forward()?;
        return match next.token {
            Token::Boolean(b) => Ok(Expr::Boolean(b)),
            Token::Number(n) => Ok(Expr::Number(n)),
            Token::Str(s) => Ok(Expr::Str(s)),
            Token::Identifier(s) => Ok(Expr::Identifier(s)),
            Token::Closeable(Closeable::OpenSquigglyParen) => self.parse_function(),
            Token::Closeable(Closeable::OpenParen) => self.parse_complex(),
            _ => {
                return Err(HestiaErr::Syntax(
                    next.line,
                    next.col_start,
                    format!("unexpected {} at top-level parse", next.token),
                ))
            }
        };
    }
}

#[cfg(test)]
mod test {
    use crate::parser::*;
    use pretty_assertions::assert_eq;
    #[test]
    fn test_parse() {
        let cases = vec![
            ("help", Expr::Identifier("help".to_string())),
            ("true", Expr::Boolean(true)),
            ("false", Expr::Boolean(false)),
            ("1.12", Expr::Number(1.12)),
            ("\"1.12\"", Expr::Str("1.12".to_string())),
            ("\"hello\nworld\"", Expr::Str("hello\nworld".to_string())),
            (
                "(def x 12)",
                Expr::Def("x".to_string(), Box::new(Expr::Number(12.0))),
            ),
            (
                "(+ 1 -2)",
                Expr::Call("+".to_string(), vec![Expr::Number(1.0), Expr::Number(-2.0)]),
            ),
            (
                "(de x \"a\" 1.12 true)",
                Expr::Call(
                    "de".to_string(),
                    vec![
                        Expr::Identifier("x".to_string()),
                        Expr::Str("a".to_string()),
                        Expr::Number(1.12),
                        Expr::Boolean(true),
                    ],
                ),
            ),
            (
                "(let ([x 12] [y true] [z y]) x)",
                Expr::Let(
                    vec![
                        ("x".to_string(), Expr::Number(12.0)),
                        ("y".to_string(), Expr::Boolean(true)),
                        ("z".to_string(), Expr::Identifier("y".to_string())),
                    ],
                    Box::new(Expr::Identifier("x".to_string())),
                ),
            ),
            (
                "{|x y z| (+ x y z) }",
                Expr::Func(
                    vec!["x".to_string(), "y".to_string(), "z".to_string()],
                    Box::new(Expr::Call(
                        "+".to_string(),
                        vec![
                            Expr::Identifier("x".to_string()),
                            Expr::Identifier("y".to_string()),
                            Expr::Identifier("z".to_string()),
                        ],
                    )),
                ),
            ),
            (
                "(if true \"help\" \"nope\")",
                Expr::If(
                    Box::new(Expr::Boolean(true)),
                    Box::new(Expr::Str("help".to_string())),
                    Box::new(Expr::Str("nope".to_string())),
                ),
            ),
            (
                "(and true false false)",
                Expr::And(vec![
                    Expr::Boolean(true),
                    Expr::Boolean(false),
                    Expr::Boolean(false),
                ]),
            ),
            (
                "(or true false false)",
                Expr::Or(vec![
                    Expr::Boolean(true),
                    Expr::Boolean(false),
                    Expr::Boolean(false),
                ]),
            ),
            (
                "(xor true false false)",
                Expr::Xor(vec![
                    Expr::Boolean(true),
                    Expr::Boolean(false),
                    Expr::Boolean(false),
                ]),
            ),
        ];
        for case in cases {
            let got = parse(case.0.to_string()).unwrap();
            assert_eq!(case.1, got);
        }
    }
}
