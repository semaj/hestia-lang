use crate::error::HestiaErr;
use crate::lexer::{AnnotatedToken, Closeable, Lexer, Token};
use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum Hashable {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
}

impl fmt::Display for Hashable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Hashable::Integer(n) => write!(f, "{}", n),
            Hashable::Boolean(b) => write!(f, "{}", b),
            Hashable::Str(s) => write!(f, "\"{}\"", s),
            Hashable::Symbol(s) => write!(f, "'{}", s),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Map<T: PartialEq> {
    pub map: HashMap<Hashable, T>,
}

impl<T: PartialEq> PartialEq<Map<T>> for Map<T> {
    fn eq(&self, other: &Map<T>) -> bool {
        if self.map.len() != other.map.len() {
            false
        } else {
            for (k, v1) in self.map.iter() {
                match other.map.get(k) {
                    Some(v2) => {
                        if v1 != v2 {
                            return false;
                        }
                    }
                    None => {
                        return false;
                    }
                }
            }
            true
        }
    }
}

// TODO: add line/column to Expr
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Hashable(Hashable),
    Float(f64),
    List(Vec<Expr>),
    Map(Map<Expr>),
    And(Vec<Expr>),
    Or(Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Func(Vec<String>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Let(Vec<(String, Expr)>, Box<Expr>),
    Def(String, Box<Expr>),
    Identifier(String),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Hashable(h) => write!(f, "{}", h),
            Expr::Map(hm) => {
                write!(f, "{{ ")?;
                for (k, v) in hm.map.iter() {
                    write!(f, "{}: {} ", k, v)?;
                }
                write!(f, "}}")
            }
            Expr::Float(n) => write!(f, "{}", n),
            Expr::List(v) => {
                let args: Vec<String> = v.iter().map(|x| format!("{}", x)).collect();
                write!(f, "[{}]", args.join(" "))
            }
            Expr::And(v) => {
                let args: Vec<String> = v.iter().map(|x| format!("{}", x)).collect();
                write!(f, "(and {})", args.join(" "))
            }
            Expr::Or(v) => {
                let args: Vec<String> = v.iter().map(|x| format!("{}", x)).collect();
                write!(f, "(or {})", args.join(" "))
            }
            Expr::If(a, b, c) => write!(f, "(if {} {} {})", a, b, c),
            Expr::Func(v, body) => write!(f, "{{|{}| {} }}", v.join(" "), body),
            Expr::Call(called, v) => {
                let args: Vec<String> = v.iter().map(|x| format!("{}", x)).collect();
                write!(f, "({} {})", called, args.join(" "))
            }
            Expr::Let(v, body) => {
                let bindings: Vec<String> =
                    v.iter().map(|(x, y)| format!("[{} {}]", x, y)).collect();
                write!(f, "(let ({}) {})", bindings.join(""), body)
            }
            Expr::Def(s, body) => write!(f, "(def {} {})", s, body),
            Expr::Identifier(s) => write!(f, "{}", s),
        }
    }
}

struct Parser {
    lexer: Lexer,
    tokens: Vec<AnnotatedToken>,
    cursor: usize,
}

pub fn parse(raw: String) -> Result<Expr, HestiaErr> {
    let lexer = Lexer::new(raw.chars().collect());
    let mut parser = Parser::new(lexer);
    let parsed = parser.parse()?;
    if parser.is_done() {
        Ok(parsed)
    } else {
        Err(HestiaErr::Runtime("dangling tokens".to_string()))
    }
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
        if self.tokens.is_empty() || self.cursor > self.tokens.len() - 1 {
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
        self.forward_expect(Token::CloseParen, "let")?;
        Ok(Expr::Let(bindings, Box::new(body)))
    }

    fn parse_args(&mut self) -> Result<Vec<Expr>, HestiaErr> {
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
        Ok(arguments)
    }

    pub fn is_done(&self) -> bool {
        self.lexer.is_done()
    }

    // Assumes we just parsed an open paren
    fn parse_complex(&mut self) -> Result<Expr, HestiaErr> {
        let next = self.forward()?;
        match next.token {
            Token::Identifier(s) => match s.as_str() {
                "if" => {
                    let result = Ok(Expr::If(
                        Box::new(self.parse()?),
                        Box::new(self.parse()?),
                        Box::new(self.parse()?),
                    ));
                    self.forward_expect(Token::CloseParen, "if")?;
                    result
                }
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
                    self.forward_expect(Token::CloseParen, "def")?;
                    Ok(Expr::Def(s, Box::new(body)))
                }
                "let" => self.parse_let(),
                identifier => {
                    let arguments = self.parse_args()?;
                    match identifier {
                        "and" => Ok(Expr::And(arguments)),
                        "or" => Ok(Expr::Or(arguments)),
                        other => Ok(Expr::Call(
                            Box::new(Expr::Identifier(other.to_string())),
                            arguments,
                        )),
                    }
                }
            },
            Token::Closeable(Closeable::OpenParen) => {
                self.back()?;
                let func = self.parse()?;
                let arguments = self.parse_args()?;
                Ok(Expr::Call(Box::new(func), arguments))
            }
            Token::Closeable(Closeable::OpenSquigglyParen) => {
                let func = self.parse_function()?;
                let arguments = self.parse_args()?;
                Ok(Expr::Call(Box::new(func), arguments))
            }
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

    fn parse_function_or_map(&mut self) -> Result<Expr, HestiaErr> {
        let mut next = self.forward()?;
        match next.token {
            Token::Closeable(Closeable::Pipe) => {
                self.back()?;
                self.parse_function()
            }
            _ => {
                let mut kvs = HashMap::new();
                loop {
                    match next.token {
                        Token::CloseSquigglyParen => return Ok(Expr::Map(Map { map: kvs })),
                        _ => {
                            self.back()?;
                            let key = self.parse()?;
                            self.forward_expect(Token::Colon, "map")?;
                            let value = self.parse()?;
                            match key {
                                Expr::Hashable(h) => {
                                    kvs.insert(h, value);
                                }
                                Expr::Identifier(i) => {
                                    kvs.insert(Hashable::Symbol(i), value);
                                }
                                _ => {
                                    return Err(HestiaErr::Runtime(format!(
                                        "map keys must be Hashable, {} is not",
                                        key
                                    )))
                                }
                            }
                        }
                    }
                    next = self.forward()?;
                }
            }
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
        self.forward_expect(Token::CloseSquigglyParen, "function")?;
        Ok(Expr::Func(arguments, Box::new(body)))
    }

    pub fn parse(&mut self) -> Result<Expr, HestiaErr> {
        let next = self.forward()?;
        match next.token {
            Token::Boolean(b) => Ok(Expr::Hashable(Hashable::Boolean(b))),
            Token::Integer(n) => Ok(Expr::Hashable(Hashable::Integer(n))),
            Token::Str(s) => Ok(Expr::Hashable(Hashable::Str(s))),
            Token::Symbol(s) => Ok(Expr::Hashable(Hashable::Symbol(s))),
            Token::Float(n) => Ok(Expr::Float(n)),
            Token::Identifier(s) => Ok(Expr::Identifier(s)),
            Token::Closeable(Closeable::OpenSquigglyParen) => self.parse_function_or_map(),
            Token::Closeable(Closeable::OpenParen) => self.parse_complex(),
            Token::Closeable(Closeable::OpenSquareParen) => {
                let mut elements = Vec::new();
                loop {
                    let next = self.forward()?;
                    match next.token {
                        Token::CloseSquareParen => break,
                        _ => {
                            self.back()?;
                            elements.push(self.parse()?);
                        }
                    }
                }
                Ok(Expr::List(elements))
            }
            _ => Err(HestiaErr::Syntax(
                next.line,
                next.col_start,
                format!("unexpected {} at top-level parse", next.token),
            )),
        }
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
            ("true", Expr::Hashable(Hashable::Boolean(true))),
            ("false", Expr::Hashable(Hashable::Boolean(false))),
            ("1.12", Expr::Float(1.12)),
            (
                "\"1.12\"",
                Expr::Hashable(Hashable::Str("1.12".to_string())),
            ),
            (
                "\"hello\nworld\"",
                Expr::Hashable(Hashable::Str("hello\nworld".to_string())),
            ),
            (
                "(def x 12)",
                Expr::Def(
                    "x".to_string(),
                    Box::new(Expr::Hashable(Hashable::Integer(12))),
                ),
            ),
            (
                "(add 1 -2)",
                Expr::Call(
                    Box::new(Expr::Identifier("add".to_string())),
                    vec![
                        Expr::Hashable(Hashable::Integer(1)),
                        Expr::Hashable(Hashable::Integer(-2)),
                    ],
                ),
            ),
            (
                "(de x \"a\" 1.12 true)",
                Expr::Call(
                    Box::new(Expr::Identifier("de".to_string())),
                    vec![
                        Expr::Identifier("x".to_string()),
                        Expr::Hashable(Hashable::Str("a".to_string())),
                        Expr::Float(1.12),
                        Expr::Hashable(Hashable::Boolean(true)),
                    ],
                ),
            ),
            (
                "(let ([x 12] [y true] [z y]) x)",
                Expr::Let(
                    vec![
                        ("x".to_string(), Expr::Hashable(Hashable::Integer(12))),
                        ("y".to_string(), Expr::Hashable(Hashable::Boolean(true))),
                        ("z".to_string(), Expr::Identifier("y".to_string())),
                    ],
                    Box::new(Expr::Identifier("x".to_string())),
                ),
            ),
            (
                "{|x y z| (add x y z) }",
                Expr::Func(
                    vec!["x".to_string(), "y".to_string(), "z".to_string()],
                    Box::new(Expr::Call(
                        Box::new(Expr::Identifier("add".to_string())),
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
                    Box::new(Expr::Hashable(Hashable::Boolean(true))),
                    Box::new(Expr::Hashable(Hashable::Str("help".to_string()))),
                    Box::new(Expr::Hashable(Hashable::Str("nope".to_string()))),
                ),
            ),
            (
                "(and true false false)",
                Expr::And(vec![
                    Expr::Hashable(Hashable::Boolean(true)),
                    Expr::Hashable(Hashable::Boolean(false)),
                    Expr::Hashable(Hashable::Boolean(false)),
                ]),
            ),
            (
                "(or true false false)",
                Expr::Or(vec![
                    Expr::Hashable(Hashable::Boolean(true)),
                    Expr::Hashable(Hashable::Boolean(false)),
                    Expr::Hashable(Hashable::Boolean(false)),
                ]),
            ),
        ];
        for case in cases {
            let got = parse(case.0.to_string());
            assert_eq!(Ok(case.1), got, "{}", case.0);
        }
    }
}
