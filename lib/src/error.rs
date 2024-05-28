use std::fmt;

#[derive(Debug, PartialEq)]
pub enum HestiaErr {
    Syntax(usize, usize, String),
    Runtime(String),
    User(String),
    Internal(String),
    Regex(String),
}

impl fmt::Display for HestiaErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HestiaErr::Syntax(l, c, s) => {
                write!(f, "syntax error at line {}, col {}: {}", l, c, s)
            }
            HestiaErr::Runtime(s) => {
                write!(f, "runtime error: {}", s)
            }
            HestiaErr::Internal(s) => {
                write!(f, "{}", s)
            }
            HestiaErr::User(s) => {
                write!(f, "user error: {}", s)
            }
            HestiaErr::Regex(s) => {
                write!(f, "regex error: {}", s)
            }
        }
    }
}
