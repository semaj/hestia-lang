use std::fmt;

#[derive(Debug, PartialEq)]
pub enum HestiaErr {
    Syntax(usize, usize, String),
    Internal(String),
}

impl fmt::Display for HestiaErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HestiaErr::Syntax(l, c, s) => {
                write!(f, "syntax error at line {}, col {}: {}", l, c, s)
            }
            HestiaErr::Internal(s) => {
                write!(f, "{}", s)
            }
        }
    }
}
