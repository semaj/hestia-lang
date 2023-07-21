use crate::error::HestiaErr;
use crate::parser::{parse, Expr};
use std::collections::HashMap;
use std::fmt;

pub enum Base {
    Number(f64),
    Boolean(bool),
    Str(String),
    Func(HashMap<String, Base>, Vec<String>, Expr),
}

// TODO: define type function that converts base to human-readable type
impl fmt::Display for Base {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Base::Number(n) => write!(f, "{}", n),
            Base::Boolean(b) => write!(f, "{}", b),
            Base::Str(s) => write!(f, "\"{}\"", s),
            Base::Func(_, _, _) => write!(f, "<function>"), // TODO
        }
    }
}

pub fn evaluate(raw: String) -> Result<Base, HestiaErr> {
    let parsed = parse(raw)?;
    let evaluator = Evaluator::new();
    evaluator.eval(parsed)
}

type Func = fn(Vec<Base>) -> Result<Base, HestiaErr>;

struct Evaluator {
    builtins: HashMap<String, Func>,
}

impl Evaluator {
    pub fn new() -> Self {
        let mut builtins: HashMap<String, Func> = HashMap::new();
        builtins.insert("+".to_string(), add);
        Self { builtins }
    }

    pub fn eval(&self, expr: Expr) -> Result<Base, HestiaErr> {
        match expr {
            Expr::Number(n) => Ok(Base::Number(n)),
            Expr::Boolean(b) => Ok(Base::Boolean(b)),
            Expr::Str(s) => Ok(Base::Str(s)),
            Expr::And(v) => self.eval_and(v),
            Expr::Or(v) => self.eval_or(v),
            Expr::If(c, i, e) => self.eval_if(c, i, e),
            Expr::Func(_, _) => todo!(),
            Expr::Call(_, _) => todo!(),
            Expr::Let(_, _) => todo!(),
            Expr::Def(_, _) => todo!(),
            Expr::Identifier(_) => todo!(),
            // Expr::Func(v, b) => self.eval_func(v, b),
            // Expr::Call(s, v) => self.eval_call(s, v),
            // Expr::Let(v, b) => self.eval_let(v, b),
            // Expr::Def(s, b) => self.eval_def(s, b),
            // Expr::Identifier(s) => self.eval_identifier(s),
        }
    }

    pub fn eval_and(&self, exprs: Vec<Expr>) -> Result<Base, HestiaErr> {
        arity_check(exprs.len(), Some(1), None, "and")?;
        for (i, expr) in exprs.into_iter().enumerate() {
            let evaluated = self.eval(expr)?;
            match evaluated {
                Base::Boolean(b) => {
                    if !b {
                        return Ok(Base::Boolean(false));
                    }
                }
                _ => {
                    return Err(HestiaErr::Runtime(format!(
                        "`and` expects arguments of type Boolean, argument {} is {}",
                        i, evaluated
                    )));
                }
            }
        }
        Ok(Base::Boolean(true))
    }

    pub fn eval_or(&self, exprs: Vec<Expr>) -> Result<Base, HestiaErr> {
        arity_check(exprs.len(), Some(1), None, "or")?;
        for (i, expr) in exprs.into_iter().enumerate() {
            let evaluated = self.eval(expr)?;
            match evaluated {
                Base::Boolean(b) => {
                    if b {
                        return Ok(Base::Boolean(true));
                    }
                }
                _ => {
                    return Err(HestiaErr::Runtime(format!(
                        "`or` expects arguments of type Boolean, argument {} is {}",
                        i, evaluated
                    )));
                }
            }
        }
        Ok(Base::Boolean(false))
    }

    pub fn eval_if(
        &self,
        condition: Box<Expr>,
        then: Box<Expr>,
        els: Box<Expr>,
    ) -> Result<Base, HestiaErr> {
        let evaluated_condition = self.eval(*condition)?;
        match evaluated_condition {
            Base::Boolean(b) => {
                if b {
                    self.eval(*then)
                } else {
                    self.eval(*els)
                }
            }
            _ => {
                return Err(HestiaErr::Runtime(format!(
                    "`if` expects a condition of type Boolean, got {}",
                    evaluated_condition
                )));
            }
        }
    }
}

fn check_min(size: usize, min: usize, name: &str) -> Result<(), HestiaErr> {
    if size < min {
        return Err(HestiaErr::Runtime(format!(
            "built-in function `{}` expects at least {} arguments, got {}",
            name, min, size
        )));
    }
    Ok(())
}

fn check_max(size: usize, max: usize, name: &str) -> Result<(), HestiaErr> {
    if size < max {
        return Err(HestiaErr::Runtime(format!(
            "built-in function `{}` expects no more than {} arguments, got {}",
            name, max, size
        )));
    }
    Ok(())
}

fn check_exactly(size: usize, expected: usize, name: &str) -> Result<(), HestiaErr> {
    if size != expected {
        return Err(HestiaErr::Runtime(format!(
            "built-in function `{}` expects exactly {} arguments, got {}",
            name, expected, size
        )));
    }
    Ok(())
}

fn arity_check(
    size: usize,
    min: Option<usize>,
    max: Option<usize>,
    name: &str,
) -> Result<(), HestiaErr> {
    match (min, max) {
        (Some(mi), Some(ma)) => {
            if mi == ma {
                check_exactly(size, mi, name)
            } else {
                check_min(size, mi, name)?;
                check_max(size, ma, name)
            }
        }
        (Some(mi), None) => check_min(size, mi, name),
        (None, Some(ma)) => check_max(size, ma, name),
        (None, None) => Err(HestiaErr::Internal(format!(
            "arity check in `{}` uses two Nones",
            name
        ))),
    }
}

fn add(args: Vec<Base>) -> Result<Base, HestiaErr> {
    arity_check(args.len(), Some(2), None, "+")?;
    let mut sum = 0.0;
    for (i, arg) in args.iter().enumerate() {
        match arg {
            Base::Number(n) => sum += n,
            _ => {
                return Err(HestiaErr::Runtime(format!(
                    "function `+` expects arguments of type Number, argument {} is {}",
                    i, arg
                )))
            }
        }
    }
    Ok(Base::Number(sum))
}
