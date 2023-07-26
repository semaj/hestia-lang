use crate::error::HestiaErr;
use crate::evaluator::{Base, Env};
use crate::parser::Hashable;
use std::collections::HashMap;

type F = fn(Vec<Base>) -> Result<Base, HestiaErr>;

#[derive(Clone)]
pub struct Func {
    pub name: String,
    curried_args: Vec<Base>,
    min_args: Option<usize>,
    max_args: Option<usize>,
    f: F,
}

impl PartialEq<Func> for Func{
    fn eq(&self, other: &Func) -> bool {
        self.name == other.name
    }
}

pub fn builtins() -> Env {
    let mut builtins: Env = HashMap::new();
    let funcs = vec![Func {
        name: "+".to_string(),
        curried_args: Vec::new(),
        min_args: Some(2),
        max_args: None,
        f: |args: Vec<Base>| -> Result<Base, HestiaErr> {
            let mut float_sum: f64 = 0.0;
            let mut int_sum: i64 = 0;
            let mut float_used = false;
            let mut int_used = false;
            for (i, arg) in args.iter().enumerate() {
                match arg {
                    Base::Hashable(Hashable::Integer(n)) => {
                        int_used = true;
                        int_sum += n;
                        if float_used {
                            return Err(HestiaErr::Runtime(format!(
                                        "function `+` received Float arguments up until Integer argument {} ({})",
                                        i, arg
                                        )))
                        }
                    }
                    Base::Float(n) => {
                        float_used = true;
                        float_sum += n;
                        if int_used {
                            return Err(HestiaErr::Runtime(format!(
                                        "function `+` received Integer arguments up until Float argument {} ({})",
                                        i, arg
                                        )))
                        }
                    }
                    _ => {
                        return Err(HestiaErr::Runtime(format!(
                                    "function `+` expects arguments of type Float OR Integer, argument {} is {}",
                                    i, arg
                                    )))
                    }
                }
            }
            if float_used {
                Ok(Base::Float(float_sum))
            } else {
                Ok(Base::Hashable(Hashable::Integer(int_sum)))
            }
        },
    }];
    for func in funcs.into_iter() {
        builtins.insert(func.name.clone(), Base::BuiltIn(func));
    }
    builtins
}

impl Func {
    pub fn call(&self, mut args: Vec<Base>) -> Result<Base, HestiaErr> {
        let mut new_args = self.curried_args.clone();
        new_args.append(&mut args);
        match self.min_args {
            Some(min) => {
                let len = new_args.len();
                if len < min {
                    return Ok(Base::BuiltIn(Func {
                        name: format!("curried_{}", self.name),
                        curried_args: new_args,
                        min_args: self.min_args,
                        max_args: self.max_args,
                        f: self.f,
                    }));
                }
            }
            None => {}
        }
        match self.max_args {
            Some(max) => {
                if new_args.len() > max {
                    return Err(HestiaErr::Runtime(format!(
                        "built-in function `{}` expects no more than {} arguments, got {}",
                        self.name,
                        max,
                        new_args.len()
                    )));
                }
            }
            None => {}
        }
        (self.f)(new_args)
    }
}

fn check_min(size: usize, min: usize, name: &str) -> Result<(), HestiaErr> {
    if size < min {
        return Err(HestiaErr::Runtime(format!(
            "built-in `{}` expects at least {} arguments, got {}",
            name, min, size
        )));
    }
    Ok(())
}

fn check_max(size: usize, max: usize, name: &str) -> Result<(), HestiaErr> {
    if size > max {
        return Err(HestiaErr::Runtime(format!(
            "built-in `{}` expects no more than {} arguments, got {}",
            name, max, size
        )));
    }
    Ok(())
}

fn check_exactly(size: usize, expected: usize, name: &str) -> Result<(), HestiaErr> {
    if size != expected {
        return Err(HestiaErr::Runtime(format!(
            "built-in `{}` expects exactly {} arguments, got {}",
            name, expected, size
        )));
    }
    Ok(())
}

pub fn arity_check(
    size: usize,
    min: Option<usize>,
    max: Option<usize>,
    name: &str,
) -> Result<(), HestiaErr> {
    match (min, max) {
        (Some(min), Some(max)) => {
            if min == max {
                check_exactly(size, min, name)
            } else {
                check_min(size, min, name)?;
                check_max(size, max, name)
            }
        }
        (Some(min), None) => check_min(size, min, name),
        (None, Some(max)) => check_max(size, max, name),
        (None, None) => Err(HestiaErr::Internal(format!(
            "arity check in `{}` uses two Nones",
            name
        ))),
    }
}
