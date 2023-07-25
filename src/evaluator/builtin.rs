use crate::error::HestiaErr;
use crate::evaluator::{Base, Env};
use std::collections::HashMap;

type F = fn(Vec<Base>) -> Result<Base, HestiaErr>;

#[derive(Clone)]
pub struct Func {
    name: String,
    min_args: Option<usize>,
    max_args: Option<usize>,
    f: F,
}

pub fn builtins() -> Env {
    let mut builtins: Env = HashMap::new();
    let funcs = vec![Func {
        name: "+".to_string(),
        min_args: Some(2),
        max_args: None,
        f: |args: Vec<Base>| -> Result<Base, HestiaErr> {
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
        },
    }];
    for func in funcs.into_iter() {
        builtins.insert(func.name.clone(), Base::BuiltIn(func));
    }
    builtins
}

impl Func {
    pub fn call(&self, args: Vec<Base>) -> Result<Base, HestiaErr> {
        match self.min_args {
            Some(min) => {
                if args.len() < min {
                    return Err(HestiaErr::Runtime(format!(
                        "built-in function `{}` expects at least {} arguments, got {}",
                        self.name,
                        min,
                        args.len()
                    )));
                }

                // let len = args.len();
                // if len < min {
                // let mut env = HashMap::new();
                //     let mut deque = VecDeque::from(args);
                //     for i in (0..len) {
                //         let popped = deque.pop_front();
                //         env.insert(format!("
                //
                //     }
                //     Ok(Base::Func(None,
                // }
            }
            None => {}
        }
        match self.max_args {
            Some(max) => {
                if args.len() > max {
                    return Err(HestiaErr::Runtime(format!(
                        "built-in function `{}` expects no more than {} arguments, got {}",
                        self.name,
                        max,
                        args.len()
                    )));
                }
            }
            None => {}
        }
        (self.f)(args)
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

pub fn add(args: Vec<Base>) -> Result<Base, HestiaErr> {
    // arity_check(args.len(), Some(2), None, "+")?;
    // let should_curry = arity_check(args.len(), Some(2), None, "+")?;
    // if should_curry {
    //     let mut env = HashMap::new();
    //     let mut args = Vec::new();
    //     for (i, arg) in args.iter().enumerate() {
    //         let name = format!("_curry-{}", i);
    //         args.push(name);
    //         env.insert(name, arg);
    //     }
    //     return Ok(Base::Func(env, args, Expr::Func(
    // }
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
