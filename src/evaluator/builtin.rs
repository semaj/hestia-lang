use crate::error::HestiaErr;
use crate::evaluator::{get_arg, Base, Env};
use crate::parser::Hashable;
use std::collections::{HashMap, VecDeque};

type F = fn(&str, Vec<Base>) -> Result<Base, HestiaErr>;

#[derive(Clone)]
pub struct Func {
    pub name: String,
    curried_args: Vec<Base>,
    min_args: Option<usize>,
    max_args: Option<usize>,
    f: F,
}

impl PartialEq<Func> for Func {
    fn eq(&self, other: &Func) -> bool {
        self.name == other.name
    }
}

fn usize_to_integer(u: usize) -> Result<Base, HestiaErr> {
    match u.try_into() {
        Ok(i) => Ok(Base::Hashable(Hashable::Integer(i))),
        Err(e) => Err(HestiaErr::Runtime(format!(
            "failed to convert {} into i64: {}",
            u, e
        ))),
    }
}

pub fn builtins() -> Env {
    let mut builtins: Env = HashMap::new();
    let funcs = vec![
        Func {
            name: "add".to_string(),
            curried_args: Vec::new(),
            min_args: Some(2),
            max_args: None,
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let mut int_sum: i64 = 0;
                for (i, arg) in args.iter().enumerate() {
                    match arg {
                        Base::Hashable(Hashable::Integer(n)) => {
                            int_sum += n;
                        }
                        _ => {
                            return Err(HestiaErr::Runtime(format!(
                            "function `{}` expects arguments of type Integer, argument {} is {}",
                            name,
                            i,
                            arg.to_type()
                        )))
                        }
                    }
                }
                Ok(Base::Hashable(Hashable::Integer(int_sum)))
            },
        },
        Func {
            name: "fadd".to_string(),
            curried_args: Vec::new(),
            min_args: Some(2),
            max_args: None,
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let mut float_sum: f64 = 0.0;
                for (i, arg) in args.iter().enumerate() {
                    match arg {
                        Base::Float(n) => {
                            float_sum += n;
                        }
                        _ => {
                            return Err(HestiaErr::Runtime(format!(
                                "function `{}` expects arguments of type Float, argument {} is {}",
                                name,
                                i,
                                arg.to_type()
                            )))
                        }
                    }
                }
                Ok(Base::Float(float_sum))
            },
        },
        Func {
            name: "sub".to_string(),
            curried_args: Vec::new(),
            min_args: Some(2),
            max_args: None,
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let mut int_sum: i64 = 0;
                for (i, arg) in args.iter().enumerate() {
                    match arg {
                        Base::Hashable(Hashable::Integer(n)) => {
                            int_sum -= n;
                        }
                        _ => {
                            return Err(HestiaErr::Runtime(format!(
                                "function `{}` expects Integer arguments, argument {} is {}",
                                name,
                                i,
                                arg.to_type()
                            )))
                        }
                    }
                }
                Ok(Base::Hashable(Hashable::Integer(int_sum)))
            },
        },
        Func {
            name: "fsub".to_string(),
            curried_args: Vec::new(),
            min_args: Some(2),
            max_args: None,
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let mut float_sum: f64 = 0.0;
                for (i, arg) in args.iter().enumerate() {
                    match arg {
                        Base::Float(n) => {
                            float_sum -= n;
                        }
                        _ => {
                            return Err(HestiaErr::Runtime(format!(
                                "function `{}` expects Float arguments, argument {} is {}",
                                name,
                                i,
                                arg.to_type()
                            )))
                        }
                    }
                }
                Ok(Base::Float(float_sum))
            },
        },
        Func {
            name: "mul".to_string(),
            curried_args: Vec::new(),
            min_args: Some(2),
            max_args: None,
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let mut int_product: i64 = 1;
                for (i, arg) in args.iter().enumerate() {
                    match arg {
                        Base::Hashable(Hashable::Integer(n)) => {
                            int_product *= n;
                        }
                        _ => {
                            return Err(HestiaErr::Runtime(format!(
                            "function `{}` expects arguments of type Integer, argument {} is {}",
                            name,
                            i,
                            arg.to_type()
                        )))
                        }
                    }
                }
                Ok(Base::Hashable(Hashable::Integer(int_product)))
            },
        },
        Func {
            name: "fmul".to_string(),
            curried_args: Vec::new(),
            min_args: Some(2),
            max_args: None,
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let mut float_product: f64 = 1.0;
                for (i, arg) in args.iter().enumerate() {
                    match arg {
                        Base::Float(n) => {
                            float_product *= n;
                        }
                        _ => {
                            return Err(HestiaErr::Runtime(format!(
                                "function `{}` expects arguments of type Float , argument {} is {}",
                                name,
                                i,
                                arg.to_type()
                            )))
                        }
                    }
                }
                Ok(Base::Float(float_product))
            },
        },
        Func {
            name: "div".to_string(),
            curried_args: Vec::new(),
            min_args: Some(2),
            max_args: Some(2),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let first = match get_arg(name, 0, &args)? {
                    Base::Hashable(Hashable::Integer(i)) => Ok(i),
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects first argument to be Integer, got {}",
                        name,
                        x.to_type()
                    ))),
                }?;
                let second = match get_arg(name, 1, &args)? {
                    Base::Hashable(Hashable::Integer(i)) => Ok(i),
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects second argument to be Integer, got {}",
                        name,
                        x.to_type()
                    ))),
                }?;
                if second == 0 {
                    return Err(HestiaErr::Runtime("attempted divide by 0".to_string()));
                }
                Ok(Base::Hashable(Hashable::Integer(first / second)))
            },
        },
        Func {
            name: "fdiv".to_string(),
            curried_args: Vec::new(),
            min_args: Some(2),
            max_args: Some(2),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let first = match get_arg(name, 0, &args)? {
                    Base::Float(f) => Ok(f),
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects first argument to be Float, got {}",
                        name,
                        x.to_type()
                    ))),
                }?;
                let second = match get_arg(name, 1, &args)? {
                    Base::Float(f) => Ok(f),
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects second argument to be Float, got {}",
                        name,
                        x.to_type()
                    ))),
                }?;
                if second == 0.0 {
                    return Err(HestiaErr::Runtime("attempted divide by 0.0".to_string()));
                }
                Ok(Base::Float(first / second))
            },
        },
        Func {
            name: "mod".to_string(),
            curried_args: Vec::new(),
            min_args: Some(2),
            max_args: Some(2),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let first = match get_arg(name, 0, &args)? {
                    Base::Hashable(Hashable::Integer(i)) => Ok(i),
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects first argument to be Integer, got {}",
                        name,
                        x.to_type()
                    ))),
                }?;
                let second = match get_arg(name, 1, &args)? {
                    Base::Hashable(Hashable::Integer(i)) => Ok(i),
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects second argument to be Integer, got {}",
                        name,
                        x.to_type()
                    ))),
                }?;
                Ok(Base::Hashable(Hashable::Integer(first % second)))
            },
        },
        Func {
            name: "log".to_string(),
            curried_args: Vec::new(),
            min_args: Some(2),
            max_args: Some(2),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let first = match get_arg(name, 0, &args)? {
                    Base::Hashable(Hashable::Integer(i)) => Ok(i),
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects first argument to be Integer, got {}",
                        name,
                        x.to_type()
                    ))),
                }?;
                let second = match get_arg(name, 1, &args)? {
                    Base::Hashable(Hashable::Integer(i)) => Ok(i),
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects second argument to be Integer, got {}",
                        name,
                        x.to_type()
                    ))),
                }?;
                match first.checked_ilog(second) {
                    Some(x) => Ok(Base::Hashable(Hashable::Integer(x.into()))),
                    None => Err(HestiaErr::Runtime(
                        "invalid logarithm arguments".to_string(),
                    )),
                }
            },
        },
        Func {
            name: "type".to_string(),
            curried_args: Vec::new(),
            min_args: Some(1),
            max_args: Some(1),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                Ok(Base::Type(get_arg(name, 0, &args)?.to_type()))
            },
        },
        Func {
            name: "eq2?".to_string(),
            curried_args: Vec::new(),
            min_args: Some(2),
            max_args: Some(2),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let first = get_arg(name, 0, &args)?;
                let second = get_arg(name, 1, &args)?;
                Ok(Base::Hashable(Hashable::Boolean(first == second)))
            },
        },
        Func {
            name: "error".to_string(),
            curried_args: Vec::new(),
            min_args: Some(1),
            max_args: Some(1),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let first = get_arg(name, 0, &args)?;
                Err(HestiaErr::User(format!("{}", first)))
            },
        },
        Func {
            name: "some".to_string(),
            curried_args: Vec::new(),
            min_args: Some(1),
            max_args: Some(1),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let first = get_arg(name, 0, &args)?;
                Ok(Base::Opt(Some(Box::new(first))))
            },
        },
        Func {
            name: "some?".to_string(),
            curried_args: Vec::new(),
            min_args: Some(1),
            max_args: Some(1),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let first = get_arg(name, 0, &args)?;
                match first {
                    Base::Opt(None) => Ok(Base::Hashable(Hashable::Boolean(false))),
                    Base::Opt(Some(_)) => Ok(Base::Hashable(Hashable::Boolean(true))),
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects argument of type Option, got {}",
                        name,
                        x.to_type()
                    ))),
                }
            },
        },
        Func {
            name: "none".to_string(),
            curried_args: Vec::new(),
            min_args: Some(0),
            max_args: Some(0),
            f: |_: &str, _: Vec<Base>| -> Result<Base, HestiaErr> { Ok(Base::Opt(None)) },
        },
        Func {
            name: "keys".to_string(),
            curried_args: Vec::new(),
            min_args: Some(1),
            max_args: Some(1),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                match get_arg(name, 0, &args)? {
                    Base::Map(m) => {
                        let mut result: VecDeque<Base> = VecDeque::new();
                        for (k, _) in m.map.iter() {
                            result.push_back(Base::Hashable(k.clone()))
                        }
                        Ok(Base::List(result))
                    }
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects argument of type Map, got {}",
                        name,
                        x.to_type(),
                    ))),
                }
            },
        },
        Func {
            name: "values".to_string(),
            curried_args: Vec::new(),
            min_args: Some(1),
            max_args: Some(1),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                match get_arg(name, 0, &args)? {
                    Base::Map(m) => {
                        let mut result: VecDeque<Base> = VecDeque::new();
                        for (_, v) in m.map.iter() {
                            result.push_back(v.clone())
                        }
                        Ok(Base::List(result))
                    }
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects argument of type Map, got {}",
                        name,
                        x.to_type(),
                    ))),
                }
            },
        },
        Func {
            name: "cons".to_string(),
            curried_args: Vec::new(),
            min_args: Some(2),
            max_args: Some(2),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let first = get_arg(name, 0, &args)?;
                match get_arg(name, 1, &args)? {
                    Base::List(v) => {
                        let mut new = v.clone();
                        new.push_front(first);
                        Ok(Base::List(new))
                    }
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects second argument of type List, got {}",
                        name,
                        x.to_type(),
                    ))),
                }
            },
        },
        Func {
            name: "first".to_string(),
            curried_args: Vec::new(),
            min_args: Some(1),
            max_args: Some(1),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                match get_arg(name, 0, &args)? {
                    Base::List(v) => match v.get(0) {
                        Some(x) => Ok(Base::Opt(Some(Box::new(x.clone())))),
                        None => Ok(Base::Opt(None)),
                    },
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects argument of type List, got {}",
                        name,
                        x.to_type(),
                    ))),
                }
            },
        },
        Func {
            name: "rest".to_string(),
            curried_args: Vec::new(),
            min_args: Some(1),
            max_args: Some(1),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                match get_arg(name, 0, &args)? {
                    Base::List(v) => match v.get(0) {
                        Some(_) => {
                            let mut new = v.clone();
                            new.pop_front();
                            Ok(Base::List(new))
                        }
                        None => Ok(Base::List(VecDeque::new())),
                    },
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects argument of type List, got {}",
                        name,
                        x.to_type(),
                    ))),
                }
            },
        },
        Func {
            name: "unwrap".to_string(),
            curried_args: Vec::new(),
            min_args: Some(2),
            max_args: Some(2),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let second = get_arg(name, 1, &args)?;
                match get_arg(name, 0, &args)? {
                    Base::Opt(v) => match v {
                        Some(b) => Ok(*b),
                        None => Err(HestiaErr::User(format!("{} failed: {}", name, second))),
                    },
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects first argument of type Option, got {}",
                        name,
                        x.to_type(),
                    ))),
                }
            },
        },
        Func {
            name: "at".to_string(),
            curried_args: Vec::new(),
            min_args: Some(2),
            max_args: Some(2),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let first = match get_arg(name, 0, &args)? {
                    Base::Hashable(Hashable::Integer(i)) => Ok(i),
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects first argument of type Integer, got {}",
                        name,
                        x.to_type()
                    ))),
                }?;
                let first_conv: usize = match usize::try_from(first) {
                    Ok(i) => Ok(i),
                    Err(_) => Err(HestiaErr::Internal(format!(
                        "function `{}` received invalid index {}",
                        name, first
                    ))),
                }?;
                match get_arg(name, 1, &args)? {
                    Base::List(v) => match v.get(first_conv) {
                        Some(b) => Ok(Base::Opt(Some(Box::new(b.clone())))),
                        None => Ok(Base::Opt(None)),
                    },
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects second argument of type List, got {}",
                        name,
                        x.to_type(),
                    ))),
                }
            },
        },
        Func {
            name: "get".to_string(),
            curried_args: Vec::new(),
            min_args: Some(2),
            max_args: Some(2),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let first = match get_arg(name, 0, &args)? {
                    Base::Hashable(h) => Ok(h),
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects first argument that is Hashable, got {}",
                        name,
                        x.to_type()
                    ))),
                }?;
                match get_arg(name, 1, &args)? {
                    Base::Map(m) => match m.map.get(&first) {
                        Some(b) => Ok(Base::Opt(Some(Box::new(b.clone())))),
                        None => Ok(Base::Opt(None)),
                    },
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects second argument of type Map, got {}",
                        name,
                        x.to_type(),
                    ))),
                }
            },
        },
        Func {
            name: "len".to_string(),
            curried_args: Vec::new(),
            min_args: Some(1),
            max_args: Some(1),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                match get_arg(name, 0, &args)? {
                    Base::List(l) => Ok(usize_to_integer(l.len())?),
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects first argument of type List, got {}",
                        name,
                        x.to_type()
                    ))),
                }
            },
        },
        Func {
            name: "push".to_string(),
            curried_args: Vec::new(),
            min_args: Some(2),
            max_args: Some(2),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let first = get_arg(name, 0, &args)?;
                match get_arg(name, 1, &args)? {
                    Base::List(v) => {
                        let mut new = v.clone();
                        new.push_back(first);
                        Ok(Base::List(new))
                    }
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects second argument of type List, got {}",
                        name,
                        x.to_type(),
                    ))),
                }
            },
        },
        Func {
            name: "insert".to_string(),
            curried_args: Vec::new(),
            min_args: Some(3),
            max_args: Some(3),
            f: |name: &str, args: Vec<Base>| -> Result<Base, HestiaErr> {
                let first = match get_arg(name, 0, &args)? {
                    Base::Hashable(h) => Ok(h),
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects first argument that is Hashable, got {}",
                        name,
                        x.to_type()
                    ))),
                }?;
                let second = get_arg(name, 1, &args)?;
                match get_arg(name, 2, &args)? {
                    Base::Map(m) => {
                        let mut new = m.clone();
                        new.map.insert(first, second);
                        Ok(Base::Map(new))
                    }
                    x => Err(HestiaErr::Runtime(format!(
                        "function `{}` expects second argument of type List, got {}",
                        name,
                        x.to_type(),
                    ))),
                }
            },
        },
    ];
    for func in funcs.into_iter() {
        builtins.insert(func.name.clone(), Base::BuiltIn(func));
    }
    builtins
}

impl Func {
    pub fn call(&self, mut args: Vec<Base>) -> Result<Base, HestiaErr> {
        let mut new_args = self.curried_args.clone();
        new_args.append(&mut args);
        if let Some(min) = self.min_args {
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
        if let Some(max) = self.max_args {
            if new_args.len() > max {
                return Err(HestiaErr::Runtime(format!(
                    "built-in function `{}` expects no more than {} arguments, got {}",
                    self.name,
                    max,
                    new_args.len()
                )));
            }
        }
        (self.f)(&self.name, new_args)
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
