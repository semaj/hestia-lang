use crate::error::HestiaErr;
use crate::parser::{parse, Expr};
use std::collections::{HashMap, VecDeque};
use std::fmt;

#[derive(Clone)]
pub enum Base {
    Number(f64),
    Boolean(bool),
    Str(String),
    Func(Option<String>, HashMap<String, Base>, Vec<String>, Expr),
    BuiltIn(Func),
}

// TODO: define type function that converts base to human-readable type
impl fmt::Display for Base {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Base::Number(n) => write!(f, "{}", n),
            Base::Boolean(b) => write!(f, "{}", b),
            Base::Str(s) => write!(f, "\"{}\"", s),
            Base::Func(name, _, args, body) => {
                write!(
                    f,
                    "closure {}: {{ |{}| {} }}",
                    name.clone().unwrap_or("<anonymous>".to_string()),
                    args.join(" "),
                    body
                )
            } // TODO
            Base::BuiltIn(_) => write!(f, "<built-in function>"),
        }
    }
}

pub fn evaluate(raw: String) -> Result<Base, HestiaErr> {
    let parsed = parse(raw)?;
    let mut evaluator = Evaluator::new();
    evaluator.eval(HashMap::new(), parsed)
}

type Func = fn(Vec<Base>) -> Result<Base, HestiaErr>;

type Env = HashMap<String, Base>;

pub struct Evaluator {
    builtins: Env,
    defs: Env,
}

impl Evaluator {
    pub fn new() -> Self {
        let mut builtins: Env = HashMap::new();
        builtins.insert("+".to_string(), Base::BuiltIn(add));
        Self {
            builtins,
            defs: HashMap::new(),
        }
    }

    // Could eval def differently when top and when not
    pub fn eval_top(&mut self, expr: Expr) -> Result<Base, HestiaErr> {
        self.eval(HashMap::new(), expr)
    }

    pub fn eval(&mut self, env: Env, expr: Expr) -> Result<Base, HestiaErr> {
        match expr {
            Expr::Number(n) => Ok(Base::Number(n)),
            Expr::Boolean(b) => Ok(Base::Boolean(b)),
            Expr::Str(s) => Ok(Base::Str(s)),
            Expr::And(v) => self.eval_and(env, v),
            Expr::Or(v) => self.eval_or(env, v),
            Expr::If(c, i, e) => self.eval_if(env, c, i, e),
            Expr::Func(v, b) => self.eval_func(env, v, b),
            Expr::Call(f, v) => self.eval_call(env, f, v),
            Expr::Let(v, b) => self.eval_let(env, v, b),
            Expr::Def(n, e) => self.eval_def(env, n, e),
            Expr::Identifier(i) => self.eval_identifier(env, i),
        }
    }

    fn eval_let(
        &mut self,
        mut env: Env,
        bindings: Vec<(String, Expr)>,
        body: Box<Expr>,
    ) -> Result<Base, HestiaErr> {
        for (name, expr) in bindings.into_iter() {
            let evaluated_expr = self.eval(env.clone(), expr)?;
            env.insert(name, evaluated_expr);
        }
        self.eval(env, *body)
    }

    fn eval_identifier(&self, env: Env, name: String) -> Result<Base, HestiaErr> {
        match env.get(&name) {
            Some(x) => return Ok(x.clone()),
            None => {}
        }
        match self.defs.get(&name) {
            Some(x) => return Ok(x.clone()),
            None => {}
        }
        match self.builtins.get(&name) {
            Some(x) => Ok(x.clone()),
            None => Err(HestiaErr::Runtime(format!(
                "attempting to use unbound identifier `{}`",
                name
            ))),
        }
    }

    fn eval_def(&mut self, env: Env, name: String, expr: Box<Expr>) -> Result<Base, HestiaErr> {
        if self.builtins.contains_key(&name) {
            return Err(HestiaErr::Runtime(format!(
                "identifier `{}` is already bound to a built-in function",
                name
            )));
        }
        match env.get(&name) {
            Some(x) => {
                return Err(HestiaErr::Runtime(format!(
                    "identifier `{}` is already let-bound to {}",
                    name, x
                )));
            }
            None => {}
        }
        match self.defs.get(&name) {
            Some(x) => {
                return Err(HestiaErr::Runtime(format!(
                    "identifier `{}` is already def-bound to {}",
                    name, x
                )));
            }
            None => {}
        }
        let mut evaluated = self.eval(env.clone(), *expr)?;
        evaluated = match evaluated {
            Base::Func(_, a, b, c) => Base::Func(Some(name.clone()), a, b, c),
            _ => evaluated,
        };
        self.defs.insert(name, evaluated.clone());
        Ok(evaluated)
    }

    fn eval_and(&mut self, env: Env, exprs: Vec<Expr>) -> Result<Base, HestiaErr> {
        arity_check(exprs.len(), Some(1), None, "and")?;
        for (i, expr) in exprs.into_iter().enumerate() {
            let evaluated = self.eval(env.clone(), expr)?;
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

    pub fn eval_or(&mut self, env: Env, exprs: Vec<Expr>) -> Result<Base, HestiaErr> {
        arity_check(exprs.len(), Some(1), None, "or")?;
        for (i, expr) in exprs.into_iter().enumerate() {
            let evaluated = self.eval(env.clone(), expr)?;
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

    fn eval_if(
        &mut self,
        env: Env,
        condition: Box<Expr>,
        then: Box<Expr>,
        els: Box<Expr>,
    ) -> Result<Base, HestiaErr> {
        let evaluated_condition = self.eval(env.clone(), *condition)?;
        match evaluated_condition {
            Base::Boolean(b) => {
                if b {
                    self.eval(env, *then)
                } else {
                    self.eval(env, *els)
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

    fn eval_func(&self, env: Env, args: Vec<String>, body: Box<Expr>) -> Result<Base, HestiaErr> {
        Ok(Base::Func(None, env.clone(), args, *body))
    }

    fn eval_call(
        &mut self,
        env: Env,
        called: Box<Expr>,
        args: Vec<Expr>,
    ) -> Result<Base, HestiaErr> {
        let evaluated_called = self.eval(env.clone(), *called)?;
        let result: Result<Vec<Base>, HestiaErr> = args
            .into_iter()
            .map(|x| self.eval(env.clone(), x))
            .collect();
        let evaluated_args = result?;
        match evaluated_called {
            // Automatic currying
            Base::Func(name, local_env, names, body) => {
                let mut arguments = VecDeque::from(evaluated_args);
                let num_args = arguments.len();
                let mut new_env = local_env.clone();
                let mut parameters = VecDeque::from(names.clone());
                let num_params = parameters.len();
                loop {
                    match parameters.pop_front() {
                        Some(param) => match arguments.pop_front() {
                            Some(arg) => {
                                new_env.insert(param, arg);
                            }
                            None => {
                                parameters.push_front(param);
                                return Ok(Base::Func(
                                    name,
                                    new_env,
                                    Vec::from(parameters),
                                    body.clone(),
                                ));
                            }
                        },
                        None => {
                            if arguments.len() > 0 {
                                return Err(HestiaErr::Runtime(format!(
                                    "function `{}` expects {} arguments, received {}",
                                    name.unwrap_or("<anonymous>".to_string()),
                                    num_params,
                                    num_args,
                                )));
                            } else {
                                return self.eval(new_env.clone(), body.clone());
                            }
                        }
                    }
                }
            }
            Base::BuiltIn(f) => return f(evaluated_args),
            _ => Err(HestiaErr::Runtime(format!(
                "attempting to call non-function `{}`",
                evaluated_called
            ))),
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
    if size > max {
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

// fn arity_check(
//     size: usize,
//     min: Option<usize>,
//     max: Option<usize>,
//     name: &str,
// ) -> Result<bool, HestiaErr> {
//     match (min, max) {
//         (Some(min), Some(max)) => {
//             if size > max {
//                 return Err(HestiaErr::Runtime(format!(
//                     "built-in function `{}` expects no more than {} arguments, got {}",
//                     name, max, size
//                 )));
//             }
//             Ok(size < min)
//         }
//         (Some(min), None) => Ok(size < min),
//         (None, Some(max)) => {
//             if size > max {
//                 Err(HestiaErr::Runtime(format!(
//                     "built-in function `{}` expects no more than {} arguments, got {}",
//                     name, max, size
//                 )))
//             } else {
//                 Ok(false)
//             }
//         }
//         (None, None) => Err(HestiaErr::Internal(format!(
//             "arity check in `{}` uses two Nones",
//             name
//         ))),
//     }
// }

fn add(args: Vec<Base>) -> Result<Base, HestiaErr> {
    arity_check(args.len(), Some(2), None, "+")?;
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
