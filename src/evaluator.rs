mod builtin;

use crate::error::HestiaErr;
use crate::evaluator::builtin::*;
use crate::parser::{parse, Expr};
use std::collections::{HashMap, VecDeque};
use std::fmt;

#[derive(Clone)]
pub enum Base {
    Number(f64),
    Boolean(bool),
    Str(String),
    List(Vec<Base>),
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
            Base::List(v) => {
            let args: Vec<String> = v.iter().map(|x| format!("{}", x)).collect();
            write!(f, "[{}]", args.join(" "))
            }
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

type Env = HashMap<String, Base>;

pub struct Evaluator {
    builtins: Env,
    defs: Env,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            builtins: builtins(),
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
            Expr::List(v) => {
                let elements: Result<Vec<Base>, HestiaErr> =
                    v.into_iter().map(|x| self.eval(env.clone(), x)).collect();
                Ok(Base::List(elements?))
            }
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
            Base::BuiltIn(f) => return f.call(evaluated_args),
            _ => Err(HestiaErr::Runtime(format!(
                "attempting to call non-function `{}`",
                evaluated_called
            ))),
        }
    }
}
