mod builtin;

use crate::error::HestiaErr;
use crate::evaluator::builtin::*;
use crate::parser::{parse, Expr, Hashable, Map};
use std::collections::{HashMap, VecDeque};
use std::fmt;

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    Integer,
    Boolean,
    Str,
    Symbol,
    Float,
    List,
    Map,
    Func,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Integer => write!(f, "Integer"),
            Type::Boolean => write!(f, "Boolean"),
            Type::Str => write!(f, "String"),
            Type::Symbol => write!(f, "Symbol"),
            Type::Float => write!(f, "Float"),
            Type::List => write!(f, "List"),
            Type::Map => write!(f, "Map"),
            Type::Func => write!(f, "Function"),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Base {
    Hashable(Hashable),
    Float(f64),
    List(Vec<Base>),
    Map(Map<Base>),
    Func(Option<String>, HashMap<String, Base>, Vec<String>, Expr),
    BuiltIn(Func),
}

pub fn get_arg(name: &str, index: usize, args: &[Base]) -> Result<Base, HestiaErr> {
    match args.get(index) {
        Some(x) => Ok(x.clone()),
        None => Err(HestiaErr::Internal(format!(
            "built-in function `{}` attempting to access non-existent argument {}",
            name, index
        ))),
    }
}

impl Base {
    fn to_type(&self) -> Type {
        match self {
            Base::Hashable(Hashable::Integer(_)) => Type::Integer,
            Base::Hashable(Hashable::Boolean(_)) => Type::Boolean,
            Base::Hashable(Hashable::Str(_)) => Type::Str,
            Base::Hashable(Hashable::Symbol(_)) => Type::Symbol,
            Base::Float(_) => Type::Float,
            Base::List(_) => Type::List,
            Base::Map(_) => Type::Map,
            Base::Func(..) => Type::Func,
            Base::BuiltIn(_) => Type::Func,
        }
    }
}

// TODO: define type function that converts base to human-readable type
impl fmt::Display for Base {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Base::Hashable(h) => write!(f, "{}", h),
            // Base::Hashable(Hashable::Boolean(b)) => write!(f, "{}", b),
            // Base::Hashable(Hashable::Str(s)) => write!(f, "\"{}\"", s),
            // Base::Hashable(Hashable::Symbol(s)) => write!(f, "\"{}\"", s),
            Base::Float(n) => write!(f, "{:?}", n),
            Base::List(v) => {
                let args: Vec<String> = v.iter().map(|x| format!("{}", x)).collect();
                write!(f, "[{}]", args.join(" "))
            }
            Base::Map(hm) => {
                write!(f, "{{ ")?;
                for (k, v) in hm.map.iter() {
                    write!(f, "{}: {} ", k, v)?;
                }
                write!(f, "}}")
            }
            Base::Func(name, _, args, body) => {
                write!(
                    f,
                    "closure {}: {{ |{}| {} }}",
                    name.clone().unwrap_or("<anonymous>".to_string()),
                    args.join(" "),
                    body
                )
            }
            Base::BuiltIn(func) => write!(f, "function: {}", func.name),
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

impl Default for Evaluator {
    fn default() -> Self {
        Evaluator::new()
    }
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
            Expr::Hashable(h) => Ok(Base::Hashable(h)),
            Expr::Float(n) => Ok(Base::Float(n)),
            Expr::List(v) => {
                let elements: Result<Vec<Base>, HestiaErr> =
                    v.into_iter().map(|x| self.eval(env.clone(), x)).collect();
                Ok(Base::List(elements?))
            }
            Expr::Map(hm) => {
                let mut result: HashMap<Hashable, Base> = HashMap::new();
                for (k, v) in hm.map.into_iter() {
                    result.insert(k, self.eval(env.clone(), v)?);
                }
                Ok(Base::Map(Map { map: result }))
            }
            Expr::And(v) => self.eval_and(env, v),
            Expr::Or(v) => self.eval_or(env, v),
            Expr::If(c, i, e) => self.eval_if(env, *c, *i, *e),
            Expr::Func(v, b) => self.eval_func(env, v, *b),
            Expr::Call(f, v) => self.eval_call(env, *f, v),
            Expr::Let(v, b) => self.eval_let(env, v, *b),
            Expr::Def(n, e) => self.eval_def(env, n, *e),
            Expr::Identifier(i) => self.eval_identifier(env, i),
        }
    }

    fn eval_let(
        &mut self,
        mut env: Env,
        bindings: Vec<(String, Expr)>,
        body: Expr,
    ) -> Result<Base, HestiaErr> {
        for (name, expr) in bindings.into_iter() {
            let evaluated_expr = self.eval(env.clone(), expr)?;
            env.insert(name, evaluated_expr);
        }
        self.eval(env, body)
    }

    fn eval_identifier(&self, env: Env, name: String) -> Result<Base, HestiaErr> {
        if let Some(x) = env.get(&name) {
            return Ok(x.clone());
        }
        if let Some(x) = self.defs.get(&name) {
            return Ok(x.clone());
        }
        match self.builtins.get(&name) {
            Some(x) => Ok(x.clone()),
            None => Err(HestiaErr::Runtime(format!(
                "attempting to use unbound identifier `{}`",
                name
            ))),
        }
    }

    fn eval_def(&mut self, env: Env, name: String, expr: Expr) -> Result<Base, HestiaErr> {
        if self.builtins.contains_key(&name) {
            return Err(HestiaErr::Runtime(format!(
                "identifier `{}` is already bound to a built-in function",
                name
            )));
        }
        if let Some(x) = env.get(&name) {
            return Err(HestiaErr::Runtime(format!(
                "identifier `{}` is already let-bound to {}",
                name, x
            )));
        }
        if let Some(x) = self.defs.get(&name) {
            return Err(HestiaErr::Runtime(format!(
                "identifier `{}` is already def-bound to {}",
                name, x
            )));
        }
        let mut evaluated = self.eval(env, expr)?;
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
                Base::Hashable(Hashable::Boolean(b)) => {
                    if !b {
                        return Ok(Base::Hashable(Hashable::Boolean(false)));
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
        Ok(Base::Hashable(Hashable::Boolean(true)))
    }

    pub fn eval_or(&mut self, env: Env, exprs: Vec<Expr>) -> Result<Base, HestiaErr> {
        arity_check(exprs.len(), Some(1), None, "or")?;
        for (i, expr) in exprs.into_iter().enumerate() {
            let evaluated = self.eval(env.clone(), expr)?;
            match evaluated {
                Base::Hashable(Hashable::Boolean(b)) => {
                    if b {
                        return Ok(Base::Hashable(Hashable::Boolean(true)));
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
        Ok(Base::Hashable(Hashable::Boolean(false)))
    }

    fn eval_if(
        &mut self,
        env: Env,
        condition: Expr,
        then: Expr,
        els: Expr,
    ) -> Result<Base, HestiaErr> {
        let evaluated_condition = self.eval(env.clone(), condition)?;
        match evaluated_condition {
            Base::Hashable(Hashable::Boolean(b)) => {
                if b {
                    self.eval(env, then)
                } else {
                    self.eval(env, els)
                }
            }
            _ => Err(HestiaErr::Runtime(format!(
                "`if` expects a condition of type Boolean, got {}",
                evaluated_condition
            ))),
        }
    }

    fn eval_func(&self, env: Env, args: Vec<String>, body: Expr) -> Result<Base, HestiaErr> {
        Ok(Base::Func(None, env, args, body))
    }

    fn eval_call(&mut self, env: Env, called: Expr, args: Vec<Expr>) -> Result<Base, HestiaErr> {
        let evaluated_called = self.eval(env.clone(), called)?;
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
                let mut parameters = VecDeque::from(names);
                let num_params = parameters.len();
                loop {
                    match parameters.pop_front() {
                        Some(param) => match arguments.pop_front() {
                            Some(arg) => {
                                new_env.insert(param, arg);
                            }
                            None => {
                                parameters.push_front(param);
                                return Ok(Base::Func(name, new_env, Vec::from(parameters), body));
                            }
                        },
                        None => {
                            if !arguments.is_empty() {
                                return Err(HestiaErr::Runtime(format!(
                                    "function `{}` expects {} arguments, received {}",
                                    name.unwrap_or("<anonymous>".to_string()),
                                    num_params,
                                    num_args,
                                )));
                            } else {
                                return self.eval(new_env.clone(), body);
                            }
                        }
                    }
                }
            }
            Base::BuiltIn(f) => f.call(evaluated_args),
            _ => Err(HestiaErr::Runtime(format!(
                "attempting to call non-function `{}`",
                evaluated_called
            ))),
        }
    }
}
