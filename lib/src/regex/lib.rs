use crate::error::HestiaErr;
use std::collections::VecDeque;

#[derive(Clone)]
pub enum Transition {
    Nothing,
    Anything,
    Literal(char),
}

impl Transition {
    pub fn matches(&self, c: Option<&char>) -> (bool, bool) {
        match (self, c) {
            (Transition::Nothing, _) => (true, false),
            (Transition::Anything, Some(_)) => (true, true),
            (Transition::Literal(m), Some(n)) => (m == n, true),
            _ => (false, false),
        }
    }
}

#[derive(Clone)]
pub struct State {
    edges: Vec<(Transition, Box<State>)>,
}

impl State {
    pub fn is_terminal(&self) -> bool {
        self.edges.is_empty()
    }
}

fn terminal() -> State {
    State { edges: Vec::new() }
}

pub struct Regex {
    // states, starts with one empty
    paths: Vec<Transition>,
    start: Box<State>,
}

impl Regex {
    fn start_edges(&self) -> Vec<(Transition, Box<State>)> {
        self.paths
            .clone()
            .into_iter()
            .map(|t| (t, self.start.clone()))
            .collect()
    }
    // regex: ba+c s: baaac
    // state: b> a>
    pub fn evaluate(&mut self, s: &str) -> Result<bool, HestiaErr> {
        let chars: Vec<char> = s.chars().collect();
        let mut cursor = 0;
        let mut states: VecDeque<Box<State>> = VecDeque::new();
        let shadow = State {
            edges: self.start_edges(),
        };
        states.push_back(Box::new(shadow));
        loop {
            match states.pop_front() {
                None => break,
                Some(state) => {
                    if state.is_terminal() {
                        // && cursor == s.len() {
                        return Ok(true);
                    }
                    for (transition, state) in state.edges {
                        let (matched, _) = transition.matches(chars.get(cursor));
                        if matched {
                            states.push_back(state.clone());
                        }
                    }
                }
            }
            cursor += 1;
        }
        //Ok(cursor == s.len())
        Ok(false)
    }
}

pub fn eval(s: &str) -> Result<bool, HestiaErr> {
    let t = Box::new(terminal());
    let paths = vec![Transition::Literal('a')];
    let mut start = Box::new(State { edges: Vec::new() });
    start.edges.push((Transition::Literal('a'), start.clone()));
    start.edges.push((Transition::Literal('b'), t));
    let mut r = Regex { paths, start };
    r.evaluate(s)
}
