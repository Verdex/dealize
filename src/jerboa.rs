
use std::rc::Rc;

#[derive(Debug)]
pub enum JerboaError {
    UnexpectedEndOfInput(Box<str>),
    RuleFailedToMatch(Box<str>),
    Multi(Vec<JerboaError>),
}

impl std::fmt::Display for JerboaError {
    fn fmt(&self, f : &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            JerboaError::UnexpectedEndOfInput(n) => write!(f, "Unexpected end of input in rule: {}", n),
            JerboaError::RuleFailedToMatch(n) => write!(f, "Rule:  {} failed to match", n),
            JerboaError::Multi(errors) => write!(f, "Multiple Rule Failure {:?}", errors),
        }
    }
}

impl std::error::Error for JerboaError { }

pub enum Capture<'a, T> {
    Item(&'a T),
    Option(Option<&'a T>),
    List(Vec<&'a T>)
}

#[derive(Clone)]
pub enum MatchOpt {
    None,
    Option,
    List,
}

#[derive(Clone)]
pub enum Match<T> {
    Free(Rc<dyn Fn(&T) -> bool>, MatchOpt),
    Context(Rc<dyn for<'a> Fn(&T, &[Capture<'a, T>]) -> bool>, MatchOpt),
}

pub struct Rule<T, S> { // TODO should fields be public or should there be some sort of constructor?
    name : Box<str>,
    matches: Vec<Match<T>>,
    transform : Rc<dyn for<'a> Fn(Vec<Capture<'a, T>>) -> S>,
}

pub fn parse<T, S>(mut input : &[T], rules: &[Rule<T, S>]) -> Result<Vec<S>, JerboaError> { 
    let mut results = vec![];
    'outer : while !input.is_empty() {
        let mut errors = vec![];
        for rule in rules {
            match try_rule(input, rule) {
                Ok((result, new_input)) => {
                    results.push(result);
                    input = new_input;
                    continue 'outer;
                },
                Err(e) => { errors.push(e); },
            }
        }
        return Err(JerboaError::Multi(errors));
    } 
    Ok(results)
}

fn try_rule<'a, T, S>(mut input : &'a [T], rule : &Rule<T, S>) -> Result<(S, &'a [T]), JerboaError> {
    let mut captures = vec![];
    for m in &rule.matches {
        match (input, m) {
            // Item
            ([x, r @ ..], Match::Free(f, MatchOpt::None)) if f(x) => {
                captures.push(Capture::Item(x));      
                input = r;
            },
            ([x, r @ ..], Match::Context(f, MatchOpt::None)) if f(x, &captures) => {
                captures.push(Capture::Item(x));      
                input = r;
            },

            // Option
            ([x, r @ ..], Match::Free(f, MatchOpt::Option)) if f(x) => {
                captures.push(Capture::Option(Some(x)));      
                input = r;
            },
            (_, Match::Free(_, MatchOpt::Option)) => {
                captures.push(Capture::Option(None));      
            },
            ([x, r @ ..], Match::Context(f, MatchOpt::Option)) if f(x, &captures) => {
                captures.push(Capture::Option(Some(x)));      
                input = r;
            },
            (_, Match::Context(_, MatchOpt::Option)) => {
                captures.push(Capture::Option(None));      
            },

            // List
            ([x, r @ ..], Match::Free(f, MatchOpt::List)) => {
                let mut x = x;
                let mut r = r;

                let mut local = vec![];
                while f(x) {
                    local.push(x);      
                    input = r;
                    match input { 
                        [x2, r2 @ ..] => { x = x2; r = r2; },
                        _ => { break; },
                    }
                }
                captures.push(Capture::List(local));
            },
            (_, Match::Free(_, MatchOpt::List)) => {
                captures.push(Capture::List(vec![]));
            },
            ([x, r @ ..], Match::Context(f, MatchOpt::List)) => {
                let mut x = x;
                let mut r = r;

                let mut local = vec![];
                while f(x, &captures) {
                    local.push(x);      
                    input = r;
                    match input { 
                        [x2, r2 @ ..] => { x = x2; r = r2; },
                        _ => { break; },
                    }
                }
                captures.push(Capture::List(local));
            },
            (_, Match::Context(_, MatchOpt::List)) => {
                captures.push(Capture::List(vec![]));
            },

            // Error
            ([], _) => {
                return Err(JerboaError::UnexpectedEndOfInput(rule.name.clone()));
            },
            (_, _) => { 
                return Err(JerboaError::RuleFailedToMatch(rule.name.clone()));
            },
        }
    }
    Ok(((rule.transform)(captures), input))
}
