
use std::rc::Rc;

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
    matches: Vec<Match<T>>,
    transform : Rc<dyn for<'a> Fn(Vec<Capture<'a, T>>) -> S>,
}

pub fn parse<T, S>(input : &[T], rules: &[Rule<T, S>]) -> Result<Vec<S>, Box<dyn std::error::Error>> { // TODO error typet 
    for rule in rules {

    }
    todo!()
}

fn try_rule<'a, T, S>(mut input : &'a [T], rule : &Rule<T, S>) -> (S, &'a [T]) {
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
            (_, Match::Free(f, MatchOpt::Option)) => {
                captures.push(Capture::Option(None));      
            },
            ([x, r @ ..], Match::Context(f, MatchOpt::Option)) if f(x, &captures) => {
                captures.push(Capture::Option(Some(x)));      
                input = r;
            },
            (_, Match::Context(f, MatchOpt::Option)) => {
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
            (_, Match::Free(f, MatchOpt::List)) => {
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
            (_, Match::Context(f, MatchOpt::List)) => {
                captures.push(Capture::List(vec![]));
            },

            // Error
            _ => {

            },
        }
    }
    ((rule.transform)(captures), input)
}