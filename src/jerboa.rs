
use std::rc::Rc;

#[derive(Debug)]
pub enum JerboaError {
    UnexpectedEndOfInput(Box<str>),
    RuleFailedToMatch(Box<str>),
    Multi(Vec<JerboaError>),
    Other(Box<dyn std::error::Error>),
}

impl std::fmt::Display for JerboaError {
    fn fmt(&self, f : &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            JerboaError::UnexpectedEndOfInput(n) => write!(f, "Unexpected end of input in rule: {}", n),
            JerboaError::RuleFailedToMatch(n) => write!(f, "Rule:  {} failed to match", n),
            JerboaError::Multi(errors) => write!(f, "Multiple Rule Failure {:?}", errors),
            JerboaError::Other(e) => write!(f, "Other Error Encountered {:?}", e),
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

impl<T> Match<T> {
    pub fn free<F : Fn(&T) -> bool + 'static>( f : F ) -> Self {
        Match::Free(Rc::new(f), MatchOpt::None)
    }
    pub fn context<F : for<'a> Fn(&T, &[Capture<'a, T>]) -> bool + 'static>( f : F ) -> Self {
        Match::Context(Rc::new(f), MatchOpt::None)
    }
    pub fn option(&mut self) {
        match self {
            Match::Free(_, ref mut opt) => { *opt = MatchOpt::Option; },
            Match::Context(_, ref mut opt) => { *opt = MatchOpt::Option; },
        }
    }
    pub fn list(&mut self) {
        match self {
            Match::Free(_, ref mut opt) => { *opt = MatchOpt::List; },
            Match::Context(_, ref mut opt) => { *opt = MatchOpt::List; },
        }
    }
}

pub struct Rule<T, S> { 
    name : Box<str>,
    matches: Vec<Match<T>>,
    transform : Rc<dyn for<'a> Fn(Vec<Capture<'a, T>>) -> Result<S, JerboaError>>,
}

impl<T, S> Rule<T, S> {
    pub fn new<N : AsRef<str>, F : for<'a> Fn(Vec<Capture<'a, T>>) -> Result<S, JerboaError> + 'static>
    
        (name : N, matches : Vec<Match<T>>, transform : F) -> Self
        
    {
        Rule { name: name.as_ref().into(), matches, transform: Rc::new(transform) }
    }
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
    Ok(((rule.transform)(captures)?, input))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn should_parse_empty_input() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let a_rule = Rule::new("a", vec![match_a], |_| Ok(1));
        let output = parse(&[], &[a_rule]).unwrap();

        assert!(output.is_empty());
    }

    #[test]
    fn should_parse_single_match_rule() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let a_rule = Rule::new("a", vec![match_a], |_| Ok(1));
        let input = "aaaa".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule]).unwrap();

        assert_eq!(output, [1, 1, 1, 1]);
    }

    #[test]
    fn should_parse_double_match_rule() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_b = Match::free(|c : &char| *c == 'b');
        let rule = Rule::new("ab", vec![match_a, match_b], |_| Ok(1));
        let input = "abab".chars().collect::<Vec<_>>();
        let output = parse(&input, &[rule]).unwrap();

        assert_eq!(output, [1, 1]);
    }

    #[test]
    fn should_parse_two_single_rules() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_b = Match::free(|c : &char| *c == 'b');
        let a_rule = Rule::new("a", vec![match_a], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));
        let input = "abab".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2, 1, 2]);
    }

    #[test]
    fn should_parse_two_rules() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_b = Match::free(|c : &char| *c == 'b');
        let match_c = Match::free(|c : &char| *c == 'c');
        let ab_rule = Rule::new("ab", vec![match_a, match_b], |_| Ok(1));
        let c_rule = Rule::new("c", vec![match_c], |_| Ok(2));
        let input = "abcab".chars().collect::<Vec<_>>();
        let output = parse(&input, &[ab_rule, c_rule]).unwrap();

        assert_eq!(output, [1, 2, 1]);
    }

    #[test]
    fn should_be_able_to_resuse_match() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_b = Match::free(|c : &char| *c == 'b');
        let ab_rule = Rule::new("ab", vec![match_a.clone(), match_b.clone()], |_| Ok(1));
        let ba_rule = Rule::new("ba", vec![match_b, match_a], |_| Ok(2));

        let input = "abba".chars().collect::<Vec<_>>();
        let output = parse(&input, &[ab_rule, ba_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    // list
    // option
    // list after all input is consumed
    // option after all input is consumed
    // error from transformer
    // error from no rules matching
    // error from all input being consumed
    // put parser into a transfomer

}