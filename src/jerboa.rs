
use std::rc::Rc;
use std::cell::RefCell;

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

pub enum Capture<'a, T, S> {
    Item(&'a T),
    Result(S),
    Option(Option<S>),
    List(Vec<S>),
}

#[derive(Clone)]
pub enum LateBound<T, S> {
    Index(usize),
    Rule(Rc<Rule<T, S>>),
}

#[derive(Clone)]
pub enum Match<T, S> {
    Pred(Rc<dyn for<'a> Fn(&T, &[Capture<'a, T, S>]) -> bool>),
    Rule(Rc<Rule<T, S>>),
    OptionRule(Rc<Rule<T, S>>),
    ListRule(Rc<Rule<T, S>>),
    UntilRule(Rc<Rule<T, S>>, Rc<Rule<T, S>>),
    RuleChoice(Vec<Rc<Rule<T, S>>>),
    LateBoundRule(RefCell<LateBound<T, S>>),
}

#[derive(Clone)]
pub struct Rule<T, S> { 
    name : Box<str>,
    matches: Vec<Match<T, S>>,
    transform : Rc<dyn for<'a> Fn(Vec<Capture<'a, T, S>>) -> Result<S, JerboaError>>,
}

impl<'a, T, S> Capture<'a, T, S> {
    pub fn unwrap(self) -> Option<&'a T> {
        match self {
            Capture::Item(x) => Some(x),
            _ => None,
        }
    }
    pub fn unwrap_result(self) -> Option<S> {
        match self {
            Capture::Result(x) => Some(x),
            _ => None,
        }
    }
    pub fn unwrap_option(self) -> Option<Option<S>> {
        match self {
            Capture::Option(x) => Some(x),
            _ => None,
        }
    }
    pub fn unwrap_list(self) -> Option<Vec<S>> {
        match self {
            Capture::List(x) => Some(x),
            _ => None,
        }
    }
}

impl<T, S> Match<T, S> {
    pub fn pred<F : for<'a> Fn(&T, &[Capture<'a, T, S>]) -> bool + 'static>( f : F ) -> Self {
        Match::Pred(Rc::new(f))
    }
    pub fn rule(r : &Rc<Rule<T, S>>) -> Self {
        Match::Rule(Rc::clone(r))
    }
    pub fn late(index : usize) -> Self {
        Match::LateBoundRule(RefCell::new(LateBound::Index(index)))
    }
    pub fn choice(rs : &[&Rc<Rule<T, S>>]) -> Self {
        Match::RuleChoice(rs.iter().map(|x| Rc::clone(x)).collect())
    }
    pub fn option(r : &Rc<Rule<T, S>>) -> Self {
        Match::OptionRule(Rc::clone(r))
    }
    pub fn list(r : &Rc<Rule<T, S>>) -> Self {
        Match::ListRule(Rc::clone(r))
    }
    pub fn until(r : &Rc<Rule<T, S>>, u : &Rc<Rule<T, S>>) -> Self {
        Match::UntilRule(Rc::clone(r), Rc::clone(u))
    }
}

impl<T, S> Rule<T, S> {
    pub fn new<N : AsRef<str>, F : for<'a> Fn(Vec<Capture<'a, T, S>>) -> Result<S, JerboaError> + 'static>
    
        (name : N, matches : Vec<Match<T, S>>, transform : F) -> Rc<Self>
        
    {
        let name : Box<str> = name.as_ref().into();
        Rc::new(Rule { name, matches, transform: Rc::new(transform) })
    }

    pub fn bind(&mut self, rules : &[&Rc<Rule<T, S>>]) {
        for m in &self.matches {
            match m {
                Match::LateBoundRule(r) => {
                    r.replace_with(|r| match r {
                        LateBound::Index(i) => LateBound::Rule(Rc::clone(rules[*i])),
                        LateBound::Rule(r) => panic!("Attempt to rebind rule: {}", r.name),
                    });
                },
                _ => { },
            }
        }
    }
}

pub fn parse<T, S>(mut input : &[T], rule: Rc<Rule<T, S>>) -> Result<Vec<S>, JerboaError> { 
    let mut results = vec![];
    while !input.is_empty() {
        match try_rule(input, &rule) {
            Ok((result, new_input)) => {
                results.push(result);
                input = new_input;
            },
            Err(e) => { return Err(e); },
        }
    } 
    Ok(results)
}

fn try_rule_choices<'a, T, S>( input : &'a [T]
                             , rules : &[Rc<Rule<T, S>>]
                             ) -> Result<(S, &'a [T]), JerboaError> {

    let mut errors = vec![];
    for rule in rules {
        match try_rule(input, rule) {
            Ok((result, new_input)) => {
                return Ok((result, new_input));
            },
            Err(e) => { errors.push(e); },
        }
    }

    Err(JerboaError::Multi(errors))
}

fn try_rule<'a, T, S>( mut input : &'a [T]
                     , rule : &Rc<Rule<T, S>>

                     ) -> Result<(S, &'a [T]), JerboaError> {

    let mut captures = vec![];
    'match_loop: for m in &rule.matches {
        match (input, m) {
            ([x, r @ ..], Match::Pred(f)) if f(x, &captures) => {
                captures.push(Capture::Item(x));      
                input = r;
            },
            (_, Match::Rule(rule)) => {
                let (value, r) = try_rule(input, rule)?;
                captures.push(Capture::Result(value));
                input = r;
            },

            (_, Match::RuleChoice(rules)) => {
                let (value, r) = try_rule_choices(input, rules)?;
                captures.push(Capture::Result(value));
                input = r;
            },

            (_, Match::OptionRule(rule)) => {
                match try_rule(input, rule) {
                    Ok((value, r)) => { 
                        captures.push(Capture::Option(Some(value)));
                        input = r;
                    },
                    Err(_) => {
                        captures.push(Capture::Option(None));
                    },
                }
            },

            (_, Match::ListRule(rule)) => {
                let mut local = vec![];
                'list_loop: loop {
                    match try_rule(input, rule) {
                        Ok((value, r)) => { 
                            local.push(value);
                            input = r;
                        },
                        Err(_) => {
                            break 'list_loop;
                        },
                    }
                }
                captures.push(Capture::List(local));
            },

            (_, Match::UntilRule(rule, until)) => {

                if try_rule(input, until).is_ok() {
                    captures.push(Capture::List(vec![]));
                    continue 'match_loop;
                }

                let mut local_input = input;
                let mut local = vec![];
               'until_loop: loop {
                    match try_rule(local_input, rule) {
                        Ok((value, r)) => { 
                            local.push(value);
                            local_input = r;

                            if try_rule(local_input, until).is_ok() {
                                captures.push(Capture::List(local));
                                input = local_input;
                                break 'until_loop;
                            }
                        },
                        Err(_) => {
                            return Err(JerboaError::RuleFailedToMatch(rule.name.clone()));
                        },
                    }
                }
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
    fn should_parse_single_predicate() {
        let input = "aaa".chars().collect::<Vec<_>>();

        let p = Match::pred(|v, _| *v == 'a');
        let r = Rule::new("a-rule", vec![p], |_| Ok(true));

        let output = parse(&input, r).unwrap();

        assert_eq!(output, [true, true, true]);
    }

    #[test]
    fn should_parse_double_predicate() {
        let input = "abab".chars().collect::<Vec<_>>();

        let ap = Match::pred(|v, _| *v == 'a');
        let bp = Match::pred(|v, _| *v == 'b');
        let r = Rule::new("ab-rule", vec![ap, bp], |_| Ok(true));

        let output = parse(&input, r).unwrap();

        assert_eq!(output, [true, true]);
    }

    #[test]
    fn should_parse_rule() {
        let input = "abab".chars().collect::<Vec<_>>();

        let ap = Match::pred(|v, _| *v == 'a');
        let bp = Match::pred(|v, _| *v == 'b');
        let rb = Rule::new("b-rule", vec![bp], |_| Ok(true));
        let ra = Rule::new("ab-rule", vec![ap, Match::rule(&rb)], |_| Ok(true));

        let output = parse(&input, ra).unwrap();

        assert_eq!(output, [true, true]);
    }

    #[test]
    fn should_parse_option_rule() {
        let input = "aab".chars().collect::<Vec<_>>();

        let ap = Match::pred(|v, _| *v == 'a');
        let bp = Match::pred(|v, _| *v == 'b');
        let rb = Rule::new("b-rule", vec![bp], |_| Ok(true));
        let ra = Rule::new("ab-rule", vec![ap, Match::option(&rb)], |_| Ok(true));

        let output = parse(&input, ra).unwrap();

        assert_eq!(output, [true, true]);
    }

    #[test]
    fn should_parse_list_rule() {
        let input = "abbb".chars().collect::<Vec<_>>();

        let ap = Match::pred(|v, _| *v == 'a');
        let bp = Match::pred(|v, _| *v == 'b');
        let rb = Rule::new("b-rule", vec![bp], |_| Ok(true));
        let ra = Rule::new("ab*-rule", vec![ap, Match::list(&rb)], |_| Ok(true));

        let output = parse(&input, ra).unwrap();

        assert_eq!(output, [true]);
    }

    #[test]
    fn should_parse_until_rule() {
        let input = "aaaaab".chars().collect::<Vec<_>>();

        let ap = Match::pred(|v, _| *v == 'a');
        let bp = Match::pred(|v, _| *v == 'b');
        let rb = Rule::new("b-rule", vec![bp], |_| Ok(true));
        let ra = Rule::new("a-rule", vec![ap], |_| Ok(true));
        let r = Rule::new("a*b-rule", vec![Match::until(&ra, &rb)], |_| Ok(true));
        // Note: rb is first because until input is not consumed in until matcher
        // This means that if r rule is first in the choice then after consumeing the 
        // a*b rule, that rule will continue to be used to consume zero input forever.
        let rc = Rule::new("or-rule", vec![Match::choice(&[&rb, &r])], |_| Ok(true));

        let output = parse(&input, rc).unwrap();

        assert_eq!(output, [true, true]);
    }
}
