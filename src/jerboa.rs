
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

pub enum Capture<'a, T, S> {
    Item(&'a T),
    Result(S),
    Option(Option<S>),
    List(Vec<S>),
}

#[derive(Clone)]
pub enum Match<T, S> {
    Pred(Rc<dyn for<'a> Fn(&T, &[Capture<'a, T, S>]) -> bool>),
    Rule(Rc<Rule<T, S>>),
    OptionRule(Rc<Rule<T, S>>),
    ListRule(Rc<Rule<T, S>>),
    UntilRule(Rc<Rule<T, S>>, Rc<Rule<T, S>>),
    RuleChoice(Vec<Rc<Rule<T, S>>>),
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
    for m in &rule.matches {
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
                loop {
                    match try_rule(input, rule) {
                        Ok((value, r)) => { 
                            local.push(value);
                            input = r;
                        },
                        Err(_) => {
                            break;
                        },
                    }
                }
                captures.push(Capture::List(local));
            },

            (_, Match::UntilRule(rule, until)) => {

                if try_rule(input, until).is_ok() {
                    captures.push(Capture::List(vec![]));
                    continue;
                }

                let mut local_input = input;
                let mut local = vec![];
                loop {
                    match try_rule(local_input, rule) {
                        Ok((value, r)) => { 
                            local.push(value);
                            local_input = r;

                            if try_rule(local_input, until).is_ok() {
                                captures.push(Capture::List(local));
                                input = local_input;
                                break;
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



/*

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn should_parse_empty_input() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let a_rule = Rule::new("a", vec![match_a], |_| Ok(1));
        let output = parse(&[], &a_rule.clone(), &[a_rule]).unwrap();

        assert!(output.is_empty());
    }

    #[test]
    fn should_parse_single_match_rule() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let a_rule = Rule::new("a", vec![match_a], |_| Ok(1));
        let input = "aaaa".chars().collect::<Vec<_>>();
        let output = parse(&input, &a_rule.clone(), &[a_rule]).unwrap();

        assert_eq!(output, [1, 1, 1, 1]);
    }

    #[test]
    fn should_parse_double_match_rule() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_b = Match::free(|c : &char| *c == 'b');
        let rule = Rule::new("ab", vec![match_a, match_b], |_| Ok(1));
        let input = "abab".chars().collect::<Vec<_>>();
        let output = parse(&input, &rule.clone(), &[rule]).unwrap();

        assert_eq!(output, [1, 1]);
    }

    #[test]
    fn should_parse_two_single_rules() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_b = Match::free(|c : &char| *c == 'b');
        let a_rule = Rule::new("a", vec![match_a], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));
        let main = Rule::fixed("main", [Match::rule_choice(&["a", "b"])], |mut x| Ok(x.remove(0).unwrap_result().unwrap()));
        let input = "abab".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2, 1, 2]);
    }

    #[test]
    fn should_parse_two_rules() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_b = Match::free(|c : &char| *c == 'b');
        let match_c = Match::free(|c : &char| *c == 'c');
        let ab_rule = Rule::new("ab", vec![match_a, match_b], |_| Ok(1));
        let c_rule = Rule::new("c", vec![match_c], |_| Ok(2));
        let main = Rule::fixed("main", [Match::rule_choice(&["ab", "c"])], |mut x| Ok(x.remove(0).unwrap_result().unwrap()));
        let input = "abcab".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[ab_rule, c_rule]).unwrap();

        assert_eq!(output, [1, 2, 1]);
    }

    #[test]
    fn should_be_able_to_resuse_match() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_b = Match::free(|c : &char| *c == 'b');
        let ab_rule = Rule::new("ab", vec![match_a.clone(), match_b.clone()], |_| Ok(1));
        let ba_rule = Rule::new("ba", vec![match_b, match_a], |_| Ok(2));
        let main = Rule::fixed("main", [Match::rule_choice(&["ab", "ba"])], |mut x| Ok(x.remove(0).unwrap_result().unwrap()));

        let input = "abba".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[ab_rule, ba_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_list_rule() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_b = Match::free(|c : &char| *c == 'b');
        let match_c = Match::free(|c : &char| *c == 'c').list();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));
        let main = Rule::fixed("main", [Match::rule_choice(&["ac", "b"])], |mut x| Ok(x.remove(0).unwrap_result().unwrap()));

        let input = "acccb".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_list_rule_at_end() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_c = Match::free(|c : &char| *c == 'c').list();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));

        let input = "accc".chars().collect::<Vec<_>>();
        let output = parse(&input, &a_rule.clone(), &[a_rule]).unwrap();

        assert_eq!(output, [1]);
    }

    #[test]
    fn should_parse_list_rule_with_empty_list() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_b = Match::free(|c : &char| *c == 'b');
        let match_c = Match::free(|c : &char| *c == 'c').list();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));
        let main = Rule::fixed("main", [Match::rule_choice(&["ac", "b"])], |mut x| Ok(x.remove(0).unwrap_result().unwrap()));

        let input = "ab".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_list_rule_at_end_with_empty_list() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_c = Match::free(|c : &char| *c == 'c').list();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));

        let input = "a".chars().collect::<Vec<_>>();
        let output = parse(&input, &a_rule.clone(), &[a_rule]).unwrap();

        assert_eq!(output, [1]);
    }
    
    #[test]
    fn should_parse_option_rule() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_b = Match::free(|c : &char| *c == 'b');
        let match_c = Match::free(|c : &char| *c == 'c').option();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));
        let main = Rule::fixed("main", [Match::rule_choice(&["ac", "b"])], |mut x| Ok(x.remove(0).unwrap_result().unwrap()));

        let input = "acb".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_option_rule_at_end() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_c = Match::free(|c : &char| *c == 'c').option();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));

        let input = "ac".chars().collect::<Vec<_>>();
        let output = parse(&input, &a_rule.clone(), &[a_rule]).unwrap();

        assert_eq!(output, [1]);
    }

    #[test]
    fn should_parse_option_rule_with_nothing() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_b = Match::free(|c : &char| *c == 'b');
        let match_c = Match::free(|c : &char| *c == 'c').option();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));
        let main = Rule::fixed("main", [Match::rule_choice(&["ac", "b"])], |mut x| Ok(x.remove(0).unwrap_result().unwrap()));

        let input = "ab".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_option_rule_at_end_with_nothing() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_c = Match::free(|c : &char| *c == 'c').option();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));

        let input = "a".chars().collect::<Vec<_>>();
        let output = parse(&input, &a_rule.clone(), &[a_rule]).unwrap();

        assert_eq!(output, [1]);
    }

    #[test]
    fn should_parse_empty_input_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let a_rule = Rule::new("a", vec![match_a], |_| Ok(1));
        let output = parse(&[], &a_rule.clone(), &[a_rule]).unwrap();

        assert!(output.is_empty());
    }

    #[test]
    fn should_parse_single_match_rule_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let a_rule = Rule::new("a", vec![match_a], |_| Ok(1));
        let input = "aaaa".chars().collect::<Vec<_>>();
        let output = parse(&input, &a_rule.clone(), &[a_rule]).unwrap();

        assert_eq!(output, [1, 1, 1, 1]);
    }

    #[test]
    fn should_parse_list_rule_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let match_b = Match::context(|c : &char, _| *c == 'b');
        let match_c = Match::context(|c : &char, _| *c == 'c').list();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));
        let main = Rule::fixed("main", [Match::rule_choice(&["ac", "b"])], |mut x| Ok(x.remove(0).unwrap_result().unwrap()));

        let input = "acccb".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_list_rule_at_end_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let match_c = Match::context(|c : &char, _| *c == 'c').list();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));

        let input = "accc".chars().collect::<Vec<_>>();
        let output = parse(&input, &a_rule.clone(), &[a_rule]).unwrap();

        assert_eq!(output, [1]);
    }

    #[test]
    fn should_parse_list_rule_with_empty_list_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let match_b = Match::context(|c : &char, _| *c == 'b');
        let match_c = Match::context(|c : &char, _| *c == 'c').list();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));
        let main = Rule::fixed("main", [Match::rule_choice(&["ac", "b"])], |mut x| Ok(x.remove(0).unwrap_result().unwrap()));

        let input = "ab".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_list_rule_at_end_with_empty_list_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let match_c = Match::context(|c : &char, _| *c == 'c').list();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));

        let input = "a".chars().collect::<Vec<_>>();
        let output = parse(&input, &a_rule.clone(), &[a_rule]).unwrap();

        assert_eq!(output, [1]);
    }
    
    #[test]
    fn should_parse_option_rule_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let match_b = Match::context(|c : &char, _| *c == 'b');
        let match_c = Match::context(|c : &char, _| *c == 'c').option();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));
        let main = Rule::fixed("main", [Match::rule_choice(&["ac", "b"])], |mut x| Ok(x.remove(0).unwrap_result().unwrap()));

        let input = "acb".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_option_rule_at_end_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let match_c = Match::context(|c : &char, _| *c == 'c').option();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));

        let input = "ac".chars().collect::<Vec<_>>();
        let output = parse(&input, &a_rule.clone(), &[a_rule]).unwrap();

        assert_eq!(output, [1]);
    }

    #[test]
    fn should_parse_option_rule_with_nothing_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let match_b = Match::context(|c : &char, _| *c == 'b');
        let match_c = Match::context(|c : &char, _| *c == 'c').option();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));
        let main = Rule::fixed("main", [Match::rule_choice(&["ac", "b"])], |mut x| Ok(x.remove(0).unwrap_result().unwrap()));

        let input = "ab".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_option_rule_at_end_with_nothing_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let match_c = Match::context(|c : &char, _| *c == 'c').option();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));

        let input = "a".chars().collect::<Vec<_>>();
        let output = parse(&input, &a_rule.clone(), &[a_rule]).unwrap();

        assert_eq!(output, [1]);
    }

    #[test]
    fn should_parse_match_context() {
        let digit = Match::free(|c : &char| c.is_digit(10) );
        let added = Match::context(|c : &char, captures| {
            if let Ok(d) = c.to_string().parse::<u8>() {
                let v = captures.iter().map(|c| match c {
                    Capture::Item(v) => v.to_string().parse::<u8>().unwrap(),
                    _ => unreachable!(),
                }).reduce(|a, b| a + b).unwrap();
                v == d 
            }
            else {
                false
            }
        });
        let r1 = Rule::fixed("add1", [digit.clone(), digit.clone(), added.clone()], |_| Ok(1));
        let r2 = Rule::fixed("add2", [digit.clone(), digit.clone(), digit, added], |_| Ok(2));
        let main = Rule::fixed("main", [Match::rule_choice(&["add1", "add2"])], |mut x| Ok(x.remove(0).unwrap_result().unwrap()));

        let input = "1247123".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[r1, r2]).unwrap();

        assert_eq!(output, [2, 1]);
    }

    #[test]
    fn should_indicate_error_from_transformer() {
        let m = Match::free(|_ : &char| true);
        let r = Rule::fixed("error", [m], |_| -> Result<u8, _> { Err(JerboaError::Multi(vec![])) });

        let input = "a".chars().collect::<Vec<_>>();
        let output = parse(&input, &r.clone(), &[r]);

        assert!(matches!(output, Err(JerboaError::Multi(_))));
    }

    #[test]
    fn should_indicate_error_from_no_rules_matching() {
        let m = Match::free(|_ : &char| false);
        let r = Rule::fixed("e1", [m.clone()], |_| -> Result<u8, _> { Err(JerboaError::Multi(vec![])) });
        let r2 = Rule::fixed("e2", [m], |_| -> Result<u8, _> { Err(JerboaError::Multi(vec![])) });
        let main = Rule::fixed("main", [Match::rule_choice(&["e1", "e2"])], |_| Ok(1));

        let input = "a".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[r, r2]);

        match output {
            Err(JerboaError::Multi(es)) => {
                match &es[..] {
                    [JerboaError::RuleFailedToMatch(n1), JerboaError::RuleFailedToMatch(n2)] => { 
                        assert_eq!(n1.to_string(), "e1");
                        assert_eq!(n2.to_string(), "e2");
                    },
                    _ => { assert!(false); },
                }
            },
            _ => { assert!(false); },
        }
    }

    #[test]
    fn should_indicate_error_from_end_of_input() {
        let m = Match::free(|_ : &char| true);
        let r = Rule::fixed("e1", [m.clone(), m.clone()], |_| -> Result<u8, _> { Err(JerboaError::Multi(vec![])) });
        let r2 = Rule::fixed("e2", [m.clone(), m], |_| -> Result<u8, _> { Err(JerboaError::Multi(vec![])) });
        let main = Rule::fixed("main", [Match::rule_choice(&["e1", "e2"])], |_| Ok(1));

        let input = "a".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[r, r2]);

        match output {
            Err(JerboaError::Multi(es)) => {
                match &es[..] {
                    [JerboaError::UnexpectedEndOfInput(n1), JerboaError::UnexpectedEndOfInput(n2)] => { 
                        assert_eq!(n1.to_string(), "e1");
                        assert_eq!(n2.to_string(), "e2");
                    },
                    _ => { assert!(false); },
                }
            },
            _ => { assert!(false); },
        }
    }

    #[test]
    fn should_transform_captures() {
        let m = Match::free(|_ : &char| true);
        let r = Rule::fixed("x", [m.clone(), m], move |captures| {
            Ok(captures.iter().map(|c| match c { 
                Capture::Item(x) => **x,
                _ => unreachable!(),
            }).collect::<String>())
        });

        let input = "abcd".chars().collect::<Vec<_>>();
        let output = parse(&input, &r.clone(), &[r]).unwrap();

        assert_eq!(output, ["ab", "cd"]);
    }

    #[test]
    fn should_indicate_error_for_unknown_rule() {
        let m = Match::rule("unknown");
        let r = Rule::fixed("x", [m], |_| Ok(1));

        let input = "abcd".chars().collect::<Vec<_>>();
        let output = parse(&input, &r.clone(), &[r]);

        if let JerboaError::RuleNotFound(name) = output.unwrap_err() {
            assert_eq!(name, "unknown".into());
        }
        else {
            assert!(false);
        }
    }

    #[test]
    fn should_parse_using_match_rule() {
        let m1 = Match::free(|a : &char| *a == 'a');
        let m2 : Match<char, u8> = Match::free(|a : &char| *a == 'b');
        let r1 = Rule::fixed("a", [m1.clone(), m1], |_| Ok(1));
        let r2 = Rule::fixed("baa", [m2, Match::rule("a")], |mut cs| Ok(cs.remove(1).unwrap_result().unwrap() + 2));

        let input = "baa".chars().collect::<Vec<_>>();
        let output = parse(&input, &r2.clone(), &[r1, r2]).unwrap();

        assert_eq!(output.len(), 1);
        assert_eq!(output[0], 3);
    }

    #[test]
    fn should_parse_using_option_match_rule() {
        let m1 = Match::free(|a : &char| *a == 'a');
        let m2 : Match<char, u8> = Match::free(|a : &char| *a == 'b');
        let r1 = Rule::fixed("a", [m1.clone(), m1], |_| Ok(1));
        let r2 = Rule::fixed("baa", [m2, Match::rule("a").option()], |mut cs| Ok(cs.remove(1).unwrap_option_result().unwrap().unwrap() + 2));

        let input = "baa".chars().collect::<Vec<_>>();
        let output = parse(&input, &r2.clone(), &[r1, r2]).unwrap();

        assert_eq!(output[0], 3);
    }

    #[test]
    fn should_parse_using_list_match_rule() {
        let m1 = Match::free(|a : &char| *a == 'a');
        let m2 : Match<char, u8> = Match::free(|a : &char| *a == 'b');
        let r1 = Rule::fixed("a", [m1.clone(), m1], |_| Ok(1));
        let r2 = Rule::fixed("baa", [m2, Match::rule("a").list()], |mut cs| Ok(cs.remove(1).unwrap_list_result().unwrap()[0] + 2));

        let input = "baaaa".chars().collect::<Vec<_>>();
        let output = parse(&input, &r2.clone(), &[r1, r2]).unwrap();

        assert_eq!(output[0], 3);
    }

    #[test]
    fn should_parse_using_match_rule_choice() {
        let m1 = Match::free(|a : &char| *a == 'a');
        let m2 : Match<char, u8> = Match::free(|a : &char| *a == 'b');
        let r1 = Rule::fixed("a", [m1.clone(), m1], |_| Ok(1));
        let r2 = Rule::fixed("baa", [m2, Match::rule("a")], |mut cs| Ok(cs.remove(1).unwrap_result().unwrap() + 2));
        let main = Rule::fixed("main", [Match::rule_choice(&["a", "baa"])], |mut cs| Ok(cs.remove(0).unwrap_result().unwrap()));

        let input = "baa".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[r1, r2]).unwrap();

        assert_eq!(output.len(), 1);
        assert_eq!(output[0], 3);
    }

    #[test]
    fn should_parse_using_option_match_rule_choice() {
        let ra = Rule::fixed("a", [Match::free(|a : &char| *a == 'a')], |_| Ok(1));
        let rb = Rule::fixed("b", [Match::free(|a : &char| *a == 'b')], |_| Ok(2));
        let main = Rule::fixed("main", [Match::rule_choice(&["a", "b"]).option()], |mut cs| Ok(cs.remove(0).unwrap_option_result().unwrap().unwrap()));

        let input = "b".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[ra, rb]).unwrap();

        assert_eq!(output.len(), 1);
        assert_eq!(output[0], 2);
    }

    #[test]
    fn should_parse_using_list_match_rule_choice() {
        let ra = Rule::fixed("a", [Match::free(|a : &char| *a == 'a')], |_| Ok(1));
        let rb = Rule::fixed("b", [Match::free(|a : &char| *a == 'b')], |_| Ok(2));
        let main = Rule::fixed("main", [Match::rule_choice(&["a", "b"]).list()], |mut cs| Ok(cs.remove(0).unwrap_list_result().unwrap().into_iter().sum()));

        let input = "bba".chars().collect::<Vec<_>>();
        let output = parse(&input, &main, &[ra, rb]).unwrap();

        assert_eq!(output.len(), 1);
        assert_eq!(output[0], 5);
    }
}
*/