
use std::rc::Rc;

#[derive(Debug)]
pub enum JerboaError {
    UnexpectedEndOfInput(Box<str>),
    RuleFailedToMatch(Box<str>),
    Multi(Vec<JerboaError>),
    Other(Box<dyn std::error::Error>),
    RuleNotFound(Box<str>),
}

impl std::fmt::Display for JerboaError {
    fn fmt(&self, f : &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            JerboaError::UnexpectedEndOfInput(n) => write!(f, "Unexpected end of input in rule: {}", n),
            JerboaError::RuleFailedToMatch(n) => write!(f, "Rule:  {} failed to match", n),
            JerboaError::Multi(errors) => write!(f, "Multiple Rule Failure {:?}", errors),
            JerboaError::Other(e) => write!(f, "Other Error Encountered {:?}", e),
            JerboaError::RuleNotFound(name) => write!(f, "Encountered Rule match for unknown rule: {}", name),
        }
    }
}

impl std::error::Error for JerboaError { }

pub enum Capture<'a, T, S> {
    Item(&'a T),
    Option(Option<&'a T>),
    List(Vec<&'a T>),
    RuleResult(S),
    OptionRuleResult(Option<S>),
    ListRuleResult(Vec<S>),
}

impl<'a, T, S> Capture<'a, T, S> {
    pub fn unwrap(self) -> Option<&'a T> {
        match self {
            Capture::Item(x) => Some(x),
            _ => None,
        }
    }
    pub fn unwrap_option(self) -> Option<Option<&'a T>> {
        match self {
            Capture::Option(x) => Some(x),
            _ => None,
        }
    }
    pub fn unwrap_list(self) -> Option<Vec<&'a T>> {
        match self {
            Capture::List(x) => Some(x),
            _ => None,
        }
    }
    pub fn unwrap_result(self) -> Option<S> {
        match self {
            Capture::RuleResult(x) => Some(x),
            _ => None,
        }
    }
    pub fn unwrap_option_result(self) -> Option<Option<S>> {
        match self {
            Capture::OptionRuleResult(x) => Some(x),
            _ => None,
        }
    }
    pub fn unwrap_list_result(self) -> Option<Vec<S>> {
        match self {
            Capture::ListRuleResult(x) => Some(x),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub enum MatchOpt {
    None,
    Option,
    List,
}

#[derive(Clone)]
pub enum Match<T, S> {
    Free(Rc<dyn Fn(&T) -> bool>, MatchOpt),
    Context(Rc<dyn for<'a> Fn(&T, &[Capture<'a, T, S>]) -> bool>, MatchOpt),
    Rule(Box<str>, MatchOpt),
    RuleChoice(Vec<Box<str>>, MatchOpt),
}

impl<T, S> Match<T, S> {
    pub fn free<F : Fn(&T) -> bool + 'static>( f : F ) -> Self {
        Match::Free(Rc::new(f), MatchOpt::None)
    }
    pub fn context<F : for<'a> Fn(&T, &[Capture<'a, T, S>]) -> bool + 'static>( f : F ) -> Self {
        Match::Context(Rc::new(f), MatchOpt::None)
    }
    pub fn rule<N : AsRef<str>>(name : N) -> Self {
        Match::Rule(name.as_ref().into(), MatchOpt::None)
    }
    pub fn rule_choice<N : AsRef<str>>(names : &[N]) -> Self {
        Match::RuleChoice(names.iter().map(|x| x.as_ref().into()).collect(), MatchOpt::None)
    }
    pub fn option(mut self) -> Self {
        match self {
            Match::Free(_, ref mut opt) => { *opt = MatchOpt::Option; },
            Match::Context(_, ref mut opt) => { *opt = MatchOpt::Option; },
            Match::Rule(_, ref mut opt) => { *opt = MatchOpt::Option; },
            Match::RuleChoice(_, ref mut opt) => { *opt = MatchOpt::Option },
        }
        self
    }
    pub fn list(mut self) -> Self {
        match self {
            Match::Free(_, ref mut opt) => { *opt = MatchOpt::List; },
            Match::Context(_, ref mut opt) => { *opt = MatchOpt::List; },
            Match::Rule(_, ref mut opt) => { *opt = MatchOpt::List; },
            Match::RuleChoice(_, ref mut opt) => { *opt = MatchOpt::List },
        }
        self
    }
}

#[derive(Clone)]
pub struct Rule<T, S> { 
    name : Box<str>,
    matches: Vec<Match<T, S>>,
    transform : Rc<dyn for<'a> Fn(Vec<Capture<'a, T, S>>) -> Result<S, JerboaError>>,
}

impl<T, S> Rule<T, S> {
    pub fn new<N : AsRef<str>, F : for<'a> Fn(Vec<Capture<'a, T, S>>) -> Result<S, JerboaError> + 'static>
    
        (name : N, matches : Vec<Match<T, S>>, transform : F) -> Self
        
    {
        Rule { name: name.as_ref().into(), matches, transform: Rc::new(transform) }
    }

    pub fn fixed<N : AsRef<str>, const RL : usize, F : for<'a> Fn(Vec<Capture<'a, T, S>>) -> Result<S, JerboaError> + 'static>
    
        (name : N, matches : [Match<T, S>; RL], transform : F) -> Self
        
    {
        Rule { name: name.as_ref().into(), matches: matches.into_iter().collect(), transform: Rc::new(transform) }
    }
}

pub fn parse<T, S>(input : &[T], rules: &[Rule<T, S>]) -> Result<Vec<S>, JerboaError> { 
    parse_with_lookup(input, rules, &[])
}

pub fn parse_with_lookup<T, S>(mut input : &[T], rules: &[Rule<T, S>], dictionary : &[Rule<T, S>]) -> Result<Vec<S>, JerboaError> { 
    let mut results = vec![];
    'outer : while !input.is_empty() {
        let mut errors = vec![];
        for rule in rules {
            match try_rule(input, rule, rules, dictionary) {
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

fn try_rule<'a, T, S>( mut input : &'a [T]
                     , rule : &Rule<T, S>
                     , rules : &[Rule<T, S>]
                     , dictionary : &[Rule<T, S>]

                     ) -> Result<(S, &'a [T]), JerboaError> {

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
            (_, Match::Rule(name, MatchOpt::None)) if has_rule(name, dictionary) => {
                let target_rule = get_rule(name, rules).unwrap();
                let (value, r) = try_rule(input, target_rule, rules, dictionary)?;
                captures.push(Capture::RuleResult(value));
                input = r;
            },
            /*(_, Match::RuleChoice(names, MatchOpt::None)) => {
                let mut errors = vec![];
                for name in names {
                    if !has_rule(name, dictionary) {
                        return Err(JerboaError::RuleNotFound(name.clone()));
                    }
                    match try_rule(input, , rules, dictionary) {
                        Ok((result, new_input)) => {
                            results.push(result);
                            input = new_input;
                            continue 'outer;
                        },
                        Err(e) => { errors.push(e); },
                    }
                }
                return Err(JerboaError::Multi(errors));
            },*/

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
            (_, Match::Rule(name, MatchOpt::Option)) if has_rule(name, dictionary) => {
                let target_rule = get_rule(name, rules).unwrap();
                match try_rule(input, target_rule, rules, dictionary) {
                    Ok((value, r)) => { 
                        captures.push(Capture::OptionRuleResult(Some(value)));
                        input = r;
                    },
                    Err(_) => {
                        captures.push(Capture::OptionRuleResult(None));
                    },
                }
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
            (_, Match::Rule(name, MatchOpt::List)) if has_rule(name, dictionary) => {
                let target_rule = get_rule(name, rules).unwrap();
                let mut local = vec![];
                loop {
                    match try_rule(input, target_rule, rules, dictionary) {
                        Ok((value, r)) => { 
                            local.push(value);
                            input = r;
                        },
                        Err(_) => {
                            break;
                        },
                    }
                }
                captures.push(Capture::ListRuleResult(local));
            },

            // Error
            (_, Match::Rule(name, _)) if !has_rule(name, dictionary) => {
                return Err(JerboaError::RuleNotFound(name.clone()));
            },
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

fn has_rule<T, S>(name : &str, rules : &[Rule<T, S>]) -> bool {
    let name : Box<str> = name.into();
    for r in rules {
        if r.name == name {
            return true;
        }
    } 
    false
}

fn get_rule<'a, T, S>(name : &str, rules : &'a [Rule<T, S>]) -> Option<&'a Rule<T, S>>{
    let name : Box<str> = name.into();
    for r in rules {
        if r.name == name {
            return Some(r);
        }
    } 
    None
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

    #[test]
    fn should_parse_list_rule() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_b = Match::free(|c : &char| *c == 'b');
        let match_c = Match::free(|c : &char| *c == 'c').list();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));

        let input = "acccb".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_list_rule_at_end() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_c = Match::free(|c : &char| *c == 'c').list();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));

        let input = "accc".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule]).unwrap();

        assert_eq!(output, [1]);
    }

    #[test]
    fn should_parse_list_rule_with_empty_list() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_b = Match::free(|c : &char| *c == 'b');
        let match_c = Match::free(|c : &char| *c == 'c').list();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));

        let input = "ab".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_list_rule_at_end_with_empty_list() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_c = Match::free(|c : &char| *c == 'c').list();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));

        let input = "a".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule]).unwrap();

        assert_eq!(output, [1]);
    }
    
    #[test]
    fn should_parse_option_rule() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_b = Match::free(|c : &char| *c == 'b');
        let match_c = Match::free(|c : &char| *c == 'c').option();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));

        let input = "acb".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_option_rule_at_end() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_c = Match::free(|c : &char| *c == 'c').option();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));

        let input = "ac".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule]).unwrap();

        assert_eq!(output, [1]);
    }

    #[test]
    fn should_parse_option_rule_with_nothing() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_b = Match::free(|c : &char| *c == 'b');
        let match_c = Match::free(|c : &char| *c == 'c').option();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));

        let input = "ab".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_option_rule_at_end_with_nothing() {
        let match_a = Match::free(|c : &char| *c == 'a');
        let match_c = Match::free(|c : &char| *c == 'c').option();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));

        let input = "a".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule]).unwrap();

        assert_eq!(output, [1]);
    }

    #[test]
    fn should_parse_empty_input_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let a_rule = Rule::new("a", vec![match_a], |_| Ok(1));
        let output = parse(&[], &[a_rule]).unwrap();

        assert!(output.is_empty());
    }

    #[test]
    fn should_parse_single_match_rule_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let a_rule = Rule::new("a", vec![match_a], |_| Ok(1));
        let input = "aaaa".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule]).unwrap();

        assert_eq!(output, [1, 1, 1, 1]);
    }

    #[test]
    fn should_parse_list_rule_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let match_b = Match::context(|c : &char, _| *c == 'b');
        let match_c = Match::context(|c : &char, _| *c == 'c').list();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));

        let input = "acccb".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_list_rule_at_end_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let match_c = Match::context(|c : &char, _| *c == 'c').list();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));

        let input = "accc".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule]).unwrap();

        assert_eq!(output, [1]);
    }

    #[test]
    fn should_parse_list_rule_with_empty_list_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let match_b = Match::context(|c : &char, _| *c == 'b');
        let match_c = Match::context(|c : &char, _| *c == 'c').list();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));

        let input = "ab".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_list_rule_at_end_with_empty_list_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let match_c = Match::context(|c : &char, _| *c == 'c').list();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));

        let input = "a".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule]).unwrap();

        assert_eq!(output, [1]);
    }
    
    #[test]
    fn should_parse_option_rule_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let match_b = Match::context(|c : &char, _| *c == 'b');
        let match_c = Match::context(|c : &char, _| *c == 'c').option();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));

        let input = "acb".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_option_rule_at_end_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let match_c = Match::context(|c : &char, _| *c == 'c').option();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));

        let input = "ac".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule]).unwrap();

        assert_eq!(output, [1]);
    }

    #[test]
    fn should_parse_option_rule_with_nothing_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let match_b = Match::context(|c : &char, _| *c == 'b');
        let match_c = Match::context(|c : &char, _| *c == 'c').option();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));
        let b_rule = Rule::new("b", vec![match_b], |_| Ok(2));

        let input = "ab".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule, b_rule]).unwrap();

        assert_eq!(output, [1, 2]);
    }

    #[test]
    fn should_parse_option_rule_at_end_with_nothing_with_context() {
        let match_a = Match::context(|c : &char, _| *c == 'a');
        let match_c = Match::context(|c : &char, _| *c == 'c').option();
        let a_rule = Rule::new("ac", vec![match_a, match_c], |_| Ok(1));

        let input = "a".chars().collect::<Vec<_>>();
        let output = parse(&input, &[a_rule]).unwrap();

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
        let r1 = Rule::fixed("add", [digit.clone(), digit.clone(), added.clone()], |_| Ok(1));
        let r2 = Rule::fixed("add", [digit.clone(), digit.clone(), digit, added], |_| Ok(2));

        let input = "1247123".chars().collect::<Vec<_>>();
        let output = parse(&input, &[r1, r2]).unwrap();

        assert_eq!(output, [2, 1]);
    }

    #[test]
    fn should_indicate_error_from_transformer() {
        let m = Match::free(|_ : &char| true);
        let r = Rule::fixed("error", [m], |_| -> Result<u8, _> { Err(JerboaError::Multi(vec![])) });

        let input = "a".chars().collect::<Vec<_>>();
        let output = parse(&input, &[r]);

        assert!(matches!(output, Err(JerboaError::Multi(_))));
    }

    #[test]
    fn should_indicate_error_from_no_rules_matching() {
        let m = Match::free(|_ : &char| false);
        let r = Rule::fixed("e1", [m.clone()], |_| -> Result<u8, _> { Err(JerboaError::Multi(vec![])) });
        let r2 = Rule::fixed("e2", [m], |_| -> Result<u8, _> { Err(JerboaError::Multi(vec![])) });

        let input = "a".chars().collect::<Vec<_>>();
        let output = parse(&input, &[r, r2]);

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

        let input = "a".chars().collect::<Vec<_>>();
        let output = parse(&input, &[r, r2]);

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
        let output = parse(&input, &[r]).unwrap();

        assert_eq!(output, ["ab", "cd"]);
    }

    #[test]
    fn should_indicate_error_for_unknown_rule() {
        let m = Match::rule("unknown");
        let r = Rule::fixed("x", [m], |_| Ok(1));

        let input = "abcd".chars().collect::<Vec<_>>();
        let output = parse(&input, &[r]);

        if let JerboaError::Multi(es) = output.unwrap_err() {
            assert!(matches!(es[0], JerboaError::RuleNotFound(_)));
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
        let output = parse_with_lookup(&input, &[r1.clone(), r2], &[r1]).unwrap();

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
        let output = parse_with_lookup(&input, &[r1.clone(), r2], &[r1]).unwrap();

        assert_eq!(output[0], 3);
    }

    #[test]
    fn should_parse_using_list_match_rule() {
        let m1 = Match::free(|a : &char| *a == 'a');
        let m2 : Match<char, u8> = Match::free(|a : &char| *a == 'b');
        let r1 = Rule::fixed("a", [m1.clone(), m1], |_| Ok(1));
        let r2 = Rule::fixed("baa", [m2, Match::rule("a").list()], |mut cs| Ok(cs.remove(1).unwrap_list_result().unwrap()[0] + 2));

        let input = "baaaa".chars().collect::<Vec<_>>();
        let output = parse_with_lookup(&input, &[r1.clone(), r2], &[r1]).unwrap();

        assert_eq!(output[0], 3);
    }
}