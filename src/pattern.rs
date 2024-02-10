
pub enum MatchKind<'a, TSelf : Matchable> {
    Atom(&'a TSelf::Atom),
    Cons(&'a str, Vec<&'a TSelf>),
    List(&'a [TSelf]),
}

pub trait Matchable : PartialEq {
    type Atom : Clone + PartialEq;

    fn kind<'a>(&'a self) -> MatchKind<'a, Self> where Self : Sized;
}

#[derive(Debug, Clone)]
pub enum Pattern<TAtom : Clone> {
    Atom(TAtom),
    Wild,
    CaptureVar(Box<str>),
    Cons(Box<str>, Vec<Pattern<TAtom>>),
    ExactList(Vec<Pattern<TAtom>>),
    ListPath(Vec<Pattern<TAtom>>),
    PathNext,
    Path(Vec<Pattern<TAtom>>),
    TemplateVar(Box<str>), 
    // TODO match with 
    // TODO and/or?
}

pub fn atom<T : Clone>(t : T) -> Pattern<T> {
    Pattern::Atom(t)
}

pub fn wild<T : Clone>() -> Pattern<T> {
    Pattern::Wild
}

pub fn capture<T : Clone, S : AsRef<str>>(name : S) -> Pattern<T> {
    Pattern::CaptureVar(name.as_ref().into())
}

pub fn exact_list<T : Clone>(patterns : &[Pattern<T>]) -> Pattern<T> {
    Pattern::ExactList(patterns.to_vec())
}

pub fn cons<T : Clone, S : AsRef<str>>(name : S, params : &[Pattern<T>]) -> Pattern<T> {
    Pattern::Cons(name.as_ref().into(), params.to_vec())
}

pub fn template<T : Clone, S : AsRef<str>>(name : S) -> Pattern<T> {
    Pattern::TemplateVar(name.as_ref().into())
}

pub fn list_path<T : Clone>(patterns : &[Pattern<T>]) -> Pattern<T> {
    Pattern::ListPath(patterns.to_vec())
}

pub fn path<T : Clone>(patterns : &[Pattern<T>]) -> Pattern<T> {
    Pattern::Path(patterns.to_vec())
}

pub fn next<T : Clone>() -> Pattern<T> {
    Pattern::PathNext
}

pub struct Matches<'a, M, A : Clone> {
    matches : Vec<(Box<str>, &'a M)>,
    work : Vec<(Pattern<A>, &'a M)>,
    nexts : Vec<&'a M>,
    alternatives: Vec<(Vec<(Box<str>, &'a M)>, Vec<(Pattern<A>, &'a M)>, Vec<&'a M>)>,
}

impl<'a, M, A : Clone> Matches<'a, M, A> {
    fn add_alt(&mut self, matches : Vec<(Box<str>, &'a M)>, work : Vec<(Pattern<A>, &'a M)>, nexts : Vec<&'a M>) {
        self.alternatives.push((matches, work, nexts));
    }

    fn switch_to_alt(&mut self) {
        let (matches, work, nexts) = self.alternatives.pop().unwrap();
        self.matches = matches;
        self.work = work;
        self.nexts = nexts;
    }
}

impl<'a, M : Matchable> Iterator for Matches<'a, M, M::Atom> {
    type Item = Vec<(Box<str>, &'a M)>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.work.len() == 0 && self.alternatives.len() == 0 { 
            return None;
        }
        if self.work.len() == 0 { 
            self.switch_to_alt();
        }
        while let Some((pattern, data)) = self.work.pop() {
            let data_kind = data.kind();
            match (pattern, data_kind) {
                (Pattern::CaptureVar(name), _) => { self.matches.push((name.clone(), data)); },
                (Pattern::TemplateVar(name), _) => {
                    let mut result = None;
                    for (k, v) in &self.matches {
                        if k == &name {
                            result = Some(*v);
                            break;
                        }
                    }
                    match result {
                        Some(v) if v == data => { /* pass */ },
                        Some(_) => { 
                            if self.alternatives.len() > 0 {
                                // TODO test
                                self.switch_to_alt();
                                continue;
                            }
                            else {
                                return None; 
                            }
                        }, 
                        None => { panic!("Used template variable with uncaptured name: {}", name); },
                    }
                },
                (Pattern::Wild, _) => { /* pass */ },
                (Pattern::Atom(p), MatchKind::Atom(d)) if &p == d => { /* pass */ },
                (Pattern::ExactList(ps), MatchKind::List(ds)) if ps.len() == ds.len() => {
                    for w in ps.into_iter().zip(ds.iter()).rev() {
                        self.work.push(w);
                    }
                },
                (Pattern::Cons(p_name, p_params), MatchKind::Cons(d_name, d_params)) 
                    if p_name == d_name.into() && p_params.len() == d_params.len() => {

                    for w in p_params.into_iter().zip(d_params.into_iter()).rev() {
                        self.work.push(w);
                    }
                },
                (Pattern::Path(ps), _) if ps.len() == 0 => { /* pass */ },
                (Pattern::Path(mut ps), _) => {
                    let mut results = find(ps.remove(0), data);

                    let result = results.next();
                    match result {
                        // Zero matches mean that this match fails.
                                // TODO test
                        None if self.alternatives.len() > 0 => { 
                            self.switch_to_alt();
                            continue;
                        },
                        None => {
                            return None;
                        },
                        Some(mut result) => {
                            self.matches.append(&mut result);

                            if results.nexts.len() != 0 {
                                let next_data = results.nexts.remove(0);

                                let next_pattern = Pattern::Path(ps);

                                for next in std::mem::replace(&mut results.nexts, vec![]) {
                                    let mut work = self.work.clone();
                                    work.push((next_pattern.clone(), next));
                                    self.add_alt(self.matches.clone(), work, vec![]);
                                }

                                self.work.push((next_pattern, next_data));
                                //self.alternatives.append(&mut results.alternatives); // TODO
                            }
                        },
                    }

                },
                (Pattern::PathNext, _) => {
                    self.nexts.push(data);
                },
                (Pattern::ListPath(ps), MatchKind::List(ds)) if ps.len() <= ds.len() => {
                    let p_len = ps.len();

                    for i in (1..=(ds.len() - p_len)).rev() {
                        let mut alt = self.work.clone();
                        let d_target = &ds[i..(i + p_len)];

                        for w in ps.clone().into_iter().zip(d_target.into_iter()).rev() {
                            alt.push(w);
                        }

                        self.add_alt(self.matches.clone(), alt, self.nexts.clone());
                    }

                    let d_target = &ds[0..p_len];
                    for w in ps.into_iter().zip(d_target.into_iter()).rev() {
                        self.work.push(w);
                    }
                },
                _ => {
                    if self.alternatives.len() > 0 {
                        // TODO test
                        self.switch_to_alt();
                        continue;
                    }
                    else {
                        return None; 
                    }
                }, 
            } 
        }
        Some(std::mem::replace(&mut self.matches, vec![]))
    }
}

pub fn find<'a, M : Matchable>(pattern : Pattern<M::Atom>, data : &'a M) -> Matches<'a, M, M::Atom> {
    Matches { matches : vec![]
            , work : vec![(pattern, data)]
            , alternatives: vec![] 
            , nexts : vec![]
            }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use super::*;

    #[derive(Debug, PartialEq, Clone)]
    enum Data {
        A(u8),
        ConsA(Box<Data>, Box<Data>),
        ConsB(Box<Data>),
        List(Vec<Data>),
    }

    impl Matchable for Data {
        type Atom = u8;
        fn kind<'a>(&'a self) -> MatchKind<'a, Self> where Self : Sized {
            match self { 
                Data::A(ref x) => MatchKind::Atom(x),
                Data::ConsA(a, b) => MatchKind::Cons("ConsA", vec![&**a, &**b]),
                Data::ConsB(a) => MatchKind::Cons("ConsB", vec![&**a]),
                Data::List(l) => MatchKind::List(&l[..]),
            }
        }
    }

    fn a(input : u8) -> Data {
        Data::A(input)
    }

    fn ca(a : Data, b : Data) -> Data {
        Data::ConsA(Box::new(a), Box::new(b))
    }

    fn cb(a : Data) -> Data {
        Data::ConsB(Box::new(a))
    }

    fn l<const N : usize>(input : [Data; N]) -> Data {
        Data::List(input.to_vec())
    }

    #[test]
    fn should_capture_single_atom() {
        let pattern = capture("x");
        let data = a(8);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(8));
    }
    
    #[test]
    fn should_find_single_atom() {
        let pattern = atom(8);
        let data = a(8);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 0);
    }

    #[test]
    fn should_find_wild() {
        let pattern = wild();
        let data = a(8);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 0);
    }

    #[test]
    fn should_find_exact_list() {
        let pattern = exact_list(&[atom(8), wild()]);
        let data = l([a(8), a(0)]);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 0);
    }

    #[test]
    fn should_capture_from_exact_list() {
        let pattern = exact_list(&[capture("x"), wild(), capture("y")]);
        let data = l([a(8), a(0), a(9)]);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 2);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(8));
        assert_eq!(*dict.get("y").unwrap(), &a(9));
    }

    #[test]
    fn should_find_cons() {
        let pattern = cons("ConsA", &[atom(8), wild()]);
        let data = ca(a(8), a(0));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 0);
    }

    #[test]
    fn should_capture_from_cons() {
        let pattern = cons("ConsA", &[atom(8), capture("x")]);
        let data = ca(a(8), a(0));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(0));
    }

    #[test]
    fn should_find_template_in_list() {
        let pattern = exact_list(&[capture("x"), template("x")]);
        let a = ca(a(8), a(0));
        let data = Data::List(vec![a.clone(), a.clone()]);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a);
    }

    #[test]
    fn should_find_template_in_cons() {
        let pattern = cons("ConsA", &[capture("x"), template("x")]);
        let data_a = ca(a(8), a(0));
        let data = ca(data_a.clone(), data_a.clone());
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &data_a);
    }

    #[test]
    fn should_find_multiple_list_paths() {
        let pattern = list_path(&[atom(0), atom(1)]);
        let data = l([a(0), a(1), a(0), a(1)]);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].len(), 0);
        assert_eq!(results[1].len(), 0);
    }

    #[test]
    fn should_capture_multiple_list_paths() {
        let pattern = list_path(&[atom(0), capture("x")]);
        let data = l([a(0), a(1), a(0), a(2)]);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(1));
        assert_eq!(results[1].len(), 1);
        let dict = results[1].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(2));
    }

    #[test]
    fn should_capture_list_path_next_to_list_path() {
        let pattern = cons("ConsA", &[list_path(&[capture("x")]), list_path(&[capture("y")])]);
        let data = ca(l([a(0), a(1)]), l([a(2), a(3)]));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 4);
        assert_eq!(results[0].len(), 2);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(0));
        assert_eq!(*dict.get("y").unwrap(), &a(2));

        assert_eq!(results[1].len(), 2);
        let dict = results[1].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(0));
        assert_eq!(*dict.get("y").unwrap(), &a(3));

        assert_eq!(results[2].len(), 2);
        let dict = results[2].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(1));
        assert_eq!(*dict.get("y").unwrap(), &a(2));

        assert_eq!(results[3].len(), 2);
        let dict = results[3].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(1));
        assert_eq!(*dict.get("y").unwrap(), &a(3));
    }

    #[test]
    fn should_capture_list_path_in_list_path() {
        let pattern = list_path(&[capture("x"), list_path(&[capture("y"), capture("z")])]);
        let data = l([a(0), l([a(1), a(2), a(3)]), l([a(4), a(5), a(6)])]);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 4);
        assert_eq!(results[0].len(), 3);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(0));
        assert_eq!(*dict.get("y").unwrap(), &a(1));
        assert_eq!(*dict.get("z").unwrap(), &a(2));

        assert_eq!(results[1].len(), 3);
        let dict = results[1].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(0));
        assert_eq!(*dict.get("y").unwrap(), &a(2));
        assert_eq!(*dict.get("z").unwrap(), &a(3));

        assert_eq!(results[2].len(), 3);
        let dict = results[2].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &l([a(1), a(2), a(3)]));
        assert_eq!(*dict.get("y").unwrap(), &a(4));
        assert_eq!(*dict.get("z").unwrap(), &a(5));

        assert_eq!(results[3].len(), 3);
        let dict = results[3].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &l([a(1), a(2), a(3)]));
        assert_eq!(*dict.get("y").unwrap(), &a(5));
        assert_eq!(*dict.get("z").unwrap(), &a(6));
    }

    #[test]
    fn should_not_find_on_non_matching_atom() {
        let pattern = atom(8);
        let data = a(1);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
    }

    #[test]
    fn should_not_find_on_non_matching_cons() {
        let pattern = cons("ConsA", &[wild(), wild()]);
        let data = cb(a(1));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
    }
    
    #[test]
    fn should_not_find_on_non_matching_cons_internal_length() {
        let pattern = cons("ConsA", &[wild()]);
        let data = ca(a(1), a(1));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
    }

    #[test]
    fn should_not_find_on_non_matching_cons_internals() {
        let pattern = cons("ConsA", &[wild(), atom(0)]);
        let data = ca(a(1), a(1));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
    }

    #[test]
    fn should_not_find_on_non_matching_list_length() {
        let pattern = exact_list(&[wild()]);
        let data = l([a(0), a(0)]);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
    }

    #[test]
    fn should_not_find_on_non_matching_list_internals() {
        let pattern = exact_list(&[wild(), atom(1)]);
        let data = l([a(0), a(0)]);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
    }

    #[test]
    fn should_not_find_on_non_matching_template() {
        let pattern = cons("ConsA", &[capture("x"), template("x")]);
        let data_a = ca(a(8), a(0));
        let data_b = ca(a(8), a(1));
        let data = ca(data_a, data_b);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
    }
}