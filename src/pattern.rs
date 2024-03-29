
use std::rc::Rc;

pub enum MatchKind<'a, TSelf : Matchable> {
    Atom(&'a TSelf::Atom),
    Cons(&'a str, Vec<&'a TSelf>),
    List(&'a [TSelf]),
}

pub trait Matchable : PartialEq {
    type Atom : Clone + PartialEq;

    fn kind<'a>(&'a self) -> MatchKind<'a, Self> where Self : Sized;
}

pub enum Pattern<T : Matchable> {
    Atom(T::Atom),
    Wild,
    CaptureVar(Box<str>),
    Cons(Box<str>, Vec<Pattern<T>>),
    ExactList(Vec<Pattern<T>>),
    ListPath(Vec<Pattern<T>>),
    PathNext,
    Path(Vec<Pattern<T>>),
    TemplateVar(Box<str>), 
    MatchWith(Rc<dyn Fn(&T) -> bool>),
    And(Box<Pattern<T>>, Box<Pattern<T>>),
    Or(Box<Pattern<T>>, Box<Pattern<T>>),
}

impl<T : Matchable<Atom = impl std::fmt::Debug>> std::fmt::Debug for Pattern<T> {
    fn fmt(&self, f : &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Pattern::Atom(x) => write!(f, "Pattern::Atom({:?})", x),
            Pattern::Wild => write!(f, "Pattern::Wild"),
            Pattern::CaptureVar(x) => write!(f, "Pattern::CaptureVar({:?})", x),
            Pattern::Cons(n, xs) => write!(f, "Pattern::Cons({}, {:?})", n, xs),
            Pattern::ExactList(xs) => write!(f, "Pattern::ExactList({:?})", xs),
            Pattern::ListPath(xs) => write!(f, "Pattern::ListPath({:?})", xs),
            Pattern::PathNext => write!(f, "Pattern::PathNext"),
            Pattern::Path(xs) => write!(f, "Pattern::Path({:?})", xs),
            Pattern::TemplateVar(x) => write!(f, "Pattern::TemplateVar({:?})", x),
            Pattern::MatchWith(_) => write!(f, "Pattern::MatchWith"),
            Pattern::And(a, b) => write!(f, "Pattern::And({:?}, {:?})", a, b),
            Pattern::Or(a, b) => write!(f, "Pattern::Or({:?}, {:?})", a, b),
        }
    }
}

impl<T : Matchable> Clone for Pattern<T> { 
    fn clone(&self) -> Self {
        match self {
            Pattern::Atom(x) => Pattern::Atom(x.clone()),
            Pattern::Wild => Pattern::Wild,
            Pattern::CaptureVar(x) => Pattern::CaptureVar(x.clone()),
            Pattern::Cons(n, xs) => Pattern::Cons(n.clone(), xs.clone()),
            Pattern::ExactList(xs) => Pattern::ExactList(xs.clone()),
            Pattern::ListPath(xs) => Pattern::ListPath(xs.clone()),
            Pattern::PathNext => Pattern::PathNext,
            Pattern::Path(xs) => Pattern::Path(xs.clone()),
            Pattern::TemplateVar(x) => Pattern::TemplateVar(x.clone()),
            Pattern::MatchWith(f) => Pattern::MatchWith(Rc::clone(&f)),
            Pattern::And(a, b) => Pattern::And(a.clone(), b.clone()),
            Pattern::Or(a, b) => Pattern::Or(a.clone(), b.clone()),
        }
    }
}

pub fn atom<T : Matchable>(t : T::Atom) -> Pattern<T> {
    Pattern::Atom(t)
}

pub fn wild<T : Matchable>() -> Pattern<T> {
    Pattern::Wild
}

pub fn capture<T : Matchable, S : AsRef<str>>(name : S) -> Pattern<T> {
    Pattern::CaptureVar(name.as_ref().into())
}

pub fn exact_list<T : Matchable>(patterns : &[Pattern<T>]) -> Pattern<T> {
    Pattern::ExactList(patterns.to_vec())
}

pub fn cons<T : Matchable, S : AsRef<str>>(name : S, params : &[Pattern<T>]) -> Pattern<T> {
    Pattern::Cons(name.as_ref().into(), params.to_vec())
}

pub fn template<T : Matchable, S : AsRef<str>>(name : S) -> Pattern<T> {
    Pattern::TemplateVar(name.as_ref().into())
}

pub fn list_path<T : Matchable>(patterns : &[Pattern<T>]) -> Pattern<T> {
    Pattern::ListPath(patterns.to_vec())
}

pub fn path<T : Matchable>(patterns : &[Pattern<T>]) -> Pattern<T> {
    Pattern::Path(patterns.to_vec())
}

pub fn next<T : Matchable>() -> Pattern<T> {
    Pattern::PathNext
}

pub fn match_with<T : Matchable>(f : impl Fn(&T) -> bool + 'static) -> Pattern<T> {
    Pattern::MatchWith(Rc::new(f))
}

pub fn and<T : Matchable>(a : Pattern<T>, b : Pattern<T>) -> Pattern<T> {
    Pattern::And(Box::new(a), Box::new(b))
}

pub fn or<T : Matchable>(a : Pattern<T>, b : Pattern<T>) -> Pattern<T> {
    Pattern::Or(Box::new(a), Box::new(b))
}

pub struct Matches<'a, M : Matchable> {
    matches : Vec<(Box<str>, &'a M)>,
    work : Vec<(Pattern<M>, &'a M)>,
    nexts : Vec<&'a M>,
    alternatives: Vec<(Vec<(Box<str>, &'a M)>, Vec<(Pattern<M>, &'a M)>, Vec<&'a M>)>,
}

impl<'a, M : Matchable> Matches<'a, M> {
    fn add_alt(&mut self, matches : Vec<(Box<str>, &'a M)>, work : Vec<(Pattern<M>, &'a M)>, nexts : Vec<&'a M>) {
        self.alternatives.push((matches, work, nexts));
    }

    fn switch_to_alt(&mut self) {
        let (matches, work, nexts) = self.alternatives.pop().unwrap();
        self.matches = matches;
        self.work = work;
        self.nexts = nexts;
    }
}

impl<'a, M : Matchable> Iterator for Matches<'a, M> {
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
                    let rest_work = self.work.clone();

                    let mut results = find(ps.remove(0), data);

                    // Note:  Take the existing matches and stuff it into the iterator
                    // so that it can be used by template variables.
                    // This only needs to happen before the first .next() because
                    // subsequent .next()'s self.switch_to_alt() method will 
                    // populate the matches field.
                    results.matches.append(&mut self.matches);

                    match results.next() {
                        // Zero matches mean that this match fails.
                        None if self.alternatives.len() > 0 => { 
                            self.switch_to_alt();
                            continue;
                        },
                        None => {
                            return None;
                        },
                        Some(mut result) => {
                            // Note:  Pull the matches out and attach them to the now empty self.matches
                            self.matches.append(&mut result);

                            if results.nexts.len() != 0 {
                                let next_data = results.nexts.remove(0);

                                let next_pattern = Pattern::Path(ps.clone());

                                for next in std::mem::replace(&mut results.nexts, vec![]) {
                                    let mut work = self.work.clone();
                                    work.push((next_pattern.clone(), next));
                                    self.add_alt(self.matches.clone(), work, vec![]);
                                }

                                self.work.push((next_pattern, next_data));
                            }
                        }
                    }

                    while let Some(matches) = results.next() {

                        if results.nexts.len() != 0 {
                            let next_pattern = Pattern::Path(ps.clone());

                            for next in std::mem::replace(&mut results.nexts, vec![]) {
                                let mut work = rest_work.clone();
                                work.push((next_pattern.clone(), next));
                                self.add_alt(matches.clone(), work, vec![]);
                            }
                        }
                        else {
                            self.add_alt(matches, vec![], vec![]);
                        }
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
                (Pattern::MatchWith(f), _) if f(data) => { /* pass */ },
                (Pattern::And(a, b), _) => {
                    self.work.push((*a, data));
                    self.work.push((*b, data));
                },
                (Pattern::Or(a, b), _) => {
                    let mut work = self.work.clone();
                    work.push((*b, data));
                    self.work.push((*a, data));
                    self.add_alt(self.matches.clone(), work, vec![]);
                },
                _ => {
                    if self.alternatives.len() > 0 {
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

pub fn find<'a, M : Matchable>(pattern : Pattern<M>, data : &'a M) -> Matches<'a, M> {
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
        ConsC(Box<Data>, Box<Data>, Box<Data>),
        List(Vec<Data>),
    }

    impl Matchable for Data {
        type Atom = u8;
        fn kind<'a>(&'a self) -> MatchKind<'a, Self> where Self : Sized {
            match self { 
                Data::A(ref x) => MatchKind::Atom(x),
                Data::ConsA(a, b) => MatchKind::Cons("ConsA", vec![&**a, &**b]),
                Data::ConsB(a) => MatchKind::Cons("ConsB", vec![&**a]),
                Data::ConsC(a, b, c) => MatchKind::Cons("ConsC", vec![&**a, &**b, &**c]),
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

    fn cc(a : Data, b : Data, c : Data) -> Data {
        Data::ConsC(Box::new(a), Box::new(b), Box::new(c))
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
    fn should_capture_path() {
        let pattern = path(&[cons("ConsA", &[next(), next()]), cons("ConsA", &[next(), next()]), capture("x")]);
        let data = ca(ca(a(0), a(1)), ca(a(2), a(3)));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 4);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(0));

        assert_eq!(results[1].len(), 1);
        let dict = results[1].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(1));

        assert_eq!(results[2].len(), 1);
        let dict = results[2].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(2));

        assert_eq!(results[3].len(), 1);
        let dict = results[3].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(3));
    }

    #[test]
    fn should_capture_path_in_initial_path() {
        let pattern = path(&[cons("ConsA", &[path(&[cons("ConsA", &[next(), next()]), capture("x")]), next()]), cons("ConsA", &[next(), next()]), capture("y")]);
        let data = ca(ca(a(0), a(1)), ca(a(2), a(3)));
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
    fn should_capture_path_in_final_path() {
        let pattern = path(&[cons("ConsA", &[next(), next()]), path(&[cons("ConsA", &[next(), next()]), capture("x")])]);
        let data = ca(ca(a(0), a(1)), ca(a(2), a(3)));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 4);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(0));

        assert_eq!(results[1].len(), 1);
        let dict = results[1].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(1));

        assert_eq!(results[2].len(), 1);
        let dict = results[2].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(2));

        assert_eq!(results[3].len(), 1);
        let dict = results[3].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(3));
    }

    #[test]
    fn should_capture_path_in_list_path() {
        let pattern = list_path(&[ path(&[cons("ConsC", &[next(), atom(0), next()]), capture("x")])
                                 , path(&[cons("ConsC", &[next(), atom(1), next()]), capture("y")])
                                 ]);
        let data = l([ cc(a(10), a(0), a(11))
                     , cc(a(20), a(1), a(22)) 
                     , a(50)
                     , cc(a(30), a(0), a(32)) 
                     , cc(a(40), a(0), a(42)) 
                     , cc(a(50), a(1), a(52)) 
                     ]); 
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 8);
        assert_eq!(results[0].len(), 2);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(10));
        assert_eq!(*dict.get("y").unwrap(), &a(20));

        assert_eq!(results[1].len(), 2);
        let dict = results[1].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(10));
        assert_eq!(*dict.get("y").unwrap(), &a(22));

        assert_eq!(results[2].len(), 2);
        let dict = results[2].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(11));
        assert_eq!(*dict.get("y").unwrap(), &a(20));

        assert_eq!(results[3].len(), 2);
        let dict = results[3].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(11));
        assert_eq!(*dict.get("y").unwrap(), &a(22));

        assert_eq!(results[4].len(), 2);
        let dict = results[4].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(40));
        assert_eq!(*dict.get("y").unwrap(), &a(50));

        assert_eq!(results[5].len(), 2);
        let dict = results[5].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(40));
        assert_eq!(*dict.get("y").unwrap(), &a(52));

        assert_eq!(results[6].len(), 2);
        let dict = results[6].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(42));
        assert_eq!(*dict.get("y").unwrap(), &a(50));
        
        assert_eq!(results[7].len(), 2);
        let dict = results[7].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(42));
        assert_eq!(*dict.get("y").unwrap(), &a(52));
    }

    #[test]
    fn should_capture_path_with_initial_capture_in_list_path() {
        let pattern = list_path(&[ path(&[cons("ConsC", &[next(), capture("z"), next()]), capture("x")])
                                 , path(&[cons("ConsC", &[next(), atom(1), next()]), capture("y")])
                                 ]);
        let data = l([ cc(a(10), a(90), a(11))
                     , cc(a(20), a(1), a(22)) 
                     , a(50)
                     , cc(a(30), a(0), a(32)) 
                     , cc(a(40), a(80), a(42)) 
                     , cc(a(50), a(1), a(52)) 
                     ]); 
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 8);
        assert_eq!(results[0].len(), 3);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(10));
        assert_eq!(*dict.get("y").unwrap(), &a(20));
        assert_eq!(*dict.get("z").unwrap(), &a(90));

        assert_eq!(results[1].len(), 3);
        let dict = results[1].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(10));
        assert_eq!(*dict.get("y").unwrap(), &a(22));
        assert_eq!(*dict.get("z").unwrap(), &a(90));

        assert_eq!(results[2].len(), 3);
        let dict = results[2].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(11));
        assert_eq!(*dict.get("y").unwrap(), &a(20));
        assert_eq!(*dict.get("z").unwrap(), &a(90));

        assert_eq!(results[3].len(), 3);
        let dict = results[3].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(11));
        assert_eq!(*dict.get("y").unwrap(), &a(22));
        assert_eq!(*dict.get("z").unwrap(), &a(90));

        assert_eq!(results[4].len(), 3);
        let dict = results[4].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(40));
        assert_eq!(*dict.get("y").unwrap(), &a(50));
        assert_eq!(*dict.get("z").unwrap(), &a(80));

        assert_eq!(results[5].len(), 3);
        let dict = results[5].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(40));
        assert_eq!(*dict.get("y").unwrap(), &a(52));
        assert_eq!(*dict.get("z").unwrap(), &a(80));

        assert_eq!(results[6].len(), 3);
        let dict = results[6].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(42));
        assert_eq!(*dict.get("y").unwrap(), &a(50));
        assert_eq!(*dict.get("z").unwrap(), &a(80));
        
        assert_eq!(results[7].len(), 3);
        let dict = results[7].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(42));
        assert_eq!(*dict.get("y").unwrap(), &a(52));
        assert_eq!(*dict.get("z").unwrap(), &a(80));
    }

    #[test]
    fn should_capture_list_path_in_path() {
        let pattern = path(&[cons("ConsA", &[capture("x"), list_path(&[next(), capture("y")])]), atom(0)]);
        let data = ca(a(99), l([a(0), a(1), a(0), a(2)]));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].len(), 2);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(99));
        assert_eq!(*dict.get("y").unwrap(), &a(1));

        assert_eq!(results[1].len(), 2);
        let dict = results[1].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(99));
        assert_eq!(*dict.get("y").unwrap(), &a(2));
    }

    #[test]
    fn should_capture_list_path_in_path_with_template() {
        let pattern = cons("ConsA", &[capture("w"), path(&[list_path(&[capture("x"), next()]), template("w")])]);
        let data = ca(a(77), l([a(0), a(77), cb(a(0)), cb(a(1)), a(77)]));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].len(), 2);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(0));
        assert_eq!(*dict.get("w").unwrap(), &a(77));

        assert_eq!(results[1].len(), 2);
        let dict = results[1].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &cb(a(1)));
        assert_eq!(*dict.get("w").unwrap(), &a(77));
    }

    #[test]
    fn should_capture_list_path_with_template() {
        let pattern = list_path(&[capture("x"), template("x")]);
        let data = l([a(0), a(0), l([]), l([])]);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(0));

        assert_eq!(results[1].len(), 1);
        let dict = results[1].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &l([]));
    }

    #[test]
    fn should_capture_path_with_template() {
        let pattern = path(&[ cons("ConsC", &[capture("x"), next(), next()])
                            , template("x")
                            ]);
        let data = cc(a(0), a(0), a(0));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(0));

        assert_eq!(results[1].len(), 1);
        let dict = results[1].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(0));
    }

    #[test]
    fn should_capture_from_first_or() {
        let pattern = or(cons("ConsA", &[capture("x"), atom(0)]), atom(9));
        let data = ca(a(1), a(0));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(1));
    }
    
    #[test]
    fn should_capture_from_second_or() {
        let pattern = or(atom(9), cons("ConsA", &[capture("x"), atom(0)]));
        let data = ca(a(1), a(0));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(1));
    }

    #[test]
    fn should_capture_from_both_or() {
        let pattern = or(cons("ConsA", &[atom(1), capture("x")]), cons("ConsA", &[capture("x"), atom(0)]));
        let data = ca(a(1), a(0));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(0));

        assert_eq!(results[1].len(), 1);
        let dict = results[1].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(1));
    }

    #[test]
    fn should_capture_both_and() {
        let pattern = and(capture("x"), capture("y"));
        let data = a(0);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 2);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(0));
        assert_eq!(*dict.get("y").unwrap(), &a(0));
    }

    #[test]
    fn should_find_with_match_with() {
        let pattern = list_path(&[match_with(|x| match x { Data::A(x) => x % 2 == 0, _ => false }), capture("x")]);
        let data = l([cb(a(0)), a(2), a(3), a(4), cb(a(0))]);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(3));

        assert_eq!(results[1].len(), 1);
        let dict = results[1].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &cb(a(0)));
    }

    #[test]
    fn should_find_with_first_or() {
        let pattern = or(atom(0), atom(1));
        let data = a(0);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
    }

    #[test]
    fn should_find_with_second_or() {
        let pattern = or(atom(0), atom(1));
        let data = a(1);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
    }

    #[test]
    fn should_not_find_with_or() {
        let pattern = or(atom(0), atom(1));
        let data = a(2);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
    }

    #[test]
    fn should_find_with_and() {
        let pattern = and(wild(), atom(0));
        let data = a(0);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
    }

    #[test]
    fn should_not_find_with_and_when_first_fails() {
        let pattern = and(atom(1), atom(0));
        let data = a(0);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
    }

    #[test]
    fn should_not_find_with_and_when_second_fails() {
        let pattern = and(atom(1), atom(0));
        let data = a(1);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
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

    #[test]
    fn should_not_capture_failing_final_path() {
        let pattern = path(&[cons("ConsA", &[next(), next()]), cons("ConsA", &[next(), atom(1)]), capture("x")]);
        let data = ca(ca(a(0), a(1)), ca(a(2), a(3)));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(0));
    }

    #[test]
    fn should_not_capture_failing_initial_path() {
        let pattern = path(&[cons("ConsA", &[next(), cons("ConsA", &[wild(), atom(3)])]), cons("ConsA", &[next(), next()]), capture("x")]);
        let data = ca(ca(a(0), a(1)), ca(a(2), a(3)));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 2);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(0));
        assert_eq!(results[1].len(), 1);
        let dict = results[1].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a(1));
    }
}