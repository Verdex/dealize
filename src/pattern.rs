
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

pub struct Matches<'a, M, A : Clone> {
    matches : Vec<(Box<str>, &'a M)>,
    work : Vec<(Pattern<A>, &'a M)>,
    alternates : Vec<Matches<'a, M, A>>,
}

impl<'a, M, A : Clone> Clone for Matches<'a, M, A> {
    fn clone(&self) -> Self {
        Matches { matches: self.matches.clone()
                , work: self.work.clone()
                , alternates: self.alternates.clone()
                }
    }
}

impl<'a, M : Matchable> Iterator for Matches<'a, M, M::Atom> {
    type Item = Vec<(Box<str>, &'a M)>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.work.len() == 0 {  // TODO ?
            return None;
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
                        Some(_) => { return None; }, // TODO this needs to be different when there are alternatives
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
                (Pattern::ListPath(ps), MatchKind::List(ds)) if ps.len() <= ds.len() => {
                    let p_len = ps.len();

                    let mut alts = vec![];
                    for i in (1..=(ds.len() - p_len)).rev() {
                        let mut alt = self.clone();
                        let d_target = &ds[i..(i + p_len)];

                        for w in ps.clone().into_iter().zip(d_target.into_iter()).rev() {
                            alt.work.push(w);
                        }

                        alts.push(alt);
                    }
                    self.alternates.append(&mut alts);

                    let d_target = &ds[0..p_len];
                    for w in ps.into_iter().zip(d_target.into_iter()).rev() {
                        self.work.push(w);
                    }
                },
                _ => { return None; }, // TODO this needs to be different when there are alternates
            } 
        }
        Some(std::mem::replace(&mut self.matches, vec![]))
    }
}

pub fn find<'a, M : Matchable>(pattern : Pattern<M::Atom>, data : &'a M) -> Matches<'a, M, M::Atom> {
    Matches { matches : vec![], work : vec![(pattern, data)], alternates: vec![] }
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

    #[test]
    fn should_capture_single_atom() {
        let pattern = capture("x");
        let data = Data::A(8);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &Data::A(8));
    }
    
    #[test]
    fn should_find_single_atom() {
        let pattern = atom(8);
        let data = Data::A(8);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 0);
    }

    #[test]
    fn should_find_wild() {
        let pattern = wild();
        let data = Data::A(8);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 0);
    }

    #[test]
    fn should_find_exact_list() {
        let pattern = exact_list(&[atom(8), wild()]);
        let data = Data::List(vec![Data::A(8), Data::A(0)]);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 0);
    }

    #[test]
    fn should_capture_from_exact_list() {
        let pattern = exact_list(&[capture("x"), wild(), capture("y")]);
        let data = Data::List(vec![Data::A(8), Data::A(0), Data::A(9)]);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 2);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &Data::A(8));
        assert_eq!(*dict.get("y").unwrap(), &Data::A(9));
    }

    #[test]
    fn should_find_cons() {
        let pattern = cons("ConsA", &[atom(8), wild()]);
        let data = Data::ConsA(Box::new(Data::A(8)), Box::new(Data::A(0)));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 0);
    }

    #[test]
    fn should_capture_from_cons() {
        let pattern = cons("ConsA", &[atom(8), capture("x")]);
        let data = Data::ConsA(Box::new(Data::A(8)), Box::new(Data::A(0)));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &Data::A(0));
    }

    #[test]
    fn should_find_template_in_list() {
        let pattern = exact_list(&[capture("x"), template("x")]);
        let a = Data::ConsA(Box::new(Data::A(8)), Box::new(Data::A(0)));
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
        let a = Data::ConsA(Box::new(Data::A(8)), Box::new(Data::A(0)));
        let data = Data::ConsA(Box::new(a.clone()), Box::new(a.clone()));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 1);
        let dict = results[0].clone().into_iter().collect::<HashMap<Box<str>, &Data>>();
        assert_eq!(*dict.get("x").unwrap(), &a);
    }

    #[test]
    fn should_not_find_on_non_matching_atom() {
        let pattern = atom(8);
        let data = Data::A(1);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
    }

    #[test]
    fn should_not_find_on_non_matching_cons() {
        let pattern = cons("ConsA", &[wild(), wild()]);
        let data = Data::ConsB(Box::new(Data::A(1)));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
    }
    
    #[test]
    fn should_not_find_on_non_matching_cons_internal_length() {
        let pattern = cons("ConsA", &[wild()]);
        let data = Data::ConsA(Box::new(Data::A(1)), Box::new(Data::A(1)));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
    }

    #[test]
    fn should_not_find_on_non_matching_cons_internals() {
        let pattern = cons("ConsA", &[wild(), atom(0)]);
        let data = Data::ConsA(Box::new(Data::A(1)), Box::new(Data::A(1)));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
    }

    #[test]
    fn should_not_find_on_non_matching_list_length() {
        let pattern = exact_list(&[wild()]);
        let data = Data::List(vec![Data::A(0), Data::A(0)]);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
    }

    #[test]
    fn should_not_find_on_non_matching_list_internals() {
        let pattern = exact_list(&[wild(), atom(1)]);
        let data = Data::List(vec![Data::A(0), Data::A(0)]);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
    }

    #[test]
    fn should_not_find_on_non_matching_template() {
        let pattern = cons("ConsA", &[capture("x"), template("x")]);
        let a = Data::ConsA(Box::new(Data::A(8)), Box::new(Data::A(0)));
        let b = Data::ConsA(Box::new(Data::A(8)), Box::new(Data::A(1)));
        let data = Data::ConsA(Box::new(a), Box::new(b));
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 0);
    }
}