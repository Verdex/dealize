
pub enum MatchKind<'a, TSelf : Matchable> {
    Atom(&'a TSelf::Atom),
    Cons(&'a str, Vec<&'a TSelf>),
    List(&'a [TSelf]),
}

pub trait Matchable {
    type Atom : Clone + PartialEq;

    fn kind<'a>(&'a self) -> MatchKind<'a, Self> where Self : Sized;
}

#[derive(Debug, Clone)]
pub enum Pattern<TAtom : Clone> {
    Atom(TAtom),
    Wild,
    CaptureVar(Box<str>),
    Cons { name: Box<str>, params: Vec<Pattern<TAtom>> },
    // TODO cons variants?
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

pub struct Matches<'a, M, A : Clone> {
    matches : Vec<(Box<str>, &'a M)>,
    work : Vec<(Pattern<A>, &'a M)>,
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
                (Pattern::Wild, _) => { /* pass */ },
                (Pattern::Atom(p), MatchKind::Atom(d)) if &p == d => { /* pass */ },
                _ => { return None; }, // TODO this needs to be different when there are alternates
            } 
        }
        Some(self.matches.clone())
    }
}

pub fn find<'a, M : Matchable>(pattern : Pattern<M::Atom>, data : &'a M) -> Matches<'a, M, M::Atom> {
    Matches { matches : vec![], work : vec![(pattern, data)] }
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, PartialEq)]
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
        assert_eq!(results[0][0].0, "x".into());
        assert_eq!(results[0][0].1, &Data::A(8));
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
}