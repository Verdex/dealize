
pub enum MatchKind<'a, TAtom, TSelf> {
    Atom(TAtom),
    Cons(&'a str, TSelf),
    List(TSelf),
}

pub trait Matchable {
    type Atom : Clone + PartialEq;

    fn kind<'a>(&'a self) -> MatchKind<'a, &'a Self::Atom, impl Iterator<Item = &'a Self>>;
}

#[derive(Debug, Clone)]
pub enum Pattern<TAtom : Clone> {
    Atom(TAtom),
    Wild,
    CaptureVar(Box<str>),
    Cons { name: Box<str>, params: Vec<Pattern<TAtom>> },
    ExactList(Vec<Pattern<TAtom>>),
    ListPath(Vec<Pattern<TAtom>>),
    PathNext,
    Path(Vec<Pattern<TAtom>>),
    TemplateVar(Box<str>), 
    // TODO match with 
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
                (Pattern::Atom(a), MatchKind::Atom(b)) if &a == b => { /* pass */ },
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
    use crate::unravel;

    enum Data {
        A(u8),
        ConsA(Box<Data>, Box<Data>),
        ConsB(Box<Data>),
        List(Vec<Data>),
    }

    impl Data {
        pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a Self> {
            unravel!(self: Data = Data::ConsA(a, b) => a, b ; Data::ConsB(a) => a)
        }
    }

    impl Matchable for Data {
        type Atom = u8;
        fn kind<'a>(&'a self) -> MatchKind<'a, &'a Self::Atom, impl Iterator<Item = &'a Self>> {
            match self { 
                Data::A(ref x) => MatchKind::Atom(x),
                x @ Data::ConsA(_, _) => MatchKind::Cons("ConsA", x.iter()),
                x @ Data::ConsB(_) => MatchKind::Cons("ConsB", x.iter()),
                _ => todo!(),
            }
        }
    }

    #[test]
    fn should_find_single_atom() {
        let pattern = Pattern::Atom(8);
        let data = Data::A(8);
        let results = find(pattern, &data).collect::<Vec<_>>();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].len(), 0);
    }
}