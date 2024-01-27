
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
        while let Some((pattern, data)) = self.work.pop() {
            let data_kind = data.kind();
            match (pattern, data_kind) {
                (Pattern::Atom(a), MatchKind::Atom(b)) if &a == b => { /* pass */ },
                _ => { return None; },
            } 
        }
        Some(self.matches.clone())
    }
}

pub fn find<'a, M : Matchable>() -> Matches<'a, M, M::Atom> {
    Matches { matches : vec![], work : vec![] }
}

