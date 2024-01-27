

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
