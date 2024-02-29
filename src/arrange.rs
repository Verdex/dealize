
#[derive(Debug)]
pub struct Bracket<T> {
    name : Box<str>,
    contents : Vec<T>, 
}

pub enum Matcher<'a, T> {
    Pointer(fn(&T) -> bool),
    Ref(&'a mut dyn FnMut(&T) -> bool),
    Boxed(Box<dyn FnMut(&T) -> bool>),
}

impl<'a, T> Matcher<'a, T> {
    pub fn call(&mut self, input : &T) -> bool {
        match self {
            Matcher::Pointer(f) => f(input),
            Matcher::Boxed(f) => f(input),
            Matcher::Ref(f) => f(input),
        }
    }
}

pub enum Transformer<'a, T, S> {
    Pointer(fn(&T) -> S),
    Ref(&'a mut dyn FnMut(&T) -> S),
    Boxed(Box<dyn FnMut(&T) -> S>),
}

impl<'a, T, S> Transformer<'a, T, S> {
    pub fn call(&mut self, input : &T) -> S {
        match self {
            Transformer::Pointer(f) => f(input),
            Transformer::Boxed(f) => f(input),
            Transformer::Ref(f) => f(input),
        }
    }
}

pub struct BracketRule<'a, T> { 
    pub name : Box<str>,
    pub start : Matcher<'a, T>,
    pub end : Matcher<'a, T>,
}

pub struct TransformRule<'a, T, S> {
    pub name : Box<str>,
    pub pattern : Vec<Matcher<'a, Bracket<T>>>,
    pub transform : Transformer<'a, Bracket<T>, S>,
}

/*
impl From<a> for b { 
    fn from(value : a) -> Self {

    }
}

*/

pub fn parse<'a, T, S>( input : &mut impl Iterator<Item = T>
                  , brackets : &[BracketRule<T>]
                  , rules : &[TransformRule<'a, T, S>]
                  ) -> Result<Vec<S>, Box<dyn std::error::Error>>{ 

    todo!()
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn blarg() {
        let blarg : Result<Vec<u8>, _> = parse(&mut "blarg".chars(), &[], &[]);
    }
}