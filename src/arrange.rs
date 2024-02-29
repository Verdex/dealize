
#[derive(Debug)]
pub struct Bracket<T> {
    name : Box<str>,
    contents : Vec<T>, 
}

pub enum Matcher<T> {
    Pointer(fn(&T) -> bool),
    Boxed(Box<dyn FnMut(&T) -> bool>),
}

impl<T> Matcher<T> {
    pub fn call(&mut self, input : &T) -> bool {
        match self {
            Matcher::Pointer(f) => f(input),
            Matcher::Boxed(f) => f(input),
        }
    }
}

pub enum Transformer<T, S> {
    Pointer(fn(&T) -> S),
    Boxed(Box<dyn FnMut(&T) -> S>),
}

impl<T, S> Transformer<T, S> {
    pub fn call(&mut self, input : &T) -> S {
        match self {
            Transformer::Pointer(f) => f(input),
            Transformer::Boxed(f) => f(input),
        }
    }
}

pub fn parse<T, S>( input : &mut impl Iterator<Item = T>
                  , brackets : &[(&str, Matcher<T>, Matcher<T>)]
                  , rules : &[(&str, &[Matcher<Bracket<T>>], Transformer<Bracket<T>, S>)]
                  ) -> Result<Vec<S>, Box<dyn std::error::Error>>{ 

    todo!()
}
