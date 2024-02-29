
#[derive(Debug)]
pub struct Bracket<T> {
    name : Box<str>,
    contents : Vec<T>, 
}

pub enum Matcher<T> {
    Pointer(fn(&T) -> bool),
    Boxed(Box<dyn FnMut(&T) -> bool>),
}


pub fn parse<T, S>( input : &mut impl Iterator<Item = T>
                  , brackets : &[(&str, fn(&T) -> bool, fn(&T) -> bool)]
                  , rules : &[(&str, &[fn(&Bracket<T>) -> bool], fn(&[Bracket<T>]) -> S)]
                  ) -> Result<Vec<S>, Box<dyn std::error::Error>>{ 

    todo!()
}
