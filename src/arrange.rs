
#[derive(Debug)]
pub struct Bracket<T> {
    name : Box<str>,
    content : Vec<T>, 
}

pub struct BracketRule<'a, T> { 
    pub name : Box<str>,
    pub start : &'a mut dyn FnMut(&T) -> bool,
    pub end : &'a mut dyn FnMut(&T) -> bool,
}

pub struct TransformRule<'a, T, S> {
    pub name : Box<str>,
    pub pattern : Vec<&'a mut dyn FnMut(&Bracket<T>) -> bool>,
    pub transform : &'a mut dyn FnMut(&[Bracket<T>]) -> S,
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