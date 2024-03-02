
use std::rc::Rc;

pub enum Capture<'a, T> {
    Item(&'a T),
    Option(Option<&'a T>),
    List(&'a [T])
}

#[derive(Clone)]
pub enum Match<T> {
    Free(Rc<dyn Fn(&T) -> bool>),
    Context(Rc<dyn for<'a> Fn(&T, &[Capture<'a, T>]) -> bool>),
    Option(Box<Match<T>>),
    List(Box<Match<T>>),
}

pub struct Rule<T, S> { // TODO should fields be public or should there be some sort of constructor?
    matches: Vec<Match<T>>,
    transform : Rc<dyn for<'a> Fn(&[Capture<'a, T>]) -> S>,
}

pub fn parse<T, S>(input : &[T], rules: &[Rule<T, S>]) -> Result<Vec<S>, Box<dyn std::error::Error>> { // TODO error typet 
    for rule in rules {

    }
}
