
use std::rc::Rc;

pub enum Capture<'a, T> {
    Item(&'a T),
    Option(Option<&'a T>),
    List(&'a [T])
}

#[derive(Clone)]
pub enum Rule<T> {
    Free(Rc<dyn Fn(&T) -> bool>),
    Context(Rc<dyn for<'a> Fn(&T, &[Capture<'a, T>]) -> bool>),
    Option(Box<Rule<T>>),
    List(Box<Rule<T>>),
}

pub struct Parser<T> {
    rule : Rule<T>,
    transform : fn,
}

#[derive(Clone)]
pub enum Blarg<'a, T> {
    Blargy(&'a dyn Fn(&T) -> bool),
}

pub fn parse<T>(input : &[T], ) {

}

/*pub enum Parser<'a, I, O, E> {

}
*/
