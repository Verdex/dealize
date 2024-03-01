
use std::iter::Peekable;

// TODO replace dyn error with actual error type
// Note:  Might not be able to use a real error type without enforcing a Display for T

#[derive(Debug)]
pub enum BracketItem<T> {
    Bracket(Bracket<T>),
    Item(T),
}

#[derive(Debug)]
pub struct Bracket<T> {
    name : Box<str>,
    content : Vec<BracketItem<T>>, 
}

pub struct BracketRule<'a, T> { 
    pub name : Box<str>,
    pub start : &'a dyn Fn(&T) -> bool,
    pub end : &'a dyn Fn(&T) -> bool,
}

pub struct TransformRule<'a, T, S> {
    pub name : Box<str>,
    pub pattern : Vec<&'a dyn Fn(&Bracket<T>) -> bool>,
    pub transform : &'a dyn Fn(&[Bracket<T>]) -> S,
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
                  ) -> Result<Vec<S>, ()>{ 

    let input = input.peekable();

    todo!()
}

fn bracket<T, I : Iterator<Item = T>>(input : &mut Peekable<I>, rules : &[BracketRule<T>]) -> Option<Result<Bracket<T>, ()>> {
    match input.next() {
        None => None,
        Some(v) => {
            for rule in rules {
                if (rule.start)(&v) {
                    return Some(sub_bracket(v, input, rule, rules));
                }
            }
            Some(Err(())) // TODO : Error no start rules found for v
        },
    }
}

fn sub_bracket<T, I : Iterator<Item = T>>(initial : T, input : &mut Peekable<I>, rule : &BracketRule<T>, rules : &[BracketRule<T>]) -> Result<Bracket<T>, ()> {
    let mut content = vec![BracketItem::Item(initial)];

    while let Some(v) = input.next_if(|x| !(rule.end)(x)) {
        match rules.iter().find(|r| (r.start)(&v)) {
            Some(r) => { content.push(BracketItem::Bracket(sub_bracket(v, input, r, rules)?)); },
            None => { content.push(BracketItem::Item(v)); },
        }
    }

    Ok(Bracket { name: rule.name.clone(), content })
}

/*
fn bracket<T>(input : &mut impl Iterator<Item = T>) -> Result<Bracket, Box<&dyn std::error::Error>> {
    match parse_bracket(&mut input)? {
        (None, ast) => Ok(ast),
        (Some(x), _) => todo!(), //Err(BracketError::NotAllInputConsumed(x.meta().start)),
    }
}

fn parse_bracket<T>(input : &mut impl Iterator<Item = T>) -> Result<(Option<Lexeme>, Vec<Bracket>), Box<&dyn std::error::Error>> {

    let mut ret = vec![];
    let end = loop {
        match input.next() {
            Some(Lexeme::LParen(m)) => {
                let item = parse_bracket(Type::Paren, m.start, input)?;
                ret.push(item);
            },
            Some(Lexeme::LAngle(m)) => {
                let item = parse_bracket(Type::Angle, m.start, input)?;
                ret.push(item);
            },
            Some(Lexeme::LCurl(m)) => {
                let item = parse_bracket(Type::Curl, m.start, input)?;
                ret.push(item);
            },
            Some(Lexeme::LSquare(m)) => {
                let item = parse_bracket(Type::Square, m.start, input)?;
                ret.push(item);
            },
            Some(x @ Lexeme::RParen(_)) => { break Some(x); },
            Some(x @ Lexeme::RAngle(_)) => { break Some(x); },
            Some(x @ Lexeme::RCurl(_)) => { break Some(x); },
            Some(x @ Lexeme::RSquare(_)) => { break Some(x); },
            Some(l) => { ret.push(Bracket::Lex(l)); },
            None => { break None; },
        }
    };

    Ok((end, ret))
}

fn parse_bracket(t : Type, initial : usize, input : &mut impl Iterator<Item = Lexeme>) -> Result<Bracket, BracketError> {
    fn to_expected(t : Type) -> char {
        match t {
            Type::Paren => ')',
            Type::Angle => '>',
            Type::Curl => '}',
            Type::Square => ']',
        }
    }

    let (end, contents) = parse_ast(input)?;
    match (t, end) {
        (Type::Paren, Some(Lexeme::RParen(m))) => Ok(Bracket::Paren(LMeta::multi(initial, m.end), contents)),
        (Type::Angle, Some(Lexeme::RAngle(m))) => Ok(Bracket::Angle(LMeta::multi(initial, m.end), contents)),
        (Type::Curl, Some(Lexeme::RCurl(m))) => Ok(Bracket::Curl(LMeta::multi(initial, m.end), contents)),
        (Type::Square, Some(Lexeme::RSquare(m))) => Ok(Bracket::Square(LMeta::multi(initial, m.end), contents)),
        (t, Some(l)) => {
            let found = l.value().chars().nth(0).unwrap();
            let terminal = l.meta().start;
            Err(BracketError::MissingEndBracket { initial, terminal, found, expected: to_expected(t) })
        },
        (t, None) => Err(BracketError::EofInsteadOfEndBracket { initial, expected: to_expected(t) }),
    }
}
*/

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn blarg() {
        let blarg : Result<Vec<u8>, _> = parse(&mut "blarg".chars(), &[], &[]);
    }
}