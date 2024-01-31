
#[macro_export]
macro_rules! unravel {
    ($item:ident : $t:ty = $($target:pat => $($e:expr),*);+) => {
        {
            use std::borrow::Borrow;

            struct Unraveler<'a> { index : usize, item : &'a $t }
            impl<'a> Iterator for Unraveler<'a> {
                type Item = &'a $t;

                #[allow(unused_assignments)]
                fn next(&mut self) -> Option<Self::Item> {

                    match self.item { 

                        $( 

                        $target => { 

                            let mut x : usize = 0;
                            $(
                                if self.index == x {
                                    self.index += 1;
                                    return Some($e);
                                }
                                x += 1;
                            )* 
                        },

                        )+    
                        _ => { return None; },
                    }
                    None
                }
            } 
            Unraveler { index: 0, item: $item.borrow() }
        }
    };
}

pub struct Seq<T> {
    q : Vec<T>,
}

pub trait Seqy<'a> {
    fn seq_next(&'a self) -> impl Iterator<Item = &'a Self>;

    fn to_seq(&'a self) -> Seq<&'a Self> {
        Seq { q : vec![ self ] }
    }
}

impl<'a, T> Iterator for Seq<&'a T> where T : Seqy<'a> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.q.pop() {
            Some(x) => {
                let nexts = x.seq_next();
                for w in nexts {
                    self.q.push(w);
                }
                Some(x) 
            },
            None => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, PartialEq)]
    enum X {
        A(Box<X>, Box<X>),
        B(Box<X>),
        L(u8),
    }

    fn a(x : X, y : X) -> X {
        X::A(Box::new(x), Box::new(y))
    }

    fn b(x : X) -> X {
        X::B(Box::new(x))
    }

    fn l(x : u8) -> X {
        X::L(x)
    }

    impl<'a> Seqy<'a> for X {
        fn seq_next(&'a self) -> impl Iterator<Item = &'a Self> {
            unravel!(self: X = X::A(a, b) => a, b ; X::B(a) => a)
        }
    }

    #[test]
    fn should_sequence_seqy() {
        let input = a(a(b(l(1)), l(2)), b(l(3)));
        let output = input.to_seq().collect::<Vec<_>>();
        assert_eq!(output.len(), 7);
        assert_eq!(output[0], &a(a(b(l(1)), l(2)), b(l(3))));
        assert_eq!(output[1], &b(l(3)));
        assert_eq!(output[2], &l(3));
        assert_eq!(output[3], &a(b(l(1)), l(2)));
        assert_eq!(output[4], &l(2));
        assert_eq!(output[5], &b(l(1)));
        assert_eq!(output[6], &l(1));
    }

    #[test]
    fn should_unravel() {
        let x = X::A(Box::new(X::L(0)), Box::new(X::B(Box::new(X::L(1)))));
        let is = unravel!(x: X = X::A(a, b) => a, b ; X::B(a) => a).collect::<Vec<_>>();
        assert_eq!(is.len(), 2);
        assert_eq!(is[0], &X::L(0));
        assert_eq!(is[1], &X::B(Box::new(X::L(1))));

        let y = is[1];

        let is = unravel!(y: X = X::A(a, b) => a, b ; X::B(a) => a).collect::<Vec<_>>();
        assert_eq!(is.len(), 1);
        assert_eq!(is[0], &X::L(1));
    }

    #[test]
    fn should_unravel_variable_length_constructor() {
        // W(W, Vec<W>)
        assert!(false);

    }
}