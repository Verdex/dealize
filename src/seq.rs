

pub struct Seq<T> {
    q : Vec<T>,
}

pub trait Seqable<'a> {
    fn seq_next(&'a self) -> Vec<&'a Self>;

    fn to_seq(&'a self) -> Seq<&'a Self> {
        Seq { q : vec![ self ] }
    }
}

impl<'a, T> Iterator for Seq<&'a T> where T : Seqable<'a> {
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

    impl<'a> Seqable<'a> for X {
        fn seq_next(&'a self) -> Vec<&'a X> {
            match self {
                X::A(a, b) => vec![a, b],
                X::B(a) => vec![a],
                X::L(_) => vec![],
            }
        }
    }

    #[test]
    fn should_sequence_seqable() {
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
}