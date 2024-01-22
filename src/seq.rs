
#[macro_export]
macro_rules! unravel {
    ($item:ident : $t:ty = $($target:pat => $($e:expr),*);+) => {
        {
            use std::borrow::Borrow;

            #[derive(Debug)]
            struct Unraveler<'a> { index : usize, item : &'a $t }
            impl<'a> Iterator for Unraveler<'a> {
                type Item = &'a $t;

                fn next(&mut self) -> Option<Self::Item> {
                    match self.item { $( 

                        $target => { 
                            let mut x : usize = 0;
                            $(
                                if self.index == x {
                                    self.index += 1;
                                    return Some($e)
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

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, PartialEq)]
    enum X {
        A(Box<X>, Box<X>),
        B(Box<X>),
        L(u8),
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
}