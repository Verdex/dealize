
#[macro_export]
macro_rules! easy_iter {
    ($item:ident: $t:ty = $($target:pat => $($e:expr),*);+) => {
        {
            struct Z<'a> { index : usize, item : &'a $t }
            impl<'a> Iterator for Z<'a> {
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
                        
                        _ => { return None; },
                    )+ }
                    None
                }
            } 
            Z { index: 0, item: &$item }
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

fn blargy() -> impl Iterator<Item = u8> {
    struct Z { }
    impl Iterator for Z {
        type Item = u8;
        fn next(&mut self) -> Option<Self::Item> { None }
    }

    Z{ }
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn blarg() {
        #[derive(Debug)]
        enum X {
            A(Box<X>, Box<X>),
            B(Box<X>),
            L(u8),
        }

        let x = X::A(Box::new(X::L(0)), Box::new(X::L(1)));

        let w = easy_iter!(x: X = X::A(a, b) => a, b ; X::B(a) => a);

        for ww in w {
            println!("{:?}", ww);
        }
    }
}