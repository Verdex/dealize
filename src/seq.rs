
#[macro_export]
macro_rules! easy_iter {
    ($item:ident: $($target:pat => $($e:ident),*);+) => {
        struct Z { index : usize }
        impl Iterator for Z {
            type Item = ;

            fn next(&mut self) -> Option<Self::Item> {
                match self.? { $( 

                    $target => { 
                        let mut x : usize = 0;
                        $(
                            if self.index == x {
                                self.index += 1;
                                return Some($e)
                            }
                            x =+ 1;
                        )* 
                    },

                )+ }
            }
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
        enum X {
            A(u8, u8, u8),
            B(u8, u8),
        }

        let x = X::A(1, 2, 7);
        let y = X::B(1, 4);
        println!("{:?}", easy_iter!(y: X::A(a, b, _) => a, b ; X::B(a, b) => a, b));
    }
}