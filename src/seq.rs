
pub struct Seq<T> {
    q : Vec<T>,
}

pub trait Seqy<'a> {
    fn l_next(&'a self) -> impl Iterator<Item = &'a Self>;

    fn to_seq(&'a self) -> Seq<&'a Self> {
        Seq { q : vec![ self ] }
    }
}