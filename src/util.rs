use crate::stack::Stack;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L, R> Either<L, R> {
    pub fn map_left<F: Fn(L) -> E, E>(self, f: F) -> Either<E, R> {
        match self {
            Self::Left(left) => Either::Left(f(left)),
            Self::Right(right) => Either::Right(right),
        }
    }

    pub fn map_right<F: Fn(R) -> E, E>(self, f: F) -> Either<L, E> {
        match self {
            Self::Left(left) => Either::Left(left),
            Self::Right(right) => Either::Right(f(right)),
        }
    }
}

impl<T> Either<T, T> {
    pub fn factor(self) -> T {
        match self {
            Self::Left(d) | Self::Right(d) => d,
        }
    }
}

pub trait GimmeABox: Sized {
    fn boxed(self) -> Box<Self>;
}

impl<T> GimmeABox for T {
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

pub enum MaybeVec<T> {
    One(Option<T>),
    Many(Stack<T>),
}

impl<T> MaybeVec<T> {
    pub fn is_one(&self) -> bool {
        matches!(self, Self::One(Some(..)))
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        match self {
            MaybeVec::One(Some(..)) => 1,
            MaybeVec::One(None) => 0,
            MaybeVec::Many(v) => v.len(),
        }
    }

    pub const fn empty() -> Self {
        Self::One(None)
    }

    pub const fn one(item: T) -> Self {
        Self::One(Some(item))
    }

    pub fn many(list: Vec<T>) -> Self {
        Self::Many(list.into())
    }

    pub fn expect_pop(self) -> T {
        self.into_iter().next().unwrap()
    }

    pub fn into_vec(self) -> Vec<T> {
        match self {
            MaybeVec::One(d) => d.into_iter().collect(),
            MaybeVec::Many(v) => v.into_inner(),
        }
    }

    pub fn push(&mut self, item: T) {
        match self {
            Self::One(ref mut opt @ Some(..)) => {
                let list = vec![opt.take().unwrap(), item];
                let _ = std::mem::replace(self, Self::many(list));
            }

            Self::One(None) => {
                let _ = std::mem::replace(self, Self::one(item));
            }

            Self::Many(v) => v.push(item),
        }
    }
}

// impl<T> AsRef<[T]> for MaybeVec<T> {
//     fn as_ref(&self) -> &[T] {
//         match self {
//             Self::One(Some(d)) => std::slice::from_ref(d),
//             Self::One(None) => &[],
//             Self::Many(v) => v.as_slice(),
//         }
//     }
// }

impl<T> IntoIterator for MaybeVec<T> {
    type Item = T;
    type IntoIter = MaybeVecIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter { maybe_list: self }
    }
}

pub struct MaybeVecIter<T> {
    maybe_list: MaybeVec<T>,
}

impl<T> Iterator for MaybeVecIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        match self.maybe_list {
            MaybeVec::One(ref mut d) => d.take(),
            MaybeVec::Many(ref mut v) => v.pop(),
        }
    }
}

pub fn count_digits(d: u64) -> usize {
    let (mut len, mut n) = (1, 1u64);
    while len < 20 {
        n *= 10;
        if n > d {
            return len;
        }
        len += 1;
    }
    len
}

macro_rules! assert_size_of {
    ($ty:ty => $size:expr) => {
        const _: [(); 0] = [(); !(std::mem::size_of::<$ty>() != $size) as usize - 1];
    };
}
