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

pub trait GettingTiredOfOneVec<T>: Sized {
    fn pop_expect<F, E, R, E2>(self, f: F, err: R) -> Result<Either<T, E>, E2>
    where
        F: Fn(Self) -> E,
        R: Fn() -> Result<Either<T, E>, E2>;

    fn pop_maybe<F, E>(self, f: F) -> Either<T, E>
    where
        F: Fn(Self) -> E;

    fn has_single_item(&self) -> bool;
}

impl<T> GettingTiredOfOneVec<T> for Vec<T> {
    fn has_single_item(&self) -> bool {
        self.len() == 1
    }

    fn pop_expect<F, E, R, E2>(mut self, f: F, err: R) -> Result<Either<T, E>, E2>
    where
        F: Fn(Vec<T>) -> E,
        R: Fn() -> Result<Either<T, E>, E2>,
    {
        match self.len() {
            0 => err(),
            1 => Ok(Either::Left(self.pop().unwrap())),
            _ => Ok(Either::Right(f(self))),
        }
    }

    fn pop_maybe<F, E>(mut self, f: F) -> Either<T, E>
    where
        F: Fn(Self) -> E,
    {
        match self.len() {
            1 => Either::Left(self.pop().unwrap()),
            _ => Either::Right(f(self)),
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
