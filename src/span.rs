#[derive(Default, Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Position {
    pub(crate) line: u16,
    pub(crate) col: u16,
}

impl Position {
    pub const fn new(line: u16, col: u16) -> Self {
        Self { line, col }
    }

    pub const fn zero() -> Self {
        Self::new(u16::min_value(), u16::min_value())
    }

    pub const fn unknown() -> Self {
        Self::new(u16::max_value(), u16::max_value())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Span {
    pub(crate) start: Position,
    pub(crate) end: Position,
}

impl Span {
    pub const fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub const fn zero() -> Self {
        Self {
            start: Position::zero(),
            end: Position::zero(),
        }
    }

    pub const fn unknown() -> Self {
        Self {
            start: Position::unknown(),
            end: Position::unknown(),
        }
    }
}

impl std::ops::Add for Span {
    type Output = Span;
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            start: self.start,
            end: rhs.end,
        }
    }
}

impl std::ops::Add<Position> for Span {
    type Output = Span;
    fn add(self, rhs: Position) -> Self::Output {
        Self {
            start: self.start,
            end: rhs,
        }
    }
}

impl std::ops::AddAssign<Span> for Span {
    fn add_assign(&mut self, rhs: Span) {
        self.end = rhs.end
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Spanned<T> {
    pub span: Span,
    pub item: T,
}

impl<T> Spanned<T> {
    pub const fn new(item: T, span: Span) -> Self {
        Self { span, item }
    }

    pub fn map<S, F: FnMut(T) -> S>(self, mut f: F) -> Spanned<S> {
        Spanned {
            span: self.span,
            item: f(self.item),
        }
    }

    pub fn map_span<S, F: FnMut(T, Span) -> S>(self, mut f: F) -> Spanned<S> {
        Spanned {
            span: self.span,
            item: f(self.item, self.span),
        }
    }

    pub const fn zero(item: T) -> Self {
        Self {
            span: Span::zero(),
            item,
        }
    }
}

assert_size_of!(Span     => 8);
assert_size_of!(Position => 4);
