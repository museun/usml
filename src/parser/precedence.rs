#[derive(Debug)]
pub enum Error {
    InfixInPrefix,
    EndsWithInfix,
    SamePrecedence,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InfixInPrefix => f.write_str("expression begins with infix operator"),
            Self::EndsWithInfix => f.write_str("expression ends with an infix operator"),
            Self::SamePrecedence => f.write_str("operators have the same precedence"),
        }
    }
}

impl std::error::Error for Error {}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Fixity {
    Infix { left: u8, right: u8 },
    Nonfix,
}

#[derive(Debug)]
pub enum Item<T> {
    Infix(u8, T),
    Nonfix(T),
}

pub trait Query<T> {
    fn fixity(&self, ty: &T) -> Fixity;
    fn infix(&self, left: T, right: T, term: T) -> Result<T, Error>;
    fn apply(&self, left: T, right: T) -> Result<T, Error>;
}

pub struct Precedence<T, Q> {
    stack: Vec<Item<T>>,
    query: Q,
}

impl<T, Q: Query<T>> Precedence<T, Q> {
    fn parse(&mut self, term: T) -> Result<(), Error> {
        let fixity = self.query.fixity(&term);
        let top = self.stack.pop().unwrap();

        match (top, fixity) {
            (Item::Nonfix(el), Fixity::Nonfix) => {
                let term = self.query.apply(el, term)?;
                self.stack.push(Item::Nonfix(term));
                Ok(())
            }

            (Item::Nonfix(e1), Fixity::Infix { left, right }) => {
                match (self.stack.pop(), self.stack.pop()) {
                    (Some(Item::Infix(p, e2)), Some(Item::Nonfix(e3))) => match () {
                        _ if left == p => Err(Error::SamePrecedence),
                        _ if left > p => {
                            self.stack.push(Item::Nonfix(e3));
                            self.stack.push(Item::Infix(p, e2));
                            self.stack.push(Item::Nonfix(e1));
                            self.stack.push(Item::Infix(right, term));
                            Ok(())
                        }
                        _ => {
                            let item = self.query.infix(e2, e3, e1)?;
                            self.stack.push(Item::Nonfix(item));
                            self.parse(term)
                        }
                    },

                    (Some(Item::Infix(p, e2)), e3) => {
                        self.stack.extend(e3);
                        self.stack.push(Item::Infix(p, e2));
                        self.stack.push(Item::Nonfix(e1));
                        self.stack.push(Item::Infix(right, term));
                        Ok(())
                    }

                    (e2, e3) => {
                        self.stack.extend(e3);
                        self.stack.extend(e2);
                        self.stack.push(Item::Nonfix(e1));
                        self.stack.push(Item::Infix(right, term));
                        Ok(())
                    }
                }
            }

            (left @ Item::Infix(..), Fixity::Nonfix) => {
                self.stack.push(left);
                self.stack.push(Item::Nonfix(term));
                Ok(())
            }

            (Item::Infix(..), Fixity::Infix { .. }) => Err(Error::InfixInPrefix),
        }
    }

    fn finish(mut self) -> Result<T, Error> {
        fn expect<T>(vec: &mut Vec<T>) -> T {
            vec.pop().expect("element in precedence parser")
        }

        match expect(&mut self.stack) {
            Item::Infix(..) => Err(Error::EndsWithInfix),
            Item::Nonfix(term) if self.stack.is_empty() => Ok(term),

            Item::Nonfix(term) => match (expect(&mut self.stack), expect(&mut self.stack)) {
                (Item::Infix(_, left), Item::Nonfix(right)) => {
                    let term = Item::Nonfix(self.query.infix(left, right, term)?);
                    self.stack.push(term);
                    self.finish()
                }
                _ => unreachable!("elements are in the wrong order"),
            },
        }
    }

    pub fn query(query: Q, items: Vec<T>) -> Result<T, Error> {
        debug_assert!(!items.is_empty(), "there must be some items");

        let mut this = Self {
            stack: Vec::new(),
            query,
        };

        let mut iter = items.into_iter();

        let first = iter.next().unwrap();
        if let Fixity::Infix { .. } = this.query.fixity(&first) {
            return Err(Error::InfixInPrefix);
        }

        this.stack.push(Item::Nonfix(first));
        for item in iter {
            this.parse(item)?;
        }

        this.finish()
    }
}
