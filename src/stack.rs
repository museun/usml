#[derive(Clone, Default)]
pub struct Stack<T> {
    inner: Vec<T>,
}

impl<T> Stack<T> {
    pub const fn new() -> Self {
        Self { inner: Vec::new() }
    }

    pub fn with_capacity(size: usize) -> Self {
        Self {
            inner: Vec::with_capacity(size),
        }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn push(&mut self, val: T) {
        self.inner.push(val)
    }

    pub fn pop(&mut self) -> Option<T> {
        self.inner.pop()
    }

    pub fn pop_many(&mut self, n: usize) {
        let _ = (0..n).map(|_| self.pop().map(drop)).last();
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.inner.get(self.inner.len().checked_sub(1 + index)?)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        let len = self.inner.len().checked_sub(1 + index)?;
        self.inner.get_mut(len)
    }

    pub fn iter(&self) -> std::slice::Iter<T> {
        self.inner.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<T> {
        self.inner.iter_mut()
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Stack<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.inner.iter()).finish()
    }
}

impl<T: PartialEq> Stack<T> {
    fn find(&self, key: &T) -> Option<usize> {
        self.iter().rposition(|e| e == key)
    }
}

impl<T> Extend<T> for Stack<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.inner.extend(iter)
    }
}

#[test]
fn stack_order() {
    let mut stack = Stack::new();
    stack.extend(0..5);

    assert_eq!(stack.get(0).unwrap(), &4);
    assert_eq!(stack.get(1).unwrap(), &3);
    assert_eq!(stack.get(2).unwrap(), &2);

    stack.pop();
    assert_eq!(stack.get(0).unwrap(), &3);

    stack.pop_many(3);
    assert_eq!(stack.get(0).unwrap(), &0);
    assert!(stack.get(1).is_none());
}
