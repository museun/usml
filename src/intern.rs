use crate::symbols::{self, Symbol};
use std::collections::HashMap;

pub type Size = u32;

#[derive(Default)]
pub struct Interner {
    map: HashMap<&'static str, Symbol>,
    vec: Vec<&'static str>,
    buf: String,
    set: Vec<String>,
}

impl Interner {
    pub fn with_capacity(cap: usize) -> Self {
        let cap = cap.next_power_of_two();
        Self {
            buf: String::with_capacity(cap),
            ..Default::default()
        }
    }

    pub fn intern(&mut self, name: &str) -> Symbol {
        if let Some(&symbol) = self.map.get(name) {
            return symbol;
        }

        let name = unsafe { self.alloc(name) };

        let index = self.map.len() as _;
        let sym = Symbol::Interned(index);
        self.map.insert(name, sym);
        self.vec.push(name);

        debug_assert!(self.lookup(sym).unwrap() == name);
        debug_assert!(self.intern(name) == sym);
        sym
    }

    pub fn lookup(&self, symbol: Symbol) -> Option<&str> {
        match symbol {
            Symbol::Builtin(id) => symbols::Builtin::STRINGS.get(id as usize).copied(),
            Symbol::Interned(id) => self.vec.get(id as usize).copied(),
            _ => None,
        }
    }

    unsafe fn alloc(&mut self, name: &str) -> &'static str {
        let cap = self.buf.capacity();
        if cap < self.buf.len() + name.len() {
            let cap = (cap.max(name.len()) + 1).next_power_of_two();
            let new = String::with_capacity(cap);
            let old = std::mem::replace(&mut self.buf, new);
            self.set.push(old)
        }

        let interned = {
            let start = self.buf.len();
            self.buf.push_str(name);
            &self.buf[start..]
        };

        &*(interned as *const str)
    }
}
