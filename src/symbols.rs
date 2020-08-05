use super::intern::{self, Interner};
use std::cell::RefCell;

#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Symbol {
    Builtin(intern::Size),
    Interned(intern::Size),
    Gensym(intern::Size),
    Tuple(intern::Size),
}

impl Symbol {
    pub const fn unknown() -> Self {
        Self::Gensym(std::u32::MAX)
    }

    pub const fn gensym(n: intern::Size) -> Self {
        Self::Gensym(n)
    }

    pub const fn tuple_field(index: intern::Size) -> Self {
        Self::Tuple(index)
    }

    pub fn is_builtin(self) -> bool {
        matches!(self, Self::Builtin{..})
    }

    pub(super) fn next_name(id: intern::Size) -> String {
        const START: u8 = b'a'; // so we can use A-Z later
        const END: u8 = b'z'; // so we can use A-Z later
        const LIMIT: intern::Size = (END - START) as intern::Size;

        let tail = std::iter::once(((id % LIMIT) as u8 + START) as char);
        (0..id / LIMIT).map(|_| END as char).chain(tail).collect()
    }
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_struct("Symbol");

        match *self {
            Self::Builtin(n) => f.field("Builtin", &Builtin::STRINGS[n as usize]),
            Self::Interned(n) => f.field("Interned", &n),
            Self::Gensym(_) => {
                INTERNER.with(|i| f.field("Gensym", &i.borrow().lookup(*self).unwrap_or("?")))
            }
            Self::Tuple(n) => f.field("Tuple", &n),
        };

        f.finish()
    }
}

thread_local! {
    pub static INTERNER: RefCell<Interner> = RefCell::new(Interner::with_capacity(512));
}

macro_rules! global_symbols {
    // (@one $($x:tt)*) => { () };
    // (@len $($x:tt),*) => { <[()]>::len(&[$(global_symbols!(@one $x)),*]); };

    (@index $index:expr, ) => { };

    (@index $index:expr, $ident:ident, $($rest:ident,)*) => {
        pub const $ident: Symbol = Symbol::Builtin($index as _);
        global_symbols!(@index $index+1usize, $($rest,)*);
    };

    ($($repr:expr => $name:ident),+ $(,)?) => {
        pub struct Builtin;
        impl Builtin {
            pub const STRINGS: &'static [&'static str] = &[$($repr,)*];
            global_symbols!(@index 0usize, $($name,)*);
        }
    };
}

global_symbols! {
    "abstype"   => SYM_ABSTYPE,
    "and"       => SYM_AND,
    "andalso"   => SYM_ANDALSO,
    "as"        => SYM_AS,
    "case"      => SYM_CASE,
    "datatype"  => SYM_DATATYPE,
    "do"        => SYM_DO,
    "else"      => SYM_ELSE,
    "end"       => SYM_END,
    "exception" => SYM_EXCEPTION,
    "fn"        => SYM_FN,
    "fun"       => SYM_FUN,
    "functor"   => SYM_FUNCTOR,
    "handle"    => SYM_HANDLE,
    "if"        => SYM_IF,
    "in"        => SYM_IN,
    "infix"     => SYM_INFIX,
    "infixr"    => SYM_INFIXR,
    "let"       => SYM_LET,
    "local"     => SYM_LOCAL,
    "nonfix"    => SYM_NONFIX,
    "of"        => SYM_OF,
    "op"        => SYM_OP,
    "open"      => SYM_OPEN,
    "orelse"    => SYM_ORELSE,
    "primitive" => SYM_PRIMITIVE,
    "raise"     => SYM_RAISE,
    "rec"       => SYM_REC,
    "then"      => SYM_THEN,
    "type"      => SYM_TYPE,
    "val"       => SYM_VAL,
    "with"      => SYM_WITH,
    "withtype"  => SYM_WITHTYPE,
    "while"     => SYM_WHILE,
    "sig"       => SYM_SIG,
    "signature" => SYM_SIGNATURE,
    "struct"    => SYM_STRUCT,
    "structure" => SYM_STRUCTURE,
    "."         => SYM_DOT,
    "..."       => SYM_FLEX,
    "->"        => SYM_ARROW,
    "=>"        => SYM_FATARROW,
    ":"         => SYM_COLON,
    "|"         => SYM_PIPE,
    "="         => SYM_EQUAL,
    ":>"        => SYM_OPAQUE,
    "*"         => SYM_STAR,
    "\\"        => SYM_SLASH,
    "+"         => SYM_PLUS,
    "-"         => SYM_MINUS,
    "int"       => SYM_INT,
    "char"      => SYM_CHAR,
    "string"    => SYM_STRING,
    "ref"       => SYM_REF,
    "list"      => SYM_LIST,
    "bool"      => SYM_BOOL,
    "exn"       => SYM_EXN,
    "nil"       => SYM_NIL,
    "::"        => SYM_CONS,
    "true"      => SYM_TRUE,
    "false"     => SYM_FALSE,
    "unit"      => SYM_UNIT,
    "Match"     => SYM_MATCH,
}
