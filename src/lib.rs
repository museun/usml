#![cfg_attr(debug_assertions, allow(dead_code))]

#[macro_use]
mod diag;

#[macro_use]
mod util;
use util::{Either, GettingTiredOfOneVec as _, GimmeABox as _};

mod stack;

mod intern;
use intern::Interner;

mod span;
use span::{Position, Span, Spanned};

mod symbols;

mod lexer;
use lexer::{Lexer, Token};

mod parser;
