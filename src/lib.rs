#![cfg_attr(debug_assertions, allow(dead_code))]

#[macro_use]
mod diag;

#[macro_use]
mod util;
use util::Either;

mod stack;

mod intern;
use intern::Interner;

pub mod span;
use span::{Position, Span, Spanned};

pub mod symbols;

pub mod lexer;
use lexer::{Lexer, Token};

pub mod parser;
