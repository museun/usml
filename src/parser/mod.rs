use crate::{diag::Diagnostic, symbols::Symbol};
use crate::{Interner, Lexer, Span, Spanned, Token};

use std::iter::Peekable;

mod precedence;

pub mod ast;
pub mod decls;
pub mod exprs;
pub mod patterns;
pub mod types;

pub mod error;
pub use error::{Error, ErrorKind};

type Result<T> = std::result::Result<T, Error>;

pub struct Parser<'a, 'sym> {
    tokens: Peekable<Lexer<'a, 'sym>>,
    current: Spanned<Token>,
    prev: Span,
    errors: Vec<Diagnostic>,
}

impl<'a, 'sym> Parser<'a, 'sym> {
    pub fn new(input: &'a str, interner: &'sym mut Interner) -> Self {
        let mut parser = Self {
            tokens: Lexer::new(input, interner).peekable(),
            current: Spanned::new(Token::Eof, Span::zero()),
            prev: Span::zero(),
            errors: Vec::new(),
        };
        parser.bump();
        parser
    }

    fn bump(&mut self) -> Token {
        use std::mem::replace;
        match self.tokens.next() {
            Some(tok) => {
                self.prev = self.current.span;
                replace(&mut self.current, tok).item
            }
            None => replace(&mut self.current.item, Token::Eof),
        }
    }

    fn maybe_bump(&mut self, token: Token) -> bool {
        if self.current.item == token {
            self.bump();
            return true;
        }
        false
    }

    fn error<T>(&self, kind: ErrorKind) -> Result<T> {
        Err(Error {
            span: self.current.span,
            token: self.current.item,
            kind,
        })
    }

    fn current(&self) -> Token {
        self.current.item
    }

    fn is_ident(&self) -> bool {
        matches!(self.current(), Token::Ident{..} | Token::SymIdent{..})
    }

    fn expect(&mut self, kind: Token) -> Result<()> {
        if self.current() == kind {
            self.bump();
            return Ok(());
        }

        self.errors.push(diag!(
            self.current.span,
            "expected token {:?}, but found {:?}",
            kind,
            self.current()
        ));
        self.error(ErrorKind::ExpectedToken(kind))
    }

    fn expect_maybe(&mut self, kind: Token) {
        if self.current() == kind {
            self.bump();
            return;
        }

        self.errors.push(diag!(
            Span::new(self.prev.end, self.current.span.start),
            "inserting token: {:?}",
            kind
        ))
    }

    fn expect_ident(&mut self) -> Result<Symbol> {
        match self.current() {
            Token::Ident(s) | Token::SymIdent(s) => {
                self.bump();
                Ok(s)
            }
            _ => self.error(ErrorKind::ExpectedIdentifier),
        }
    }

    fn expect_ident_alpha(&mut self) -> Result<Symbol> {
        match self.current() {
            Token::Ident(s) => {
                self.bump();
                Ok(s)
            }
            _ => self.error(ErrorKind::ExpectedIdentifier),
        }
    }

    fn spanned<T, F>(&mut self, func: F) -> Result<Spanned<T>>
    where
        F: Fn(&mut Self) -> Result<T>,
    {
        let span = self.current.span;
        func(self).map(|inner| Spanned::new(inner, span + self.current.span))
    }

    fn once<T, E, F>(&mut self, func: F, message: &str) -> std::result::Result<T, E>
    where
        F: Fn(&mut Self) -> std::result::Result<T, E>,
    {
        func(self).map_err(|err| {
            self.errors.push(diag!(self.current.span, "{}", message));
            err
        })
    }

    fn kleene_plus<T, E, F, D>(&mut self, func: F, delimited: D) -> std::result::Result<Vec<T>, E>
    where
        F: Fn(&mut Self) -> std::result::Result<T, E>,
        D: Into<Option<Token>>,
    {
        let delimited = delimited.into();
        let mut out = vec![func(self)?];
        if let Some(tok) = delimited {
            if !self.maybe_bump(tok) {
                return Ok(out);
            }
        }

        while let Ok(exp) = func(self) {
            out.push(exp);
            if let Some(tok) = delimited {
                if !self.maybe_bump(tok) {
                    return Ok(out);
                }
            }
        }
        Ok(out)
    }

    fn kleene_star<T, E, F, D>(&mut self, func: F, delimited: D) -> Vec<T>
    where
        F: Fn(&mut Self) -> std::result::Result<T, E>,
        D: Into<Option<Token>>,
    {
        let delimited = delimited.into();
        let mut out = vec![];
        while let Ok(exp) = func(self) {
            out.push(exp);
            if let Some(tok) = delimited {
                if !self.maybe_bump(tok) {
                    break;
                }
            }
        }
        out
    }

    fn delimited<T, F>(&mut self, func: F, delimited: Token) -> Result<Vec<T>>
    where
        F: Fn(&mut Self) -> Result<T>,
    {
        let mut out = vec![func(self)?];
        if !self.maybe_bump(delimited) {
            return Ok(out);
        }

        while let Ok(exp) = func(self) {
            out.push(exp);
            if !self.maybe_bump(delimited) {
                break;
            }
        }
        Ok(out)
    }
}
