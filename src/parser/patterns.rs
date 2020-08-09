use super::{ErrorKind, Parser, Result};
use crate::symbols::Builtin;
use crate::{parser::ast::*, util::*, Token};

impl<'a, 'sym> Parser<'a, 'sym> {
    pub fn parse_pattern(&mut self) -> Result<Pat> {
        let mut span = self.current.span;

        let pat = self.application_pattern()?;
        if !self.maybe_bump(Token::Colon) {
            return Ok(pat);
        }

        let ty = self.once(Self::parse_type, "expected type annotation after `pat :`")?;
        span += self.prev;

        Ok(Pat::new(PatKind::Ascribe(pat.boxed(), ty.boxed()), span))
    }

    pub(super) fn atomic_pattern(&mut self) -> Result<Pat> {
        let span = self.current.span;
        match self.current.item {
            Token::Wildcard => {
                self.bump();
                Ok(Pat::new(PatKind::Wildcard, span))
            }

            Token::Ident(..) | Token::SymIdent(..) => {
                let pat = self.expect_ident().map(PatKind::Variable)?;
                Ok(Pat::new(pat, span))
            }

            Token::Const(..) => {
                let pat = self.constant().map(PatKind::Const)?;
                Ok(Pat::new(pat, span))
            }

            Token::LParen => self.spanned(Self::tuple_pattern),
            Token::LBrace => self.spanned(Self::record_pattern),
            Token::LBracket => self.spanned(Self::list_pattern),

            _ => self.error(ErrorKind::ExpectedPat),
        }
    }

    fn tuple_pattern(&mut self) -> Result<PatKind> {
        self.expect(Token::LParen)?;
        if self.maybe_bump(Token::RParen) {
            return Ok(PatKind::Const(Const::Unit));
        }

        // this should always parse an expression because we handle an empty
        // tuple (a unit) above
        let mut out = self.kleene_star(Self::parse_pattern, Token::Comma);
        self.expect_maybe(Token::RParen);

        debug_assert!(!out.is_empty());
        match out.len() {
            1 => Ok(out.pop().unwrap().item),
            _ => Ok(PatKind::make_record(out, false)),
        }
    }

    fn record_pattern(&mut self) -> Result<PatKind> {
        self.expect(Token::LBrace)?;
        if self.maybe_bump(Token::RBrace) {
            return Ok(PatKind::Const(Const::Unit));
        }
        let out = self.delimited(Self::row_pattern, Token::Comma)?;
        let flex = self.maybe_bump(Token::Flex);
        self.expect_maybe(Token::RBrace);
        Ok(PatKind::Record(out, flex))
    }

    fn row_pattern(&mut self) -> Result<Row<Pat>> {
        let span = self.current.span;
        let label = self.expect_ident()?;

        let (item, span) = if self.maybe_bump(Token::Equals) {
            let item = self.once(Self::parse_pattern, "expected pattern in `label = ...`")?;
            (item, span + self.prev)
        } else {
            let item = Pat::new(PatKind::Variable(label), span);
            (item, span)
        };

        Ok(Row { label, item, span })
    }

    fn list_pattern(&mut self) -> Result<PatKind> {
        self.expect(Token::LBracket)?;
        if self.maybe_bump(Token::RBracket) {
            return Ok(PatKind::Variable(Builtin::SYM_NIL));
        }
        let out = self.delimited(Self::parse_pattern, Token::Comma)?;
        self.expect_maybe(Token::RBracket);
        Ok(PatKind::List(out))
    }

    // app_pat ::= atpat
    //             app_pat atpat
    fn application_pattern(&mut self) -> Result<Pat> {
        let span = self.current.span;
        let mut pats = self.kleene_plus(Self::atomic_pattern, None)?;

        debug_assert!(!pats.is_empty());
        match pats.len() {
            1 => Ok(pats.pop().unwrap()),
            _ => Ok(Pat::new(PatKind::FlatApply(pats), span + self.prev)),
        }
    }
}
