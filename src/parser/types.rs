use super::{ErrorKind, Parser, Result};
use crate::symbols::{Builtin, Symbol};
use crate::{parser::ast::*, util::*, Token};

impl<'a, 'sym> Parser<'a, 'sym> {
    pub fn parse_type(&mut self) -> Result<Type> {
        let mut span = self.current.span;
        let ty = self.product()?;
        if self.maybe_bump(Token::Arrow) {
            let next = self.parse_type()?;
            span += next.span;
            let ctor = TypeKind::Ctor(Builtin::SYM_ARROW, vec![ty, next]);
            return Ok(Type::new(ctor, span));
        }
        Ok(ty)
    }

    pub(super) fn type_var_seq(&mut self) -> Result<Vec<Symbol>> {
        if self.current() == Token::LParen {
            match self.tokens.peek().map(|t| t.item) {
                Some(Token::Apostrophe) => self.bump(),
                _ => return Ok(Vec::new()),
            };

            let ret = self.delimited(Self::type_var, Token::Comma)?;
            self.expect(Token::RParen)?;
            return Ok(ret);
        }

        if self.current() == Token::Apostrophe {
            Ok(vec![self.type_var()?])
        } else {
            Ok(Vec::new())
        }
    }

    fn row(&mut self) -> Result<Row<Type>> {
        let span = self.current.span;
        let label = self.expect_ident()?;
        self.expect(Token::Colon)?;
        let item = self.once(
            Self::parse_type,
            "record type row requires a type {label: ty, ...}",
        )?;
        Ok(Row { label, item, span })
    }

    fn record(&mut self) -> Result<TypeKind> {
        self.expect(Token::LBrace)?;
        let rows = self.delimited(Self::row, Token::Comma)?;
        self.expect(Token::RBrace)?;
        Ok(TypeKind::Record(rows))
    }

    fn type_atom(&mut self) -> Result<Type> {
        let span = self.current.span;
        match self.current.item {
            Token::Apostrophe => {
                self.bump();
                let span = span + self.current.span;
                self.expect_ident_alpha()
                    .map(|p| Type::new(TypeKind::Var(p), span))
            }

            Token::Ident(..) | Token::SymIdent(..) => {
                let ctor = self.expect_ident()?;
                Ok(Type::new(TypeKind::Ctor(ctor, vec![]), span))
            }

            Token::LBrace => self.spanned(Self::record),

            Token::LParen => {
                self.bump();

                let mut out = self.delimited(Self::parse_type, Token::Comma)?;
                self.expect(Token::RParen)?;
                if out.len() == 1 {
                    return Ok(out.pop().unwrap());
                }

                let ty_ctor = self.once(
                    Self::expect_ident,
                    "expected type constructor after `(ty1,..tyN)`",
                )?;
                Ok(Type::new(TypeKind::Ctor(ty_ctor, out), span + self.prev))
            }

            _ => self.error(ErrorKind::ExpectedType),
        }
    }

    // ty | ty * ty * ty
    fn product(&mut self) -> Result<Type> {
        let span = self.current.span;
        let mut out = vec![self.application()?];
        while self.maybe_bump(Token::SymIdent(Builtin::SYM_STAR)) {
            out.push(self.application()?)
        }

        let res = out.pop_maybe(|seq| Type::new(TypeKind::make_record(seq), span + self.prev));
        Ok(res.factor())
    }

    fn type_var(&mut self) -> Result<Symbol> {
        self.expect(Token::Apostrophe)?;
        self.expect_ident_alpha()
    }

    fn application(&mut self) -> Result<Type> {
        let mut head = self.type_atom()?;
        while self.is_ident() && self.current() == Token::SymIdent(Builtin::SYM_STAR) {
            let ctor = self.expect_ident()?;
            let span = head.span;
            head = Type::new(TypeKind::Ctor(ctor, vec![head]), span + self.prev);
        }
        Ok(head)
    }
}
