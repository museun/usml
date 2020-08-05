use super::{Error, ErrorKind, Parser, Result};
use crate::{diag::Diagnostic, parser::ast::*, util::*, Either, Token};

// decls
impl<'a, 'sym> Parser<'a, 'sym> {
    pub fn parse_decl(&mut self) -> Result<Decl> {
        let mut seq = vec![];
        let span = self.current.span;

        self.maybe_bump(Token::Semi);

        loop {
            match self.parse_decl_atom() {
                Ok(d) => seq.push(d),
                Err(Error { kind, .. }) if matches!(kind, ErrorKind::Eof) => break,

                Err(..) => match self.parse_expr() {
                    Ok(expr) => seq.push(self.insert_top_level(expr)),
                    Err(err) => {
                        match err.kind {
                            ErrorKind::Eof | ErrorKind::ExpectedExpr => {}
                            _ => self.errors.push(err.into_diagnostic()),
                        }
                        break;
                    }
                },
            }

            self.maybe_bump(Token::Semi);
        }

        seq.pop_expect(
            |seq| Decl::new(DeclKind::Seq(seq), span + self.prev),
            || self.error(ErrorKind::ExpectedDecl),
        )
        .map(Either::factor)
    }

    pub fn parse_decl_local(&mut self) -> Result<DeclKind> {
        self.expect(Token::Local)?;
        let left = self.parse_decl()?;
        self.expect_maybe(Token::In);
        let right = self.parse_decl()?;
        self.expect_maybe(Token::End);
        Ok(DeclKind::Local(left.boxed(), right.boxed()))
    }

    pub fn parse_decl_exn(&mut self) -> Result<DeclKind> {
        self.expect(Token::Exception)?;
        let bindings = self.delimited(Self::variant, Token::And)?;
        Ok(DeclKind::Exception(bindings))
    }

    fn insert_top_level(&mut self, expr: Expr) -> Decl {
        self.errors.push(Diagnostic::warn(
            "top level expressions are not supported. emitting `val _ = ...`",
            expr.span,
        ));
        let span = expr.span;
        Decl::new(
            DeclKind::Value(vec![], Pat::new(PatKind::Wildcard, span), expr),
            span,
        )
    }

    fn type_binding(&mut self) -> Result<Typebind> {
        let ty_vars = self.type_var_seq()?;
        let ty_con = self.expect_ident()?;
        self.expect(Token::Equals)?;
        let ty = self.parse_type()?;
        Ok(Typebind {
            ty_con,
            ty_vars,
            ty,
        })
    }

    fn parse_decl_type(&mut self) -> Result<DeclKind> {
        self.expect(Token::Type)?;
        let bindings = self.delimited(Self::type_binding, Token::And)?;
        Ok(DeclKind::Type(bindings))
    }

    fn variant(&mut self) -> Result<Variant> {
        let mut span = self.current.span;
        let label = self.expect_ident()?;
        let item = match self.maybe_bump(Token::Of) {
            true => Some(self.parse_type()?),
            false => None,
        };
        span += self.prev;
        Ok(Variant { label, item, span })
    }

    fn data_type(&mut self) -> Result<Datatype> {
        let mut span = self.current.span;
        let ty_vars = self.type_var_seq()?;
        let ty_con = self.expect_ident()?;
        self.expect(Token::Equals)?;
        let ctors = self.delimited(Self::variant, Token::Pipe)?;
        span += self.prev;
        Ok(Datatype {
            ty_con,
            ty_vars,
            ctors,
            span,
        })
    }

    fn parse_decl_datatype(&mut self) -> Result<DeclKind> {
        self.expect(Token::Datatype)?;
        let bindings = self.delimited(Self::data_type, Token::And)?;
        Ok(DeclKind::Datatype(bindings))
    }

    fn parse_decl_val(&mut self) -> Result<DeclKind> {
        self.expect(Token::Val)?;
        let vars = self.type_var_seq()?;
        let pat = self.parse_pattern()?;
        self.expect(Token::Equals)?;
        let expr = self.parse_expr()?;
        Ok(DeclKind::Value(vars, pat, expr))
    }

    fn parse_fun_binding(&mut self) -> Result<FnBinding> {
        let mut span = self.current.span;
        let name = self.once(Self::expect_ident, "ident required for function binding")?;
        let pats = self.kleene_plus(Self::atomic_pattern, None)?;

        let ret = if self.maybe_bump(Token::Colon) {
            Some(self.once(Self::parse_type, "result type expected after ':'")?)
        } else {
            None
        };

        self.expect(Token::Equals)?;

        let expr = self.once(Self::parse_expr, "missing expression in function")?;
        span += self.prev;

        Ok(FnBinding {
            name,
            pats,
            expr,
            ret,
            span,
        })
    }

    fn parse_fun(&mut self) -> Result<Fun> {
        self.spanned(|p| p.delimited(Self::parse_fun_binding, Token::Pipe))
    }

    fn parse_decl_fun(&mut self) -> Result<DeclKind> {
        self.expect(Token::Fun)?;
        let vars = self.type_var_seq()?;
        let funs = self.delimited(Self::parse_fun, Token::And)?;
        Ok(DeclKind::Function(vars, funs))
    }

    fn fixity(&mut self) -> Result<DeclKind> {
        let fixity = match self.bump() {
            Token::Infix => Fixity::Infix,
            Token::Infixr => Fixity::Infixr,
            Token::Nonfix => Fixity::Nonfix,
            _ => unreachable!(),
        };

        let p = match self.current() {
            Token::Const(Const::Int(i)) => {
                self.bump();
                i as u8
            }
            _ => 0,
        };

        let symbol = self.once(Self::expect_ident, "symbol required after fixity decl")?;
        Ok(DeclKind::Fixity(fixity, p, symbol))
    }

    fn parse_decl_atom(&mut self) -> Result<Decl> {
        match self.current() {
            Token::Fun => self.spanned(Self::parse_decl_fun),
            Token::Val => self.spanned(Self::parse_decl_val),
            Token::Type => self.spanned(Self::parse_decl_type),
            Token::Datatype => self.spanned(Self::parse_decl_datatype),
            Token::Exception => self.spanned(Self::parse_decl_exn),
            Token::Infix | Token::Infixr | Token::Nonfix => self.spanned(Self::fixity),
            Token::Local => self.spanned(Self::parse_decl_local),
            Token::Eof => self.error(ErrorKind::Eof),
            _ => self.error(ErrorKind::ExpectedDecl),
        }
    }
}
