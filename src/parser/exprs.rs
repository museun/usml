use super::{ErrorKind, Parser, Result};
use crate::{
    intern::Size,
    parser::ast::*,
    symbols::{Builtin, Symbol},
    util::*,
    Token,
};

impl<'a, 'sym> Parser<'a, 'sym> {
    pub fn parse_expr(&mut self) -> Result<Expr> {
        let expr = match self.current() {
            Token::Case => self.spanned(Self::case_expr),
            Token::Fn => self.spanned(Self::lambda_expr),
            Token::While => self.spanned(Self::while_expr),
            Token::If => self.spanned(Self::if_expr),
            Token::Raise => self.spanned(Self::raise_expr),
            _ => self.application_expr(),
        }?;

        let (span, kind) = match self.current() {
            Token::Colon => {
                self.bump();
                let rhs = self.once(Self::parse_type, "expected type after `exp : `")?;
                let span = expr.span + rhs.span;
                (span, ExprKind::Constraint(expr.boxed(), rhs.boxed()))
            }
            Token::Handle => {
                self.bump();
                let rhs = self.spanned(|p| p.delimited(Self::case_arm, Token::Pipe))?;
                let span = expr.span + rhs.span;
                (span, ExprKind::Handle(expr.boxed(), rhs.item))
            }
            Token::Orelse => {
                self.bump();
                let rhs = self.once(Self::parse_expr, "expected expression after orelse")?;
                let span = expr.span + rhs.span;
                (span, ExprKind::OrElse(expr.boxed(), rhs.boxed()))
            }
            Token::Andalso => {
                self.bump();
                let rhs = self.once(Self::parse_expr, "expected expression after andalso")?;
                let span = expr.span + rhs.span;
                (span, ExprKind::AndAlso(expr.boxed(), rhs.boxed()))
            }
            _ => return Ok(expr),
        };

        Ok(Expr::new(kind, span))
    }

    pub(super) fn constant(&mut self) -> Result<Const> {
        match self.bump() {
            Token::Const(c) => Ok(c),
            _ => self.error(ErrorKind::Internal),
        }
    }

    // atexp ::=    constant
    //              id
    //              { [label = expr] }
    //              ()
    //              ( expr, .. exprN )
    //              ( expr )
    //              let decl in expr, ... exprN end
    fn atomic_expr(&mut self) -> Result<Expr> {
        let span = self.current.span;
        match self.current.item {
            Token::Ident(..) | Token::SymIdent(..) => {
                let expr = self.expect_ident().map(ExprKind::Var)?;
                Ok(Expr::new(expr, span))
            }
            Token::Primitive => self.spanned(Self::primitive),
            Token::Let => self.spanned(Self::let_binding),
            Token::Selector => self.spanned(Self::selector),
            Token::Const(..) => {
                let expr = self.constant().map(ExprKind::Const)?;
                Ok(Expr::new(expr, span))
            }
            Token::LBrace => self.spanned(Self::record_expr),
            Token::LParen => self.spanned(Self::seq_expr),
            Token::LBracket => self.spanned(Self::atomic_list),
            _ => self.error(ErrorKind::ExpectedExpr),
        }
    }

    fn atomic_list(&mut self) -> Result<ExprKind> {
        self.expect(Token::LBracket)?;
        if self.maybe_bump(Token::RBracket) {
            return Ok(ExprKind::Var(Builtin::SYM_NIL));
        }

        let rest = self
            .delimited(Self::parse_expr, Token::Comma)
            .map(ExprKind::List)?;
        self.expect_maybe(Token::RBracket);
        Ok(rest)
    }

    fn primitive(&mut self) -> Result<ExprKind> {
        self.expect(Token::Primitive)?;

        if let Token::Const(Const::String(sym)) = self.current() {
            self.bump();
            self.expect(Token::Colon)?;

            return self
                .parse_type()
                .map(|ty| ExprKind::Primitive(Primitive { sym, ty }));
        }

        let tok = Token::Const(Const::String(Symbol::unknown()));
        self.error(ErrorKind::ExpectedToken(tok))
    }

    fn selector(&mut self) -> Result<ExprKind> {
        self.expect(Token::Selector)?;
        let symbol = match self.current() {
            Token::Ident(sym) | Token::SymIdent(sym) => {
                self.bump();
                sym
            }
            Token::Const(Const::Int(id)) => {
                self.bump();
                Symbol::tuple_field(id as Size)
            }
            _ => return self.error(ErrorKind::ExpectedIdentifier),
        };

        Ok(ExprKind::Selector(symbol))
    }

    fn seq_expr(&mut self) -> Result<ExprKind> {
        self.expect(Token::LParen)?;
        if self.maybe_bump(Token::RParen) {
            return Ok(ExprKind::Const(Const::Unit));
        }

        let first = self.parse_expr()?;
        let expected = match self.current() {
            tok @ Token::Semi | tok @ Token::Comma => tok,
            _ => {
                self.expect_maybe(Token::RParen);
                return Ok(first.item);
            }
        };
        self.bump();

        let mut out = vec![first];
        while let Ok(expr) = self.parse_expr() {
            out.push(expr);
            if !self.maybe_bump(expected) {
                break;
            }
        }

        self.expect(Token::RParen)?;

        debug_assert!(!out.is_empty());
        let ok = match out.len() {
            1 => out.pop().unwrap().item,
            _ => match expected {
                Token::Semi => ExprKind::Seq(out),
                Token::Comma => ExprKind::make_record(out),
                _ => unreachable!(),
            },
        };

        Ok(ok)
    }

    fn raise_expr(&mut self) -> Result<ExprKind> {
        self.expect(Token::Raise)?;
        let cond = self.parse_expr()?;
        Ok(ExprKind::Raise(cond.boxed()))
    }

    fn if_expr(&mut self) -> Result<ExprKind> {
        self.expect(Token::If)?;
        let cond = self.parse_expr()?;
        self.expect(Token::Then)?;
        let then = self.parse_expr()?;
        self.expect(Token::Else)?;
        let else_ = self.parse_expr()?;
        Ok(ExprKind::If(cond.boxed(), then.boxed(), else_.boxed()))
    }

    fn while_expr(&mut self) -> Result<ExprKind> {
        self.expect(Token::While)?;
        let cond = self.parse_expr()?;
        self.expect(Token::Do)?;
        let expr = self.parse_expr()?;
        Ok(ExprKind::While(cond.boxed(), expr.boxed()))
    }

    fn lambda_expr(&mut self) -> Result<ExprKind> {
        self.expect(Token::Fn)?;
        let arms = self.delimited(Self::case_arm, Token::Pipe)?;
        Ok(ExprKind::Fn(arms))
    }

    fn case_expr(&mut self) -> Result<ExprKind> {
        self.expect(Token::Case)?;
        let expr = self.once(Self::parse_expr, "missing case expression")?;
        self.expect(Token::Of)?;
        self.maybe_bump(Token::Pipe);
        let arms = self.delimited(Self::case_arm, Token::Pipe)?;
        self.expect_maybe(Token::End);
        Ok(ExprKind::Case(expr.boxed(), arms))
    }

    fn case_arm(&mut self) -> Result<Rule> {
        let pat = self.once(Self::parse_pattern, "missing pattern in case arm")?;
        self.expect(Token::FatArrow)?;
        let expr = self.once(Self::parse_expr, "missing expression in case arm")?;
        self.maybe_bump(Token::Comma);
        let span = pat.span + expr.span;
        Ok(Rule { pat, expr, span })
    }

    fn let_binding(&mut self) -> Result<ExprKind> {
        self.expect(Token::Let)?;
        let decls = self.kleene_plus(
            |p| {
                let d = p.parse_decl();
                p.maybe_bump(Token::Semi);
                d
            },
            None,
        )?;
        self.expect_maybe(Token::In);
        let body = self.once(Self::parse_expr, "let body is required")?;
        self.expect_maybe(Token::End);
        Ok(ExprKind::Let(decls, body.boxed()))
    }

    fn record_expr(&mut self) -> Result<ExprKind> {
        self.expect(Token::LBrace)?;
        let fields = self.delimited(Self::record_row, Token::Comma)?;
        self.expect_maybe(Token::RBrace);
        Ok(ExprKind::Record(fields))
    }

    fn record_row(&mut self) -> Result<Row<Expr>> {
        let mut span = self.current.span;
        let label = self.expect_ident()?;
        self.expect_maybe(Token::Equals);
        let item = self.once(Self::parse_expr, "missing expr in record row")?;
        span += self.prev;
        Ok(Row { label, item, span })
    }

    // appexp ::=   atexp
    //              appexp atexp
    fn application_expr(&mut self) -> Result<Expr> {
        let span = self.current.span;
        let mut exprs = vec![self.atomic_expr()?];
        while let Ok(exp) = self.atomic_expr() {
            exprs.push(exp)
        }

        debug_assert!(!exprs.is_empty());

        match exprs.len() {
            1 => Ok(exprs.pop().unwrap()),
            _ => Ok(Expr::new(ExprKind::FlatApply(exprs), span + self.prev)),
        }
    }
}
