use crate::{diag::Diagnostic, lexer::Token, span::Span};

#[derive(Clone, Debug)]
pub struct Error {
    pub span: Span,
    pub token: Token,
    pub kind: ErrorKind,
}

impl Error {
    pub fn into_diagnostic(self) -> Diagnostic {
        use ErrorKind::*;
        let msg = match self.kind {
            ExpectedToken(kind) => format!(
                "expected a token of kind {:?}, but found {:?}",
                kind, self.token
            ),
            ExpectedIdentifier => format!("expected identifier, but found {:?}", self.token),
            ExpectedDecl => format!("expected declaration, but found: {:?}", self.token),
            ExpectedExpr => format!("expected expression, but found: {:?}", self.token),
            ExpectedPat => format!("expected pattern, but found: {:?}", self.token),
            ExpectedType => format!("expected type, but found: {:?}", self.token),
            Internal => format!("internal parser error :( last token was: {:?}", self.token),
            Eof => "reached the end of file".to_string(),
        };
        Diagnostic::error(msg, self.span)
    }
}

#[derive(Clone, Debug)]
pub enum ErrorKind {
    ExpectedToken(Token),
    ExpectedIdentifier,
    ExpectedDecl,
    ExpectedExpr,
    ExpectedPat,
    ExpectedType,
    Internal,
    Eof,
}
