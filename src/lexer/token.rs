use crate::{parser::ast::Const, symbols::Symbol};

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Token {
    Apostrophe,
    Wildcard,
    Dot,
    Flex,
    Pipe,
    Comma,
    Colon,
    Semi,
    Arrow,
    FatArrow,
    Equals,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Selector,

    Abstype,
    And,
    Andalso,
    As,
    Case,
    Datatype,
    Do,
    Else,
    End,
    Exception,
    Fn,  // this one is for type signatures
    Fun, // this one is for syntax
    Functor,
    Handle,
    If,
    In,
    Infix,
    Infixr,
    Let,
    Local,
    Nonfix,
    Of,
    Op,
    Open,
    Orelse,
    Primitive,
    Raise,
    Rec,
    Then,
    Type,
    Val,
    With,
    Withtype,
    While,
    Sig,
    Signature,
    Struct,
    Structure,

    Forall,
    Ident(Symbol),
    SymIdent(Symbol),
    Const(Const),

    Invalid(char),
    Eof,
}
