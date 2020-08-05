use crate::{
    intern::Size,
    span::{Span, Spanned},
    symbols::Symbol,
};

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Const {
    Unit,
    Int(usize),
    Char(char),
    String(Symbol),
}

pub struct Datatype {
    pub ty_con: Symbol,
    pub ty_vars: Vec<Symbol>,
    pub ctors: Vec<Variant>,
    pub span: Span,
}

pub struct Typebind {
    pub ty_con: Symbol,
    pub ty_vars: Vec<Symbol>,
    pub ty: Type,
}

pub struct Primitive {
    pub sym: Symbol,
    pub ty: Type,
}

pub enum Fixity {
    Infix,
    Infixr,
    Nonfix,
}

pub type Decl = Spanned<DeclKind>;

pub enum DeclKind {
    Datatype(Vec<Datatype>),
    Type(Vec<Typebind>),
    Function(Vec<Symbol>, Vec<Fun>),
    Value(Vec<Symbol>, Pat, Expr),
    Exception(Vec<Variant>),
    Fixity(Fixity, u8, Symbol),
    Local(Box<Decl>, Box<Decl>),
    Seq(Vec<Decl>),
}

pub type Type = Spanned<TypeKind>;

pub enum TypeKind {
    Var(Symbol),
    Ctor(Symbol, Vec<Type>), // Con
    Record(Vec<Row<Type>>),
}

impl TypeKind {
    pub fn make_record(v: Vec<Type>) -> Self {
        let v = v.into_iter().enumerate().map(|(i, e)| Row {
            label: Symbol::tuple_field(1 + i as Size),
            span: e.span,
            item: e,
        });
        Self::Record(v.collect())
    }
}

pub type Expr = Spanned<ExprKind>;

pub enum ExprKind {
    AndAlso(Box<Expr>, Box<Expr>),
    Apply(Box<Expr>, Box<Expr>),
    Case(Box<Expr>, Vec<Rule>),
    Const(Const),
    Constraint(Box<Expr>, Box<Type>),
    FlatApply(Vec<Expr>),
    Fn(Vec<Rule>), // this is a lambda
    Handle(Box<Expr>, Vec<Rule>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let(Vec<Decl>, Box<Expr>),
    List(Vec<Expr>),
    OrElse(Box<Expr>, Box<Expr>),
    Primitive(Primitive),
    Raise(Box<Expr>),
    // no Tuple because we parse them into anonymous Records (like Rust)
    Record(Vec<Row<Expr>>),
    Selector(Symbol),
    Seq(Vec<Expr>),
    Var(Symbol),
    While(Box<Expr>, Box<Expr>),
}

impl ExprKind {
    pub fn make_record(v: Vec<Expr>) -> Self {
        let v = v.into_iter().enumerate().map(|(i, e)| Row {
            label: Symbol::tuple_field(1 + i as Size),
            span: e.span,
            item: e,
        });
        Self::Record(v.collect())
    }
}

pub type Fun = Spanned<Vec<FnBinding>>;

pub struct FnBinding {
    pub name: Symbol,
    pub pats: Vec<Pat>,
    pub ret: Option<Type>,
    pub expr: Expr,
    pub span: Span,
}

pub struct Rule {
    pub pat: Pat,
    pub expr: Expr,
    pub span: Span,
}

pub type Variant = Row<Option<Type>>;

pub struct Row<T> {
    pub label: Symbol,
    pub item: T,
    pub span: Span,
}

pub type Pat = Spanned<PatKind>;

pub enum PatKind {
    Apply(Symbol, Box<Pat>),
    Ascribe(Box<Pat>, Box<Type>),
    Const(Const),
    FlatApply(Vec<Pat>),
    List(Vec<Pat>),
    Record(Vec<Row<Pat>>, bool),
    Variable(Symbol),
    Wildcard,
}

impl PatKind {
    pub fn make_record(v: Vec<Pat>, flexible: bool) -> Self {
        let v = v.into_iter().enumerate().map(|(i, e)| Row {
            label: Symbol::tuple_field(1 + i as Size),
            span: e.span,
            item: e,
        });
        Self::Record(v.collect(), flexible)
    }
}
