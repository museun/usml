use crate::{parser::ast::Const, span::Span, symbols::Symbol};
use std::collections::VecDeque;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct TypeId(pub u32);

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ExprId(pub u32);

pub type Var<'a> = (Symbol, &'a Type<'a>);

#[derive(Debug, PartialEq)]
pub enum Type<'a> {
    Var(&'a TypeVar<'a>),
    Ctor(TyCtor, Vec<&'a Type<'a>>),
    Record(Record<&'a Type<'a>>),
    Flex(Flex<'a>),
}

impl<'a> Type<'a> {
    pub fn as_ty_var(&self) -> &TypeVar<'a> {
        match self {
            Self::Var(ty) => ty,
            _ => panic!("internal compiler error: Type is not a variable"),
        }
    }

    pub fn split_arrow(&self) -> Option<(&'_ Type<'a>, &'_ Type<'a>)> {
        match self {
            Self::Ctor(TY_ARROW, types) => match types.as_slice() {
                [left, right] => Some((left, right)),
                _ => None,
            },
            Self::Var(ty) => ty.ty().and_then(Self::split_arrow),
            _ => None,
        }
    }

    pub fn occurs(&mut self, ty: &mut TypeVar) -> bool {
        match self {
            Type::Var(ref mut var) => match var.ty_mut() {
                Some(left) => left.occurs(ty),
                None => {
                    // TODO this should probably emit an dianogistic about self promotion
                    var.rank = std::cmp::min(var.rank(), ty.rank());
                    var.id == ty.id
                }
            },
            Type::Ctor(_, types) => types.iter().any(|left| left.occurs(ty)),
            Type::Record(rows) => rows.item_iter().any(|left| left.occurs(ty)),
            Type::Flex(flex) => match flex.ty() {
                Some(left) => left.occurs(ty),
                None => flex.constraints.item_iter().any(|left| left.occurs(ty)),
            },
        }
    }

    pub fn visit<F: Fn(&Self)>(&self, visit: F) {
        let mut queue = VecDeque::new();
        queue.push_back(self);

        while let Some(ty) = queue.pop_front() {
            visit(ty);

            match ty {
                Type::Var(var) => queue.extend(var.ty()),
                Type::Ctor(_, types) => queue.extend(types),
                Type::Record(rows) => queue.extend(rows.item_iter()),
                Type::Flex(flex) => match flex.ty() {
                    Some(link) => queue.push_back(link),
                    None => queue.extend(flex.constraints.item_iter()),
                },
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct TypeVar<'a> {
    pub id: usize,
    pub rank: usize,
    pub ty: Option<&'a Type<'a>>,
}

impl<'a> TypeVar<'a> {
    fn rank(&self) -> usize {
        self.rank
    }

    fn ty_mut(&mut self) -> &mut Option<&'a Type<'a>> {
        &mut self.ty
    }

    fn ty(&self) -> Option<&'a Type<'a>> {
        self.ty
    }
}

#[derive(Debug, PartialEq)]
pub enum Scheme<'a> {
    Monomorphic(&'a Type<'a>),
    Polymorphic(Vec<usize>, &'a Type<'a>),
}

impl<'a> Scheme<'a> {
    fn new(ty: &'a Type<'a>, vars: Vec<usize>) -> Self {
        if vars.is_empty() {
            Self::Monomorphic(ty)
        } else {
            Self::Polymorphic(vars, ty)
        }
    }

    fn arity(&self) -> usize {
        match self {
            Self::Monomorphic(_) => 0,
            Self::Polymorphic(vars, _) => vars.len(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Lambda<'a> {
    pub arg: Symbol,
    pub ty: &'a Type<'a>,
    pub body: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub enum PatKind<'a> {
    Apply(Constructor, Option<Pat<'a>>),
    Const(Const),
    Record(Record<Pat<'a>>),
    Variable(Symbol),
    Wildcard,
}

#[derive(Debug, PartialEq)]
pub struct Pat<'a> {
    pub kind: &'a PatKind<'a>,
    pub ty: &'a Type<'a>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct Rule<'a> {
    pub pat: Pat<'a>,
    pub expr: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Datatype<'a> {
    pub ty_cor: TyCtor,
    pub ty_vars: Vec<usize>,
    pub ctors: Vec<(Constructor, Option<&'a Type<'a>>)>,
}

#[derive(Debug, PartialEq)]
pub enum Decl<'a> {
    Datatype(Vec<Datatype<'a>>),
    Function(Vec<usize>, Vec<(Symbol, Lambda<'a>)>),
    Value(Vec<usize>, Rule<'a>),
    Exception(Constructor, Option<&'a Type<'a>>),
}

#[derive(Debug, PartialEq)]
pub struct Record<T> {
    pub rows: Vec<Row<T>>,
}

impl<T> Record<T> {
    fn iter(&self) -> impl Iterator<Item = &Row<T>> + '_ {
        self.rows.iter()
    }

    fn item_iter(&self) -> impl Iterator<Item = &T> + '_ {
        self.rows.iter().map(|Row { item, .. }| item)
    }
}

#[derive(Debug, PartialEq)]
pub struct Row<T> {
    pub label: Symbol,
    pub item: T,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct Flex<'a> {
    pub constraints: Record<&'a Type<'a>>,
    pub unified: Option<&'a Type<'a>>,
}

impl<'a> Flex<'a> {
    fn ty(self) -> Option<&'a Type<'a>> {
        self.unified
    }
}

#[derive(Debug, PartialEq)]
pub struct TyCtor {
    pub name: Symbol,
    pub arity: usize,
    pub depth: usize,
}

#[derive(Debug, PartialEq)]
pub struct Constructor {
    pub name: Symbol,
    pub type_id: TypeId,
    pub ty_ctor: Symbol,
    pub tag: u8,
    pub arity: u8,
    pub ty_arity: u8,
}

#[derive(Debug, PartialEq)]
pub struct Expr<'a> {
    pub kind: &'a ExprKind<'a>,
    pub ty: &'a Type<'a>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum ExprKind<'a> {
    Apply(Expr<'a>, Expr<'a>),
    Case(Expr<'a>, Vec<Rule<'a>>),
    Ctor(Constructor, Vec<&'a Type<'a>>),
    Const(Const),
    Handle(Expr<'a>, Symbol, Expr<'a>),
    Let(Vec<Decl<'a>>, Expr<'a>),
    List(Vec<Expr<'a>>),
    Primitive(Symbol),
    Raise(Expr<'a>),
    Record(Vec<Row<Expr<'a>>>),
    Seq(Vec<Expr<'a>>),
    Var(Symbol),
}
