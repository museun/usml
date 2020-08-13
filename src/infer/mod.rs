#![cfg_attr(debug_assertions, allow(dead_code,))]
use crate::{
    diag::Diagnostic,
    parser::ast::{self, Datatype, Expr, Fun, Pat, PatKind, Row},
    span::Span,
    symbols::{Builtin, Symbol},
    Interner,
};

use ast::{Fixity, Rule, Variant};
use std::collections::{HashMap, HashSet, VecDeque};

use check::Check;
use types::{Constructor, ExprId, TypeId};

mod check;
mod types;

enum ElabError {}
enum UnifyError {}

type Map<V> = HashMap<Symbol, V>;

enum ValueKind {
    Exception(Constructor),
    Ctor(Constructor),
    Var,
}

struct Namespace {
    parent: Option<usize>,
    depth: usize,
    types: Map<TypeId>,
    values: Map<ExprId>,
    infx: Map<Fixity>,
}

struct TypeVar<'a> {
    _marker: std::marker::PhantomData<&'a Self>,
}
struct TypeStruct<'a> {
    _marker: std::marker::PhantomData<&'a Self>,
}

type Arena = bumpalo::Bump;

pub struct Context<'a> {
    arena: &'a Arena,

    ty_vars: Vec<(Symbol, &'a TypeVar<'a>)>,
    namespaces: Vec<Namespace>,
    types: Vec<TypeStruct<'a>>,
    values: Vec<((), ValueKind)>,

    elab_errors: Vec<ElabError>,
    unify_errors: Vec<UnifyError>,

    rank: usize,
    current: usize,
    depth: usize,
}

impl<'a> Context<'a> {
    pub fn new(arena: &'a Arena) -> Self {
        let (ty_vars, namespaces, types, values, elab_errors, unify_errors) = <_>::default();

        Self {
            arena,
            ty_vars,
            namespaces,
            types,
            values,

            elab_errors,
            unify_errors,

            rank: 0,
            current: 0,
            depth: 0,
        }
    }

    fn diagnostics(&self, interner: &'a Interner) -> Vec<Diagnostic> {
        todo!()
    }

    pub fn elaborate_decl(&mut self, decl: &ast::Decl) -> Vec<Decl<'a>> {
        todo!()
    }
}

pub struct Decl<'a> {
    _marker: std::marker::PhantomData<&'a Self>,
}

pub fn check_and_elaborate<'a>(
    arena: &'a Arena,
    decl: &ast::Decl,
    interner: &'a mut Interner,
) -> (Vec<Decl<'a>>, Vec<Diagnostic>) {
    let mut checker = Check::new(interner);
    checker.check_decl(decl);

    let mut ctx = Context::new(arena);
    let decls = ctx.elaborate_decl(decl);

    let mut diags = checker.into_diags();
    diags.extend(ctx.diagnostics(interner));

    (decls, diags)
}
