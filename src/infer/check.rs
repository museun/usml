use crate::{
    diag::Diagnostic,
    parser::ast,
    span::Span,
    symbols::{Builtin, Symbol},
    Interner,
};
use std::collections::{HashSet, VecDeque};

pub struct Check<'a> {
    interner: &'a mut Interner,
    diags: Vec<Diagnostic>,
}

impl<'a> Check<'a> {
    pub fn new(interner: &'a mut Interner) -> Self {
        Self {
            interner,
            diags: Vec::new(),
        }
    }

    pub fn into_diags(self) -> Vec<Diagnostic> {
        self.diags
    }

    pub fn check_decl(&mut self, decl: &ast::Decl) {
        use ast::DeclKind::*;

        let span = decl.span;
        match &decl.item {
            Datatype(bindings) => {
                self.check_datatype(bindings);
                self.check_datatype_len(span, bindings);
            }

            Function(vars, bindings) => {
                self.check_fn_bindings(span, vars, bindings);
            }

            Value(vars, pat, expr) => {
                self.check_val_bindings(span, vars, pat, expr);
            }

            Exception(vars) => {
                self.check_variants(vars);
                self.check_datatype_len(span, vars);
            }

            Local(left, right) => {
                self.check_decl(left);
                self.check_decl(right);
            }

            Seq(decls) => {
                self.check_many(decls, Self::check_decl);
            }

            Type(..) | Fixity(..) => {}
        }
    }

    fn check_rows<T, F>(&mut self, rows: &[ast::Row<T>], apply: F)
    where
        F: Fn(&mut Self, &T),
    {
        let mut set = HashSet::new();
        for row in rows {
            if set.insert(row.label) {
                apply(self, &row.item);
                continue;
            }

            let err = diag!(
                row.span,
                "duplicate record label: '{}'",
                self.interner.lookup_or(row.label)
            );
            self.diags.push(err)
        }
    }

    fn check_ty_vars(&mut self, span: Span, symbols: &[Symbol]) {
        let mut set = HashSet::new();
        for var in symbols {
            if set.insert(var) {
                continue;
            }

            let err = diag!(
                span,
                "type variable '{}' cannot be rebound",
                self.interner.lookup_or(*var)
            );
            self.diags.push(err)
        }
    }

    fn check_datatype(&mut self, bindings: &[ast::Datatype]) {
        for binding in bindings {
            self.check_ty_vars(binding.span, &binding.ty_vars);
            self.check_variants(&binding.ctors);

            for ctor in &binding.ctors {
                if !Builtin::CONSTRUCTORS.contains(&ctor.label) {
                    continue;
                }

                let err = diag!(
                    ctor.span,
                    "builtin data constructor '{}' cannot be rebound",
                    self.interner.lookup_or(ctor.label)
                );
                self.diags.push(err)
            }
        }
    }

    fn check_fn_bindings(&mut self, span: Span, vars: &[Symbol], bindings: &[ast::Fun]) {
        self.check_ty_vars(span, vars);

        let mut names = HashSet::new();
        for binding in bindings {
            let name = binding[0].name;
            let arity = binding.iter().map(|d| d.pats.len()).max().unwrap_or(1);

            for binding in &**binding {
                if name != binding.name {
                    let err = diag!(
                        binding.span,
                        "function clause with a different name. expected: {}, got: {}",
                        self.interner.lookup_or(name),
                        self.interner.lookup_or(binding.name)
                    );
                    self.diags.push(err);
                }

                let len = binding.pats.len();
                if arity != len {
                    if len == 1 && binding.pats[0].item == ast::PatKind::Wildcard {
                        // we'll have to defer the expansion until later
                        continue;
                    }

                    let err = diag!(
                        binding.span,
                        "function clause with a different number of args. expected: {}, got: {}",
                        arity,
                        len
                    );
                    self.diags.push(err);
                }
            }

            if !names.insert(name) {
                let err = diag!(
                    binding.span,
                    "function '{}' was previously defined in function bindings",
                    self.interner.lookup_or(name)
                );
                self.diags.push(err)
            }
        }
    }

    fn check_val_bindings(
        &mut self,
        span: Span,
        vars: &[Symbol],
        pat: &ast::Pat,
        expr: &ast::Expr,
    ) {
        self.check_ty_vars(span, vars);

        if let ast::PatKind::Variable(p) = pat.item {
            if Builtin::CONSTRUCTORS.contains(&p) {
                let err = diag!(
                    pat.span,
                    "builtin data constructor '{}' cannot be rebound",
                    self.interner.lookup_or(p)
                );
                self.diags.push(err)
            }
        }

        self.check_pat(pat);
        self.check_expr(expr);
    }

    fn check_pat(&mut self, pat: &ast::Pat) {
        use ast::PatKind::*;

        let mut vars = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(pat);

        while let Some(pat) = queue.pop_front() {
            match &pat.item {
                Ascribe(pat, _) => {
                    queue.push_back(pat);
                }
                FlatApply(pats) | List(pats) => {
                    queue.extend(pats);
                }
                Record(rows, _) => {
                    self.check_rows(rows, Self::check_pat);
                }
                &Variable(sym) => {
                    if !sym.is_builtin() && !vars.insert(sym) {
                        let err = diag!(
                            pat.span,
                            "duplicate variable in pattern: '{}'",
                            self.interner.lookup_or(sym)
                        );
                        self.diags.push(err);
                    }
                }
                Const(..) | Wildcard => {}
                Apply(..) => unreachable!(),
            }
        }
    }

    fn check_rule(&mut self, rule: &ast::Rule) {
        self.check_pat(&rule.pat);
        self.check_expr(&rule.expr)
    }

    fn check_expr(&mut self, expr: &ast::Expr) {
        use ast::ExprKind::*;
        match &expr.item {
            AndAlso(left, right) | Apply(left, right) | OrElse(left, right) => {
                self.check_expr(left);
                self.check_expr(right)
            }

            Case(expr, rules) | Handle(expr, rules) => {
                self.check_expr(expr);
                self.check_many(rules, Self::check_rule)
            }

            Fn(rules) => {
                self.check_many(rules, Self::check_rule);
            }

            If(cond, then, else_) => {
                self.check_expr(cond);
                self.check_expr(then);
                self.check_expr(else_);
            }

            Let(decls, expr) => {
                self.check_many(decls, Self::check_decl);
                self.check_expr(expr)
            }

            Record(rows) => {
                self.check_rows(rows, Self::check_expr);
            }

            Constraint(expr, _) | Raise(expr) => {
                self.check_expr(expr);
            }

            FlatApply(exprs) | List(exprs) | Seq(exprs) => {
                self.check_many(exprs, Self::check_expr);
            }

            While(_, _) => {
                let err = diag!(expr.span, "`while` is not an expression");
                self.diags.push(err)
            }

            Var(..) | Selector(..) | Primitive(..) | Const(..) => {}
        }
    }

    fn check_variants(&mut self, vars: &[ast::Variant]) {
        self.check_rows(vars, |_, _| {})
    }

    fn check_many<T, F>(&mut self, data: &[T], f: F)
    where
        F: Fn(&mut Self, &T),
    {
        for data in data {
            f(self, data)
        }
    }

    fn check_datatype_len<T>(&mut self, span: Span, list: &[T]) {
        const MAX_RECURSION: usize = 1 << 8;

        if list.len() >= MAX_RECURSION {
            let err = diag!(
                span,
                "maximum of {} mutually recursive datatypes. got: {}",
                MAX_RECURSION,
                list.len()
            );
            self.diags.push(err)
        }
    }
}
