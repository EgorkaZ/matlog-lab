use std::{collections::{HashMap}, fmt::Display, hash::Hash, rc::Rc};

use crate::{ast::{Expr, ExprNode, ExprPred, ExprUnOp, Term, TermNode, TermVar, term::{TermProvider}}};

use super::helpers::SubstContainer;

#[derive(Debug, Clone)]
pub struct Matcher<'a>
{
    term_provider: &'a TermProvider,
    basis: ExprNode,
}

impl<'a> Matcher<'a>
{
    pub fn new(basis: ExprNode, term_provider: &'a TermProvider) -> Self
    {
        Self{ term_provider, basis }
    }

    pub fn match_expression(&self, checked: &ExprNode) -> MatchResult
    {
        self.run_matching(checked, VarSubst::Bound)
    }

    pub fn match_expr_free_var_subst(&self, checked: &ExprNode, var_name: char) -> MatchResult
    {
        self.run_matching(checked, VarSubst::new(var_name))
    }

// private:
    fn run_matching(&self, expected: &ExprNode, mut var_subst: VarSubst) -> MatchResult
    {
        let mut vars_substs = HashMap::new();
        let mut expr_substs = HashMap::new();
        self.match_exprs(&self.basis, expected, &mut expr_substs, &mut vars_substs, &mut var_subst)
            .map(|_| Substitutions{ vars: vars_substs, exprs: expr_substs, var: var_subst })
    }

    fn match_exprs(&self, expected: &ExprNode, checked: &ExprNode,
        expr_substs: &mut HashMap<String, ExprNode>,
        vars_substs: &mut HashMap<String, char>,
        var_subst: &mut VarSubst)-> Result<(), Mismatch>
    {
        use Expr::*;

        let mut bound = VarSubst::Bound;
        let var_subst = {
            use ExprUnOp::*;
            match &**expected {
                UnOp(Any(TermVar::Static(name, ..)) | Ext(TermVar::Static(name, ..)), ..)
                    if var_subst.has_name(*name) => &mut bound,
                _ => var_subst
            }
        };

        match (&**expected, &**checked) {
            (Pred(ExprPred::Dynamic(dyn_name)), _) => {
                Self::check_substitution(dyn_name, checked, expr_substs, Rc::ptr_eq)
            },
            (UnOp(ExprUnOp::Any(TermVar::Dynamic(dyn_name)), l_sub),
             UnOp(ExprUnOp::Any(TermVar::Static(st_name, ..)), r_sub)) => {

                Self::check_substitution(dyn_name, st_name, vars_substs, char::eq)
                    .and_then(|()| self.match_exprs(l_sub, r_sub, expr_substs, vars_substs, var_subst))
            },
            (UnOp(ExprUnOp::Ext(TermVar::Dynamic(dyn_name)), l_sub),
             UnOp(ExprUnOp::Ext(TermVar::Static(st_name, ..)), r_sub)) => {
                Self::check_substitution(dyn_name, st_name, vars_substs, char::eq)
                    .and_then(|()| self.match_exprs(l_sub, r_sub, expr_substs, vars_substs, var_subst))
            },
            (Pred(ExprPred::Static(l, ..)), Pred(ExprPred::Static(r, ..))) if l == r => Ok(()),
            (UnOp(l_op, l_sub), UnOp(r_op, r_sub)) if l_op == r_op => {
                self.match_exprs(l_sub, r_sub, expr_substs, vars_substs, var_subst)
            },
            (BiOp(l_op, l_l, l_r), BiOp(r_op, r_l, r_r)) if l_op == r_op => {
                self.match_exprs(l_l, r_l, expr_substs, vars_substs, var_subst)
                    .and(self.match_exprs(l_r, r_r, expr_substs, vars_substs, var_subst))
            }
            (Eq(l_l, l_r), Eq(r_l, r_r)) => {
                Self::match_terms(l_l, r_l, vars_substs, var_subst)
                    .and_then(|()| Self::match_terms(l_r, r_r, vars_substs, var_subst))
            }
            _ => Err(Mismatch::Expr{ expected: Rc::clone(expected), actual: Rc::clone(checked) }),
        }
    }

    fn match_terms(expected: &TermNode, checked: &TermNode,
        vars_substs: &mut HashMap<String, char>,
        var_subst: &mut VarSubst) -> Result<(), Mismatch>
    {
        use Term::*;

        match (&**expected, &**checked) {
            (Var(TermVar::Dynamic(l_dyn)), Var(TermVar::Static(r_st, ..))) => {
                Self::check_substitution(l_dyn, r_st, vars_substs, char::eq)
            }
            (Var(TermVar::Static(l, ..)), Var(TermVar::Static(r, ..))) if l == r => Ok(()),
            (UnOp(l_op, l_sub), UnOp(r_op, r_sub)) if l_op == r_op => {
                Self::match_terms(l_sub, r_sub, vars_substs, var_subst)
            }
            (BiOp(l_op, l_l, l_r), BiOp(r_op, r_l, r_r)) if l_op == r_op => {
                Self::match_terms(l_l, r_l, vars_substs, var_subst)
                    .and(Self::match_terms(l_r, r_r, vars_substs, var_subst))
            },
            (Zero, Zero) => Ok(()),
            (Var(TermVar::Static(var_name, ..)), ..) if var_subst.has_name(*var_name) => {
                Self::check_substitution(var_name, checked, var_subst, Rc::ptr_eq)
            }
            _ => Err(Mismatch::from((Rc::clone(expected), Rc::clone(checked))))
        }
    }

    fn check_substitution<Key, Substituted, Cmp>(
            name: &Key,
            checked: &Substituted,
            substs: &mut impl SubstContainer<Key, Substituted>,
            eq: Cmp) -> Result<(), Mismatch>
        where Substituted: Clone,
              Key: Clone + Eq + Hash,
              Cmp: FnOnce(&Substituted, &Substituted) -> bool,
              Mismatch: From<(Substituted, Substituted)>
    {
        substs.check_substitution(name, checked, eq)
            .map_err(|expected| Mismatch::from((expected, checked.clone())))
    }
}

#[derive(Debug)]
pub struct Substitutions
{
    vars: HashMap<String, char>,
    exprs: HashMap<String, ExprNode>,
    var: VarSubst,
}

pub trait GetSubsts<Key, Substituted>
{
    fn get_substs(&self) -> &'_ HashMap<String, Substituted>;

    fn all_substs<Pred>(&self, pred: Pred) -> bool
        where Pred: Fn(&str, &Substituted) -> bool
    {
        self.get_substs().iter().all(|(name, subst)| pred(name, subst))
    }
}

impl GetSubsts<String, char> for Substitutions
{
    fn get_substs(&self) -> &'_ HashMap<String, char> {
        &self.vars
    }
}

impl GetSubsts<String, ExprNode> for Substitutions
{
    fn get_substs(&self) -> &'_ HashMap<String, ExprNode> {
        &self.exprs
    }
}

#[derive(Debug)]
pub enum Mismatch
{
    Expr{ expected: ExprNode, actual: ExprNode },
    Term{ expected: TermNode, actual: TermNode },
    Variable{ expected: char, actual: char },
}

pub type MatchResult = Result<Substitutions, Mismatch>;

impl From<(ExprNode, ExprNode)> for Mismatch
{
    fn from((expected, actual): (ExprNode, ExprNode)) -> Self {
        Mismatch::Expr{ expected, actual }
    }
}

impl From<(TermNode, TermNode)> for Mismatch
{
    fn from((expected, actual): (TermNode, TermNode)) -> Self {
        Mismatch::Term{ expected, actual }
    }
}

impl From<(char, char)> for Mismatch
{
    fn from((expected, actual): (char, char)) -> Self {
        Mismatch::Variable{ expected, actual }
    }
}

#[derive(Debug, Clone)]
enum VarSubst
{
    Bound,
    Free{ name: char, subst: Option<TermNode> },
}

impl VarSubst
{
    fn new(name: char) -> Self
    {
        Self::Free{ name, subst: None }
    }

    fn has_name(&self, name: char) -> bool
    {
        match &self {
            Self::Free{ name: actual_name, .. } => name == *actual_name,
            Self::Bound => false,
        }
    }
}

impl SubstContainer<char, TermNode> for VarSubst
{
    fn get_subst(&self, key: &char) -> Option<&'_ TermNode> {
        match &self {
            Self::Free{ name, subst } if *name == *key => {
                if let Some(subst) = &subst {
                    Some(subst)
                } else {
                    None
                }
            },
            _ => None
        }
    }

    fn substitute(&mut self, key: &char, subst: Rc<Term>) {
        match self {
            Self::Free{ name, subst: found } if *name == *key => {
                *found = Some(subst);
            },
            _ => (),
        }
    }
}

impl<'a> Display for Matcher<'a>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("basis: {}", self.basis))
    }
}
