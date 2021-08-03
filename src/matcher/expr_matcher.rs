use std::{collections::{HashMap}, fmt::Display, rc::Rc};

use crate::{ast::{Expr, ExprNode, ExprPred, ExprUnOp, Term, TermNode, TermVar, term::{TermProvider}}};

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
        let mut term_substs = HashMap::new();
        let mut expr_substs = HashMap::new();
        match self.match_exprs(&self.basis, checked, &mut expr_substs, &mut term_substs) {
            Ok(()) => Ok(Substitutions{ terms: term_substs, exprs: expr_substs }),
            Err(err) => Err(err),
        }
    }

// private:
    fn match_exprs(&self, expected: &ExprNode, checked: &ExprNode,
        expr_substs: &mut HashMap<String, ExprNode>,
        term_substs: &mut HashMap<String, TermNode>)-> Result<(), Mismatch>
    {
        use Expr::*;

        match (&**expected, &**checked) {
            (Pred(ExprPred::Dynamic(dyn_name)), _) => {
                match Self::check_substitution(dyn_name.clone(), checked, expr_substs) {
                    Ok(()) => Ok(()),
                    Err((expected, actual)) => Err(Mismatch::Expr{ expected, actual }),
                }
            },
            (UnOp(ExprUnOp::Any(TermVar::Dynamic(dyn_name)), l_sub),
             UnOp(ExprUnOp::Any(TermVar::Static(st_name, ..)), r_sub)) => {

                match Self::check_substitution(dyn_name.clone(), &self.term_provider.var(*st_name), term_substs) {
                    Ok(()) => self.match_exprs(l_sub, r_sub, expr_substs, term_substs),
                    Err((expected, actual)) => Err(Mismatch::Term{ expected, actual })
                }
            },
            (UnOp(ExprUnOp::Ext(TermVar::Dynamic(dyn_name)), l_sub),
             UnOp(ExprUnOp::Ext(TermVar::Static(st_name, ..)), r_sub)) => {
                match Self::check_substitution(dyn_name.clone(), &self.term_provider.var(*st_name), term_substs) {
                    Ok(()) => self.match_exprs(l_sub, r_sub, expr_substs, term_substs),
                    Err((expected, actual)) => Err(Mismatch::Term{ expected, actual })
                }
            },
            (Pred(ExprPred::Static(l, ..)), Pred(ExprPred::Static(r, ..))) if l == r => Ok(()),
            (UnOp(l_op, l_sub), UnOp(r_op, r_sub)) if l_op == r_op => {
                self.match_exprs(l_sub, r_sub, expr_substs, term_substs)
            },
            (BiOp(l_op, l_l, l_r), BiOp(r_op, r_l, r_r)) if l_op == r_op => {
                self.match_exprs(l_l, r_l, expr_substs, term_substs)
                    .and(self.match_exprs(l_r, r_r, expr_substs, term_substs))
            }
            (Eq(l_l, l_r), Eq(r_l, r_r)) => {
                let cmp_res = Self::match_terms(l_l, r_l, term_substs)
                    .and(Self::match_terms(l_r, r_r, term_substs));
                match cmp_res {
                    Ok(()) => Ok(()),
                    Err((expected, actual)) => Err(Mismatch::Term{ expected, actual }),
                }
            }
            _ => Err(Mismatch::Expr{ expected: Rc::clone(expected), actual: Rc::clone(checked) }),
        }
    }

    fn match_terms(expected: &TermNode, checked: &TermNode, term_substs: &mut HashMap<String, TermNode>) -> Result<(), (TermNode, TermNode)>
    {
        use Term::*;

        match (&**expected, &**checked) {
            (Var(TermVar::Dynamic(l_dyn)), _) => {
                Self::check_substitution(l_dyn.clone(), checked, term_substs)
            }
            (Var(TermVar::Static(l, ..)), Var(TermVar::Static(r, ..))) if l == r => Ok(()),
            (UnOp(l_op, l_sub), UnOp(r_op, r_sub)) if l_op == r_op => {
                Self::match_terms(l_sub, r_sub, term_substs)
            }
            (BiOp(l_op, l_l, l_r), BiOp(r_op, r_l, r_r)) if l_op == r_op => {
                Self::match_terms(l_l, r_l, term_substs)
                    .and(Self::match_terms(l_r, r_r, term_substs))
            },
            (Zero, Zero) => Ok(()),
            _ => Err((Rc::clone(expected), Rc::clone(checked)))
        }
    }

    fn check_substitution<Type>(name: String, checked: &Rc<Type>, substs: &mut HashMap<String, Rc<Type>>) -> Result<(), (Rc<Type>, Rc<Type>)>
    {
        if let Some(subst) = substs.get(&name) {
            if Rc::ptr_eq(subst, checked) {
                Ok(())
            } else {
                Err((Rc::clone(subst), Rc::clone(checked)))
            }
        } else {
            substs.insert(name, Rc::clone(checked));
            Ok(())
        }
    }
}

#[derive(Debug)]
pub struct Substitutions
{
    terms: HashMap<String, TermNode>,
    exprs: HashMap<String, ExprNode>,
}

pub trait GetSubsts<Type>
{
    fn get_substs(&self) -> &'_ HashMap<String, Rc<Type>>;

    fn all_substs<Pred>(&self, pred: Pred) -> bool
        where Pred: Fn(&str, &Rc<Type>) -> bool
    {
        self.get_substs().iter().all(|(name, subst)| pred(name, subst))
    }
}

impl GetSubsts<Term> for Substitutions
{
    fn get_substs(&self) -> &'_ HashMap<String, Rc<Term>> {
        &self.terms
    }
}

impl GetSubsts<Expr> for Substitutions
{
    fn get_substs(&self) -> &'_ HashMap<String, Rc<Expr>> {
        &self.exprs
    }
}

#[derive(Debug)]
pub enum Mismatch
{
    Expr{ expected: ExprNode, actual: ExprNode },
    Term{ expected: TermNode, actual: TermNode },
}

pub type MatchResult = Result<Substitutions, Mismatch>;

impl<'a> Display for Matcher<'a>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("basis: {}", self.basis))
    }
}
