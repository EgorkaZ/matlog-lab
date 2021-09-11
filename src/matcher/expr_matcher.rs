use std::{fmt::Display, hash::Hash, rc::Rc};

use rustc_hash::FxHashMap;

use crate::{ast::{Expr, ExprNode, ExprVar}};

use super::helpers::{SubstContainer};

#[derive(Debug, Clone)]
pub struct Matcher
{
    basis: ExprNode,
}

impl Matcher
{
    pub fn new(basis: ExprNode) -> Self
    {
        Self{ basis }
    }

    pub fn match_expression(&self, checked: &ExprNode) -> MatchResult
    {
        self.run_matching(checked)
    }

// private:
    fn run_matching(&self, expected: &ExprNode) -> MatchResult
    {
        let mut expr_substs = Default::default();
        self.match_exprs(&self.basis, expected, &mut expr_substs)
            .map(|_| Substitutions{ exprs: expr_substs })
    }

    fn match_exprs(&self, expected: &ExprNode, checked: &ExprNode,
        expr_substs: &mut FxHashMap<String, ExprNode>,)-> Result<(), Mismatch>
    {
        use Expr::*;

        match (&**expected, &**checked) {
            (Variable(ExprVar::Meta(dyn_name)), _) => {
                Self::check_substitution(dyn_name, checked, expr_substs, Rc::ptr_eq)
            },
            (Variable(ExprVar::Static(l, ..)), Variable(ExprVar::Static(r, ..))) if l == r => Ok(()),
            (UnOp(l_op, l_sub), UnOp(r_op, r_sub)) if l_op == r_op => {
                self.match_exprs(l_sub, r_sub, expr_substs)
            },
            (BiOp(l_op, l_l, l_r), BiOp(r_op, r_l, r_r)) if l_op == r_op => {
                self.match_exprs(l_l, r_l, expr_substs)
                    .and_then(|_| self.match_exprs(l_r, r_r, expr_substs))
            }
            _ => Err(Mismatch::Expr{ expected: Rc::clone(expected), actual: Rc::clone(checked) }),
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
    exprs: FxHashMap<String, ExprNode>,
}

impl Substitutions
{
    pub fn expr_substs(&self) -> &'_ FxHashMap<String, ExprNode>
    { &self.exprs }
}

#[derive(Debug)]
pub enum Mismatch
{
    Expr{ expected: ExprNode, actual: ExprNode },
    Variable{ expected: char, actual: char },
}

pub type MatchResult = Result<Substitutions, Mismatch>;

impl From<(ExprNode, ExprNode)> for Mismatch
{
    fn from((expected, actual): (ExprNode, ExprNode)) -> Self {
        Mismatch::Expr{ expected, actual }
    }
}

impl From<(char, char)> for Mismatch
{
    fn from((expected, actual): (char, char)) -> Self {
        Mismatch::Variable{ expected, actual }
    }
}

impl<'a> Display for Matcher
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("basis: {}", self.basis))
    }
}


pub struct VarSubstResult
{
    match_result: MatchResult,
}

impl VarSubstResult
{
    pub fn validate<Validator, Res>(self, validator: Validator) -> Res
        where Validator: FnOnce(MatchResult) -> Res
    {
        validator(self.match_result)
    }
}
