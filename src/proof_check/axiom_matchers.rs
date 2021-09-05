use std::{rc::Rc};

use mset::MultiSet;

use crate::{
    ast::{ExprNode},
    matcher::{Matcher},
    parser::ExprManager};

use super::{BaseExpr, Based, Wrong};



pub struct ProofChecker<'a>
{
    schemes: Vec<Matcher>,
    hypothesis: &'a mset::MultiSet<ExprNode>,
    manager: &'a ExprManager,
}

impl<'a> ProofChecker<'a>
{
    pub fn new(manager: &'a ExprManager, hypothesis: &'a MultiSet<ExprNode>) -> Self
    {
        let schemes = [
            "a:Meta -> b:Meta -> a:Meta",
            "(a:Meta -> b:Meta) -> (a:Meta -> b:Meta -> c:Meta) -> (a:Meta -> c:Meta)",
            "a:Meta -> b:Meta -> a:Meta & b:Meta",
            "a:Meta & b:Meta -> a:Meta",
            "a:Meta & b:Meta -> b:Meta",
            "a:Meta -> a:Meta | b:Meta",
            "b:Meta -> a:Meta | b:Meta",
            "(a:Meta -> c:Meta) -> (b:Meta -> c:Meta) -> (a:Meta | b:Meta -> c:Meta)",
            "(a:Meta -> b:Meta) -> (a:Meta -> !b:Meta) -> !a:Meta",
            "a:Meta -> !a:Meta -> b:Meta",
        ];
        let schemes = Self::into_matchers(manager, &schemes);

        ProofChecker{ schemes, manager, hypothesis }
    }

    pub fn check_single_expr(&self, checked: &ExprNode) -> BaseExpr
    {
        let proof = self.check_in_hypothesis(checked)
            .or_else(|_| self.match_schemes(checked));
        BaseExpr{ expr: Rc::clone(checked), proof }
    }

//private:

    fn check_in_hypothesis(&self, checked: &ExprNode) -> Result<Based, Wrong>
    {
        if self.hypothesis.contains(checked) {
            Ok(Based::FromHyp)
        } else {
            Err(Wrong::Unproved)
        }
    }

    fn match_schemes(&self, checked: &ExprNode) -> Result<Based, Wrong>
    {
        match self.match_basic(checked, &self.schemes) {
            Some(num) => Ok(Based::Scheme(num)),
            None => Err(Wrong::Unproved),
        }
    }

// proof checking

    fn match_basic(&self, checked: &ExprNode, matchers: &[Matcher]) -> Option<u8>
    {
        let matched = Self::idx_of_pred(
            matchers,
            |matcher| matcher.match_expression(checked).is_ok());

        matched.map(|found| found as u8 + 1)
    }

// util
    fn into_matchers(manager: &ExprManager, as_strs: &[&str]) -> Vec<Matcher>
    {
        as_strs.iter()
            .map(|as_str| manager.matcher_str(*as_str))
            .collect()
    }

    fn idx_of_pred<T, Pred>(slice: &[T], pred: Pred) -> Option<usize>
        where Pred: Fn(&T) -> bool
    {
        slice.iter()
            .enumerate()
            .find_map(|(idx, curr)| {
                if pred(curr) {
                    Some(idx)
                } else {
                    None
                }
            })
    }
}
