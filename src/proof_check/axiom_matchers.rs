use std::{rc::Rc};

use mset::MultiSet;
use smallvec::SmallVec;

use crate::{ast::{ExprNode}, matcher::{Matcher}, parser::ExprManager};

use super::{Based, Wrong};



pub struct ProofChecker<'a>
{
    schemes: Vec<Matcher>,
    hypothesis: &'a mset::MultiSet<ExprNode>,
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

        ProofChecker{ schemes, hypothesis }
    }

    pub fn check_single_expr(&self, checked: &ExprNode) -> Result<Based, Wrong>
    {
        self.check_in_hypothesis(checked)
            .or_else(|_| self.match_schemes(checked))
    }

//private:
    fn check_in_hypothesis(&self, checked: &ExprNode) -> Result<Based, Wrong>
    {
        self.hypothesis.get(checked)
            .ok_or(Wrong::Unproved)
            .map(|_| Based::FromHyp(Rc::clone(checked)))
    }

    fn match_schemes(&self, checked: &ExprNode) -> Result<Based, Wrong>
    {
        self.match_basic(checked)
            .ok_or(Wrong::Unproved)
            .map(|(num, children)| Based::Scheme(num, children))
    }

// proof checking

    fn match_basic(&self, checked: &ExprNode) -> Option<(u8, SmallVec<[ExprNode; 3]>)>
    {
        self.schemes.iter()
            .enumerate()
            .find_map(|(idx, matcher)| {
                matcher.match_expression(checked)
                    .map_or(None, |substs| {
                        let substs = substs.expr_substs();
                        let mut children = SmallVec::new();
                        for name in &["a", "b", "c"] {
                            if let Some(found) = substs.get(*name) {
                                children.push(Rc::clone(found))
                            }
                        }

                        Some((1 + idx as u8, children))
                    })
            })
    }

// util
    fn into_matchers(manager: &ExprManager, as_strs: &[&str]) -> Vec<Matcher>
    {
        as_strs.iter()
            .map(|as_str| manager.matcher_str(*as_str))
            .collect()
    }
}
