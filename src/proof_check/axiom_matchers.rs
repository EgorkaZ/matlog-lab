use std::{fmt::Display, ops::BitAnd, rc::Rc};

use crate::{
    ast::{ExprNode, Term, TermVar, TermUnOp},
    matcher::{Matcher, Substitutions, helpers::{SubstContainer}},
    parser::ExprManager};

use super::{BaseExpr, Based, Wrong, QuanRule};



pub struct ProofChecker<'a>
{
    schemes: Vec<Matcher<'a>>,
    axioms: Vec<Matcher<'a>>,
    manager: &'a ExprManager,
}

impl<'a> ProofChecker<'a>
{
    pub fn new(manager: &'a ExprManager) -> Self
    {
        let schemes = [
            "a:Pred -> b:Pred -> a:Pred",
            "(a:Pred -> b:Pred) -> (a:Pred -> b:Pred -> c:Pred) -> (a:Pred -> c:Pred)",
            "a:Pred -> b:Pred -> a:Pred & b:Pred",
            "a:Pred & b:Pred -> a:Pred",
            "a:Pred & b:Pred -> b:Pred",
            "a:Pred -> a:Pred | b:Pred",
            "b:Pred -> a:Pred | b:Pred",
            "(a:Pred -> c:Pred) -> (b:Pred -> c:Pred) -> (a:Pred | b:Pred -> c:Pred)",
            "(a:Pred -> b:Pred) -> (a:Pred -> !b:Pred) -> !a:Pred",
            "!!a:Pred -> a:Pred",

            "(@x:Var.orig:Pred) -> substed:Pred",
            "substed:Pred -> ?x:Var.orig:Pred"
        ];
        let schemes = Self::into_matchers(manager, &schemes);

        let axioms = [
            "a = b -> a' = b'",
            "a = b -> a = c -> b = c",
            "a' = b' -> a = b",
            "!a' = 0",
            "a + b' = (a + b)'",
            "a + 0 = a",
            "a * 0 = 0",
            "a * b' = a * b + a",

            "(zeroed:Pred) & (@x:Var.orig:Pred -> (nexted:Pred)) -> orig:Pred",
        ];
        let axioms = Self::into_matchers(manager, &axioms);

        ProofChecker{ schemes, axioms, manager }
    }

    pub fn match_schemes_and_axioms(&self, checked: &ExprNode) -> BaseExpr
    {
        let proof = self.match_schemes(checked)
            .or_else(|schemes_err| self.match_axioms(checked).map_err(|ax_err| schemes_err.min(ax_err)));
        BaseExpr{ expr: Rc::clone(checked), proof }
    }

//private:

// proof checking

    fn match_schemes(&self, checked: &ExprNode) -> Result<Based, Wrong>
    {
        match self.match_basic(checked, &self.schemes[..10]) {
            Some(num) => Ok(Based::Scheme(num)),
            None => self.match_quan_scheme(checked),
        }
    }

    fn match_axioms(&self, checked: &ExprNode) -> Result<Based, Wrong>
    {
        match self.match_basic(checked, &self.axioms[..8]) {
            Some(num) => Ok(Based::Axiom(num)),
            None => self.match_quan_axiom(checked),
        }
    }

    fn match_basic(&self, checked: &ExprNode, matchers: &[Matcher]) -> Option<u8>
    {
        let matched = Self::idx_of_pred(
            matchers,
            |matcher| matcher.match_expression(checked).is_ok());

        matched.map(|found| found as u8 + 1)
    }

    fn match_quan_scheme(&self, checked: &ExprNode) -> Result<Based, Wrong>
    {
        // "(@x:Var.orig:Pred) -> substed:Pred",
        // "substed:Pred -> ?x:Var.orig:Pred"
        self.schemes[10..].iter().enumerate()
            .map(|(idx, matcher)| {
                matcher.match_expression(checked)
                    .map_err(|_mismatch| Wrong::Unproved)
                    .and_then(|substs| {
                        let x_key: String = "x".into();
                        let orig_key: String = "orig".into();
                        let substed_key: String = "substed".into();
                        let quan_var = substs.var_mapping().get_subst(&x_key)
                            .unwrap_or_else(|| panic_on_match_failure(&substs, &x_key));

                        let expr_substs = substs.expr_substs();
                        let orig = expr_substs.get_subst(&orig_key)
                            .unwrap_or_else(|| panic_on_match_failure(&substs, &orig_key));
                        let substed = expr_substs.get_subst(&substed_key)
                            .unwrap_or_else(|| panic_on_match_failure(&substs, &substed_key));

                        let orig_matcher = self.manager.matcher_node(Rc::clone(orig));

                        let check_freedom_for_subst = |substs: Substitutions| {
                            let based = Based::Scheme(11 + idx as u8);
                            substs.var_subst().get_subst(quan_var)
                                .map_or(Ok(based), |substed_term| {
                                    let free_vars = Matcher::all_vars(substed_term);
                                    if !(Matcher::all_bound_at_var_mention(orig, *quan_var).bitand(&free_vars)).is_empty() {
                                        let rule = if idx == 0 { QuanRule::Any } else { QuanRule::Exist };
                                        Err(Wrong::NonFreeToSubst{ var: *quan_var, substed: Rc::clone(substed_term), rule })
                                    } else {
                                        Ok(Based::Scheme(11 + idx as u8))
                                    }
                                })
                        };

                        orig_matcher.match_expr_free_var_subst(substed, *quan_var)
                            .validate(|match_res| {
                                match_res
                                    .map_err(|_| Wrong::Unproved)
                                    .and_then(check_freedom_for_subst)
                            })
                    })
            })
            .fold(Err(Wrong::Unproved), |res, match_res| {
                match_res.or_else(|match_err| res.map_err(|prev_err| prev_err.min(match_err)))
            })
    }

    fn match_quan_axiom(&self, checked: &ExprNode) -> Result<Based, Wrong>
    {
        // "(zeroed:Pred) & (@x:Var.orig:Pred -> (nexted:Pred)) -> orig:Pred"
        self.axioms[8]
            .match_expression(checked)
            .map_err(|_| Wrong::Unproved)
            .and_then(|substs| {
                let x_key = "x".into();
                let orig_key = "orig".into();
                let zeroed_key = "zeroed".into();
                let nexted_key = "nexted".into();

                let quan_var = substs.var_mapping().get_subst(&x_key)
                    .unwrap_or_else(|| panic_on_match_failure(&substs, &x_key));

                let expr_substs = substs.expr_substs();
                let orig = expr_substs.get_subst(&orig_key)
                    .unwrap_or_else(|| panic_on_match_failure(&substs, &orig_key));
                let zeroed = expr_substs.get_subst(&zeroed_key)
                    .unwrap_or_else(|| panic_on_match_failure(&substs, &zeroed_key));
                let nexted = expr_substs.get_subst(&nexted_key)
                    .unwrap_or_else(|| panic_on_match_failure(&substs, &nexted_key));

                let orig_matcher = self.manager.matcher_node(Rc::clone(orig));

                let zeroed = orig_matcher.match_expr_free_var_subst(zeroed, *quan_var)
                    .validate(|match_res| match_res.map_or(false, |subst| {
                        subst.var_subst().get_subst(quan_var)
                            .map_or(true, |substed_term| matches!(**substed_term, Term::Zero))
                    }));

                let nexted = orig_matcher.match_expr_free_var_subst(nexted, *quan_var)
                    .validate(|match_res| match_res.map_or(false, |subst| {
                        use Term::{UnOp, Var};
                        use TermUnOp::Next;

                        subst.var_subst().get_subst(quan_var)
                            .map_or(true, |var_subst| {
                                if let UnOp(Next, sub) = &**var_subst {
                                    if let Var(TermVar::Static(sub_var_name, ..)) = &**sub {
                                        return sub_var_name == quan_var
                                    }
                                }
                                false
                            })
                    }));

                if zeroed && nexted {
                    Ok(Based::Axiom(9))
                } else {
                    Err(Wrong::Unproved)
                }
            })
            .map_err(|_| Wrong::Unproved)
    }

// util
    fn into_matchers<'b>(manager: &'b ExprManager, as_strs: &[&str]) -> Vec<Matcher<'b>>
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

fn panic_on_match_failure<Key: Display, Ret>(subst: &Substitutions, key: &Key) -> Ret
{
    panic!("get_subst('{}').unwrap() failed. Substitutions: {}", key, subst);
}
