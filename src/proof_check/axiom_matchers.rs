use std::{ops::BitAnd, rc::Rc};

use crate::{
    ast::{ExprNode, Term, TermVar, TermUnOp},
    matcher::{Matcher, Substitutions, helpers::{SubstContainer}},
    parser::ExprManager};

use super::{BaseExpr, Based, Cringe, QuanRule};



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
            "a:Var = b:Var -> (a:Var)' = (b:Var)'",
            "a:Var = b:Var -> a:Var = c:Var -> b:Var = c:Var",
            "(a:Var)' = (b:Var)' -> a:Var = b:Var",
            "!(a:Var)' = 0",
            "a:Var + (b:Var)' = (a:Var + b:Var)'",
            "a:Var + 0 = a:Var",
            "a:Var * 0 = 0",
            "a:Var * (b:Var)' = a:Var * b:Var + a:Var",

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

    fn match_schemes(&self, checked: &ExprNode) -> Result<Based, Cringe>
    {
        match self.match_basic(checked, &self.schemes[..10]) {
            Some(num) => Ok(Based::Scheme(num)),
            None => self.match_quan_scheme(checked),
        }
    }

    fn match_axioms(&self, checked: &ExprNode) -> Result<Based, Cringe>
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

    fn match_quan_scheme(&self, checked: &ExprNode) -> Result<Based, Cringe>
    {
        // "(@x:Var.orig:Pred) -> substed:Pred",
        // "substed:Pred -> ?x:Var.orig:Pred"
        self.schemes[10..].iter().enumerate()
            .map(|(idx, matcher)| {
                matcher.match_expression(checked)
                    .map_err(|_mismatch| Cringe::casual_cringe())
                    .and_then(|substs| {
                        let quan_var = substs.var_mapping().get_subst(&"x".into()).unwrap();

                        let expr_substs = substs.expr_substs();
                        let orig = expr_substs.get_subst(&"orig".into()).unwrap();
                        let substed = expr_substs.get_subst(&"substed".into()).unwrap();

                        let orig_matcher = self.manager.matcher_node(Rc::clone(orig));

                        let check_freedom_for_subst = |substs: Substitutions| {
                            let substed_term = substs.var_subst().get_subst(quan_var).unwrap();
                            let free_vars = Matcher::all_vars(substed_term);
                            if !(Matcher::all_bound_at_var_mention(orig, *quan_var).bitand(&free_vars)).is_empty() {
                                let rule = if idx == 0 { QuanRule::Any } else { QuanRule::Exist };
                                Err(Cringe::NonFreeToSubst{ var: *quan_var, substed: Rc::clone(substed_term), rule })
                            } else {
                                Ok(Based::Scheme(11 + idx as u8))
                            }
                        };

                        orig_matcher.match_expr_free_var_subst(substed, *quan_var)
                            .validate(|match_res| {
                                match_res
                                    .map_err(|_| Cringe::casual_cringe())
                                    .and_then(check_freedom_for_subst)
                            })
                    })
            })
            .fold(Err(Cringe::casual_cringe()), |res, match_res| {
                match_res.or_else(|match_err| res.map_err(|prev_err| prev_err.min(match_err)))
            })
    }

    fn match_quan_axiom(&self, checked: &ExprNode) -> Result<Based, Cringe>
    {
        // "(zeroed:Pred) & (@x:Var.orig:Pred -> (nexted:Pred)) -> orig:Pred"
        self.axioms[8]
            .match_expression(checked)
            .map_err(|_| Cringe::casual_cringe())
            .and_then(|substs| {
                let quan_var = substs.var_mapping().get_subst(&"x".into()).unwrap();

                let expr_substs = substs.expr_substs();
                let orig = expr_substs.get_subst(&"orig".into()).unwrap();
                let zeroed = expr_substs.get_subst(&"zeroed".into()).unwrap();
                let nexted = expr_substs.get_subst(&"nexted".into()).unwrap();

                let orig_matcher = self.manager.matcher_node(Rc::clone(orig));

                let zeroed = orig_matcher.match_expr_free_var_subst(zeroed, *quan_var)
                    .validate(|match_res| match_res.map_or(false, |subst| {
                        matches!(**subst.var_subst().get_subst(quan_var).unwrap(), Term::Zero)
                    }));

                let nexted = orig_matcher.match_expr_free_var_subst(nexted, *quan_var)
                    .validate(|match_res| match_res.map_or(false, |subst| {
                        use Term::{UnOp, Var};
                        use TermUnOp::Next;

                        let var_subst = subst.var_subst().get_subst(quan_var).unwrap();
                        if let UnOp(Next, sub) = &**var_subst {
                            if let Var(TermVar::Static(sub_var_name, ..)) = &**sub {
                                return sub_var_name == quan_var
                            }
                        }
                        false
                    }));

                if zeroed && nexted {
                    Ok(Based::Axiom(9))
                } else {
                    Err(Cringe::casual_cringe())
                }
            })
            .map_err(|_| Cringe::casual_cringe())
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
