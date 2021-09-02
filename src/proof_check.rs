use crate::{ast::{ExprNode}, matcher::Matcher, parser::ExprManager};

pub struct Proof
{
    to_prove: ExprNode,
    pub proof: Vec<ExprNode>,
}

impl Proof
{
    pub fn new(to_prove: ExprNode, proof: Vec<ExprNode>) -> Self
    {
        Proof{ to_prove, proof }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Base
{
    Scheme(u8),
    Axiom(u8),
}

#[derive(Debug, Clone)]
pub struct WrongProof
{

}

pub struct ProofChecker<'a>
{
    schemes: Vec<Matcher<'a>>,
    axioms: Vec<Matcher<'a>>,
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

        ProofChecker{ schemes, axioms }
    }

    pub fn check_proof(&self, proof: &Proof) -> Vec<Result<Base, WrongProof>>
    {
        proof.proof.iter()
            .map(|expr| {
                self.match_schemes(expr)
                    .or_else(|_| self.match_axioms(expr))
            })
            .collect()
    }

//private:

// proof checking

    fn match_schemes(&self, checked: &ExprNode) -> Result<Base, WrongProof>
    {
        match self.match_basic(checked, &self.schemes[..10]) {
            Some(num) => Ok(Base::Scheme(num)),
            None => Err(WrongProof{})
        }
    }

    fn match_axioms(&self, checked: &ExprNode) -> Result<Base, WrongProof>
    {
        match self.match_basic(checked, &self.axioms[..8]) {
            Some(num) => Ok(Base::Axiom(num)),
            None => Err(WrongProof{})
        }
    }

    fn match_basic(&self, checked: &ExprNode, matchers: &[Matcher]) -> Option<u8>
    {
        let matched = Self::idx_of_pred(
            matchers,
            |matcher| matcher.match_expression(checked).is_ok());

        matched.map(|found| found as u8 + 1)
    }

// util
    fn into_matchers<'b>(manager: &'b ExprManager, as_strs: &[&str]) -> Vec<Matcher<'b>>
    {
        as_strs.into_iter()
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
