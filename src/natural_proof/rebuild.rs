use std::rc::Rc;

use mset::MultiSet;
use rustc_hash::FxHashMap;

use crate::{ast::{ExprNode, ExprProvider}, proof_check::Based};

use super::base::{BaseNode, BaseProvider};

pub struct ProofRebuilder<'a>
{
    classic_proof: &'a FxHashMap<ExprNode, Based>,
    expr_provider: &'a ExprProvider,
    base_provider: BaseProvider,
}

impl<'a> ProofRebuilder<'a> {
    pub fn new(
        classic_proof: &'a FxHashMap<ExprNode, Based>,
        expr_provider: &'a ExprProvider,
        base_provider: BaseProvider) -> Self
    { Self { classic_proof, expr_provider, base_provider } }

    pub fn rebuild(&self, to_prove: &ExprNode, hypothesis: &[ExprNode])
    {

    }

    fn rebuild_rec(&self, to_prove: &ExprNode, hypothesis: MultiSet<ExprNode>) -> BaseNode
    {
        let classic_base = self.classic_proof.get(to_prove).unwrap_or_else(|_| panic!("Couldn't find proof for: {}", to_prove));
        match classic_base {
            Based::FromHyp(same) =>
                self.base_provider.provide(
                    "Ax",
                    Default::default(),
                    Rc::clone(to_prove),
                    None),
            Based::MP{from, imp} =>
                self.base_provider.provide(
                    "E->",
                    smallvec![
                        self.rebuild_rec(imp, hypothesis.clone()),
                        self.rebuild_rec(from, hypothesis.clone())],
                    to_prove,
                    None),
            Based::Scheme(num, used) => {
                match num {
                    1 if used.len() == 2 => {

                    }
                }
            }
        }
    }
}

trait AxiomRebuild<const num: u8>
{
    fn rebuild_axiom(&self, rebuilt: &ExprNode, used: &[ExprNode]) -> BaseNode;
}

struct AxiomRebuilder<const num: u8>;

impl<'a> AxiomRebuild<1> for ProofRebuilder<'a>
{
    fn rebuild_axiom(&self, rebuilt: &ExprNode, used: &[ExprNode]) -> BaseNode {
        match used {
            &[a, b] => self.base_provider.provide(
                "I->", smallvec![
                    self.base_provider.provide(
                        
                    )
                ],
                rebuilt,
                None)
        }
    }
}
