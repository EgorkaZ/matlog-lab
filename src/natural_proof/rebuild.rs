use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::{ast::{ExprNode, ExprProvider}, proof_check::Based};

use super::base::{BaseNode, BaseProvider};

pub fn rebuild_to_natural(classic_proof: &FxHashMap<ExprNode, Based>, expr_provider: &ExprProvider, initial: &ExprNode) -> BaseNode
{
    ProofRebuilder::new(classic_proof, expr_provider, Default::default())
        .rebuild(initial)
}

struct ProofRebuilder<'a>
{
    classic_proof: &'a FxHashMap<ExprNode, Based>,
    expr_provider: &'a ExprProvider,
    base_provider: BaseProvider,
}

impl<'a> ProofRebuilder<'a> {
    fn new(
        classic_proof: &'a FxHashMap<ExprNode, Based>,
        expr_provider: &'a ExprProvider,
        base_provider: BaseProvider) -> Self
    { Self { classic_proof, expr_provider, base_provider } }

    fn rebuild(&self, to_prove: &ExprNode) -> BaseNode
    {
        self.rebuild_rec(to_prove)
    }

    fn rebuild_rec(&self, to_prove: &ExprNode) -> BaseNode
    {
        let classic_base = self.classic_proof.get(to_prove).unwrap_or_else(|| panic!("Couldn't find proof for: {}", to_prove));
        match classic_base {
            Based::FromHyp(same) => {
                assert_eq!(same, to_prove, "to_prove: {}, FromHyp base: {}", same, to_prove);
                self.base_provider.provide(
                    "Ax",
                    Default::default(),
                    Rc::clone(to_prove))
            },
            Based::MP{from, imp} => {
                let from_base = self.rebuild_rec(from);
                let imp_base = self.rebuild_rec(imp);

                let mp_base = self.base_provider.provide(
                    "E->", smallvec![(imp_base, None), (from_base, None)], Rc::clone(to_prove));

                mp_base
            },
            Based::Scheme(num, used) => {
                match num {
                    1 if used.len() == 2 => <AxiomRebuilder<1>>::rebuild_axiom(self, used),
                    2 if used.len() == 3 => <AxiomRebuilder<2>>::rebuild_axiom(self, used),
                    3 if used.len() == 2 => <AxiomRebuilder<3>>::rebuild_axiom(self, used),
                    4 if used.len() == 2 => <AxiomRebuilder<4>>::rebuild_axiom(self, used),
                    5 if used.len() == 2 => <AxiomRebuilder<5>>::rebuild_axiom(self, used),
                    6 if used.len() == 2 => <AxiomRebuilder<6>>::rebuild_axiom(self, used),
                    7 if used.len() == 2 => <AxiomRebuilder<7>>::rebuild_axiom(self, used),
                    8 if used.len() == 3 => <AxiomRebuilder<8>>::rebuild_axiom(self, used),
                    9 if used.len() == 2 => <AxiomRebuilder<9>>::rebuild_axiom(self, used),
                    10 if used.len() == 2 => <AxiomRebuilder<10>>::rebuild_axiom(self, used),
                    _ => panic!("Unexpected scheme. num: {}, to_prove: {}, used: {:?}", num, to_prove, used),
                }
            }
        }
    }
}

trait AxiomRebuild
{
    fn rebuild_axiom(rebuilder: &ProofRebuilder, used: &[ExprNode]) -> BaseNode;
}

struct AxiomRebuilder<const NUM: u8>;

impl AxiomRebuild for AxiomRebuilder<1>
{
    fn rebuild_axiom(rebuilder: &ProofRebuilder, used: &[ExprNode]) -> BaseNode {
        let (a, b) = (&used[0], &used[1]);
        let a_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], Rc::clone(a));

        let b_a = rebuilder.expr_provider.imp(b, a);
        let b_a = rebuilder.base_provider.provide(
            "I->", smallvec![(a_base, Some(Rc::clone(b)))], b_a);

        let a_b_a = rebuilder.expr_provider.imp(a, b_a.curr());
        let a_b_a = rebuilder.base_provider.provide(
            "I->", smallvec![(b_a, Some(Rc::clone(a)))], a_b_a);

        a_b_a
    }
}

impl AxiomRebuild for AxiomRebuilder<2>
{
    fn rebuild_axiom(rebuilder: &ProofRebuilder, used: &[ExprNode]) -> BaseNode {
        let (a, b, c) = (&used[0], &used[1], &used[2]);

        let b_c = rebuilder.expr_provider.imp(b, c);
        let a_b_c = rebuilder.expr_provider.imp(a, &b_c);
        let a_b = rebuilder.expr_provider.imp(a, b);

        let a_b_c_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], Rc::clone(&a_b_c));
        let a_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], Rc::clone(a));
        let b_c_base = rebuilder.base_provider.provide(
            "E->", smallvec![(a_b_c_base, None), (Rc::clone(&a_base), None)], b_c);

        let a_b_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], Rc::clone(&a_b));
        let b_base = rebuilder.base_provider.provide(
            "E->", smallvec![(a_b_base, None), (a_base, None)], Rc::clone(b));

        let c_base = rebuilder.base_provider.provide(
            "E->", smallvec![(b_c_base, None), (b_base, None)], Rc::clone(c));

        let a_c = rebuilder.expr_provider.imp(a, c);
        let a_c_base = rebuilder.base_provider.provide(
            "I->", smallvec![(c_base, Some(Rc::clone(a)))], Rc::clone(&a_c));

        let a_b_c__a_c = rebuilder.expr_provider.imp(&a_b_c, &a_c);
        let a_b_c__a_c_base = rebuilder.base_provider.provide(
            "I->", smallvec![(a_c_base, Some(a_b_c))], Rc::clone(&a_b_c__a_c));

        let a_b__a_b_c__a_c = rebuilder.expr_provider.imp(&a_b, &a_b_c__a_c);
        let a_b__a_b_c__a_c_base = rebuilder.base_provider.provide(
            "I->", smallvec![(a_b_c__a_c_base, Some(a_b))], a_b__a_b_c__a_c);

        a_b__a_b_c__a_c_base
    }
}

impl AxiomRebuild for AxiomRebuilder<3>
{
    fn rebuild_axiom(rebuilder: &ProofRebuilder, used: &[ExprNode]) -> BaseNode {
        let (a, b) = (&used[0], &used[1]);

        let a_b = rebuilder.expr_provider.conj(a, b);
        let a_b_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], Rc::clone(&a_b));

        let a_base = rebuilder.base_provider.provide(
            "El&", smallvec![(a_b_base, None)], Rc::clone(a));

        let a_b__a = rebuilder.expr_provider.imp(&a_b, a);
        let a_b__a_base = rebuilder.base_provider.provide(
            "I->", smallvec![(a_base, Some(a_b))], a_b__a);

        a_b__a_base
    }
}

impl AxiomRebuild for AxiomRebuilder<4>
{
    fn rebuild_axiom(rebuilder: &ProofRebuilder, used: &[ExprNode]) -> BaseNode {
        let (a, b) = (&used[0], &used[1]);

        let a_b = rebuilder.expr_provider.conj(a, b);
        let a_b_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], Rc::clone(&a_b));

        let b_base = rebuilder.base_provider.provide(
            "Er&", smallvec![(a_b_base, None)], Rc::clone(b));

        let a_b__b = rebuilder.expr_provider.imp(&a_b, b);
        let a_b__b_base = rebuilder.base_provider.provide(
            "I->", smallvec![(b_base, Some(a_b))], a_b__b);

        a_b__b_base
    }
}

impl AxiomRebuild for AxiomRebuilder<5>
{
    fn rebuild_axiom(rebuilder: &ProofRebuilder, used: &[ExprNode]) -> BaseNode {
        let (a, b) = (&used[0], &used[1]);

        let a_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], Rc::clone(a));
        let b_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], Rc::clone(b));

        let a_b = rebuilder.expr_provider.conj(a, b);
        let a_b_base = rebuilder.base_provider.provide(
            "I&", smallvec![(a_base, None), (b_base, None)], a_b);

        let b__a_b = rebuilder.expr_provider.imp(b, a_b_base.curr());
        let b__a_b_base = rebuilder.base_provider.provide(
            "I->", smallvec![(a_b_base, Some(Rc::clone(b)))], b__a_b);

        let a__b__a_b = rebuilder.expr_provider.imp(a, b__a_b_base.curr());
        let a__b__a_b_base = rebuilder.base_provider.provide(
            "I->", smallvec![(b__a_b_base, Some(Rc::clone(a)))], a__b__a_b);

        a__b__a_b_base
    }
}

impl AxiomRebuild for AxiomRebuilder<6>
{
    fn rebuild_axiom(rebuilder: &ProofRebuilder, used: &[ExprNode]) -> BaseNode {
        let (a, b) = (&used[0], &used[1]);

        let a_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], Rc::clone(a));

        let a_b = rebuilder.expr_provider.disj(a, b);
        let a_b_base = rebuilder.base_provider.provide(
            "Il|", smallvec![(a_base, None)], a_b);

        let a__a_b = rebuilder.expr_provider.imp(a, a_b_base.curr());
        let a__a_b_base = rebuilder.base_provider.provide(
            "I->", smallvec![(a_b_base, Some(Rc::clone(a)))], a__a_b);

        a__a_b_base
    }
}

impl AxiomRebuild for AxiomRebuilder<7>
{
    fn rebuild_axiom(rebuilder: &ProofRebuilder, used: &[ExprNode]) -> BaseNode {
        let (a, b) = (&used[0], &used[1]);

        let b_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], Rc::clone(a));

        let a_b = rebuilder.expr_provider.disj(a, b);
        let a_b_base = rebuilder.base_provider.provide(
            "Ir|", smallvec![(b_base, None)], a_b);

        let b__a_b = rebuilder.expr_provider.imp(b, a_b_base.curr());
        let b__a_b_base = rebuilder.base_provider.provide(
            "I->", smallvec![(a_b_base, Some(Rc::clone(b)))], b__a_b);

        b__a_b_base
    }
}

impl AxiomRebuild for AxiomRebuilder<8>
{
    fn rebuild_axiom(rebuilder: &ProofRebuilder, used: &[ExprNode]) -> BaseNode {
        let (a, b, c) = (&used[0], &used[1], &used[2]);

        let a_b = rebuilder.expr_provider.imp(a, b);
        let a_b_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], a_b);

        let a_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], Rc::clone(a));

        let c_from_a_base = rebuilder.base_provider.provide(
            "E->", smallvec![(a_b_base, None), (a_base, None)], Rc::clone(c));

        let b_c = rebuilder.expr_provider.imp(b, c);
        let b_c_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], b_c);

        let b_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], Rc::clone(b));

        let c_from_b_base = rebuilder.base_provider.provide(
            "E->", smallvec![(b_c_base, None), (b_base, None)], Rc::clone(c));

        let a_or_b = rebuilder.expr_provider.disj(a, b);
        let a_or_b_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], Rc::clone(&a_or_b));

        let c_from_any_base = rebuilder.base_provider.provide(
            "E|",
            smallvec![(c_from_a_base, Some(Rc::clone(a))), (c_from_b_base, Some(Rc::clone(b))), (a_or_b_base, None)],
            Rc::clone(c)
        );

        let if_else = rebuilder.expr_provider.imp(&a_or_b, c);
        let if_else_base = rebuilder.base_provider.provide(
            "I->", smallvec![(c_from_any_base, Some(Rc::clone(&a_or_b)))], if_else);

        let a_c = rebuilder.expr_provider.imp(a, c);
        let a_c__if_else = rebuilder.expr_provider.imp(&a_c, if_else_base.curr());
        let a_c__if_else_base = rebuilder.base_provider.provide(
            "I->", smallvec![(if_else_base, Some(a_c))], a_c__if_else);

        let a_b = rebuilder.expr_provider.imp(a, b);
        let res = rebuilder.expr_provider.imp(&a_b, a_c__if_else_base.curr());
        let res_base = rebuilder.base_provider.provide(
            "I->", smallvec![(a_c__if_else_base, Some(a_b))], res);

        res_base
    }
}

impl AxiomRebuild for AxiomRebuilder<9>
{
    fn rebuild_axiom(_rebuilder: &ProofRebuilder, used: &[ExprNode]) -> BaseNode {
        panic!("Expected to be recognized as Ax.sch. 2. Used: {:?}", used);
    }
}

impl AxiomRebuild for AxiomRebuilder<10>
{
    fn rebuild_axiom(rebuilder: &ProofRebuilder, used: &[ExprNode]) -> BaseNode {
        let (a, b) = (&used[0], &used[1]);

        let neg_a = rebuilder.expr_provider.neg(a);
        let neg_a_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], Rc::clone(&neg_a));

        let a_base = rebuilder.base_provider.provide(
            "Ax", smallvec![], Rc::clone(a));

        let bott = rebuilder.expr_provider.bott();
        let bott_base = rebuilder.base_provider.provide(
            "E->", smallvec![(neg_a_base, None), (a_base, None)], bott);

        let b_base = rebuilder.base_provider.provide(
            "E_|_", smallvec![(bott_base, None)], Rc::clone(b));

        let neg_a__b = rebuilder.expr_provider.imp(&neg_a, b);
        let neg_a__b_base = rebuilder.base_provider.provide(
            "I->", smallvec![(b_base, Some(neg_a))], neg_a__b);

        let a__neg_a__b = rebuilder.expr_provider.imp(a, neg_a__b_base.curr());
        let a__neg_a__b_base = rebuilder.base_provider.provide(
            "I->", smallvec![(neg_a__b_base, Some(Rc::clone(a)))], a__neg_a__b);

        a__neg_a__b_base
    }
}
