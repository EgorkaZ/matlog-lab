use crate::ast::{TermNode, ExprNode};

#[derive(Debug, Clone, Copy)]
pub enum Based
{
    Scheme(u8),
    Axiom(u8),
    MP{ from: usize, imp: usize },
}

#[derive(Debug, Clone)]
pub enum Cringe
{
    NonFreeToSubst{ var: char, substed: TermNode, rule: char },
    Unproved,
}

// TODO I beg you, rename it before you send
impl Cringe /*i.e. not based*/
{
    pub fn casual_cringe() -> Self
    {
        Self::Unproved
    }
}

#[derive(Debug, Clone)]
pub struct BaseExpr
{
    pub expr: ExprNode,
    pub proof: Result<Based, Cringe>,
}

#[allow(unused)]
pub struct Proof
{
    to_prove: ExprNode,
    pub proof: Vec<BaseExpr>,
}

impl Proof
{
    pub fn new(to_prove: ExprNode, proof: Vec<BaseExpr>) -> Self
    {
        Proof{ to_prove, proof }
    }
}
