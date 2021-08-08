use crate::ast::ExprNode;

pub struct Proof
{
    to_prove: ExprNode,
    proof: Vec<ExprNode>,
}

impl Proof
{
    pub fn new(to_prove: ExprNode, proof: Vec<ExprNode>) -> Self
    {
        Proof{ to_prove, proof }
    }
}
