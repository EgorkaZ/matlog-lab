use std::{rc::Rc};

use crate::tree::NodeProvider;

pub trait OperNode
{
    type UnOp;
    type BiOp;

    fn un_op(op: Self::UnOp, sub: Rc<Self>) -> Self;
    fn bin_op(op: Self::BiOp, l: Rc<Self>, r: Rc<Self>) -> Self;
}

pub trait OperNodeProvider : NodeProvider
    where <Self as NodeProvider>::Node: OperNode
{
    fn un_op(&self, op: <Self::Node as OperNode>::UnOp, sub: &Rc<Self::Node>) -> Rc<Self::Node>
    {
        let mb_new = Self::Node::un_op(op, Rc::clone(sub));
        self.get_or_insert(mb_new)
    }
    fn bin_op(&self, op: <Self::Node as OperNode>::BiOp, l: &Rc<Self::Node>, r: &Rc<Self::Node>) -> Rc<Self::Node>
    {
        let mb_new = Self::Node::bin_op(op, Rc::clone(l), Rc::clone(r));
        self.get_or_insert(mb_new)
    }
}