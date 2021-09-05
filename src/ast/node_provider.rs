use std::{cell::RefMut, hash::Hash, rc::Rc};

use rustc_hash::FxHashSet;

pub trait OperNode
{
    type UnOp;
    type BiOp;

    fn un_op(op: Self::UnOp, sub: Rc<Self>) -> Self;
    fn bin_op(op: Self::BiOp, l: Rc<Self>, r: Rc<Self>) -> Self;
}

pub trait OperNodeProvider
{
    type Node: OperNode + Eq + Hash;

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

    fn node_set(&self) -> RefMut<'_, FxHashSet<Rc<Self::Node>>>;

    fn get_or_insert(&self, node: Self::Node) -> Rc<Self::Node>
    {
        let mut set = self.node_set();

        if let Some(found) = set.get(&node) {
            Rc::clone(found)
        } else {
            let returned = Rc::new(node);
            set.insert(Rc::clone(&returned));
            returned
        }
    }
}