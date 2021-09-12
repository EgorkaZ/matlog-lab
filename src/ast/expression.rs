use std::{cell::RefCell, fmt::{Display, Write}, hash::Hash, ptr, rc::Rc};

use rustc_hash::FxHashSet;

use crate::tree::{ChildrenIter, NodeProvider, Tree};

use super::{node_provider::{OperNode, OperNodeProvider}};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExprVar
{
    Static(String),
    Meta(String),
}

impl Display for ExprVar
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Static(name) => f.write_str(name),
            Self::Meta(name) => f.write_str(name).and_then(|()| f.write_str(":Meta"))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnOper
{
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOper
{
    Conj,
    Disj,
    Impl,
}


#[derive(Debug, Clone, Eq)]
pub enum Expr
{
    Variable(ExprVar),
    UnOp(UnOper, ExprNode),
    BiOp(BinOper, ExprNode, ExprNode),
    Bottom
}

pub type ExprNode = Rc<Expr>;

impl Hash for Expr
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use Expr::*;

        match self {
            Variable(name) => {
                state.write_u8(0);
                name.hash(state)
            },
            UnOp(op, sub) => {
                state.write_u8(1);
                op.hash(state);
                ptr::hash(Rc::as_ptr(sub), state)
            },
            BiOp(op, l, r) => {
                state.write_u8(2);
                op.hash(state);
                ptr::hash(Rc::as_ptr(l), state);
                ptr::hash(Rc::as_ptr(r), state)
            },
            Bottom => {
                state.write_u8(3);
            },
        }
    }
}

impl PartialEq for Expr
{
    fn eq(&self, other: &Self) -> bool {
        use Expr::*;

        match (self, other) {
            (Variable(l), Variable(r)) => l == r,
            (UnOp(l_op, l_sub), UnOp(r_op, r_sub)) => {
                l_op == r_op &&
                    Rc::ptr_eq(l_sub, r_sub)
            },
            (
                BiOp(l_op, l_l, l_r),
                BiOp(r_op, r_l, r_r)) => {
                l_op == r_op &&
                    Rc::ptr_eq(l_l, r_l) &&
                    Rc::ptr_eq(l_r, r_r)
            },
            (Bottom, Bottom) => true,
            _ => false
        }
    }
}

impl Tree for Expr
{
    type Child = ExprNode;

    fn children(&self) -> crate::tree::ChildrenIter<'_, Self> {
        use Expr::*;

        match self {
            UnOp(_, sub) => ChildrenIter::new(vec![sub]),
            BiOp(_, l, r) => ChildrenIter::new(vec![l, r]),
            _ => ChildrenIter::new(Vec::new()),
        }
    }
}

impl OperNode for Expr
{
    type UnOp = UnOper;

    type BiOp = BinOper;

    fn un_op(op: UnOper, sub: Rc<Self>) -> Self {
        Expr::UnOp(op, sub)
    }

    fn bin_op(op: BinOper, l: Rc<Self>, r: Rc<Self>) -> Self {
        Expr::BiOp(op, l, r)
    }
}

pub struct ExprProvider
{
    saved: RefCell<FxHashSet<ExprNode>>,
}

impl NodeProvider for ExprProvider
{
    type Node = Expr;

    fn node_set(&self) -> std::cell::RefMut<'_, FxHashSet<Rc<Self::Node>>> {
        self.saved.borrow_mut()
    }
}

impl OperNodeProvider for ExprProvider {}

impl ExprProvider
{
    pub fn new() -> Self
    {
        ExprProvider{ saved: Default::default() }
    }

    pub fn conj(&self, l: &ExprNode, r: &ExprNode) -> ExprNode
    {
        self.bin_op(BinOper::Conj, l, r)
    }

    pub fn disj(&self, l: &ExprNode, r: &ExprNode) -> ExprNode
    {
        self.bin_op(BinOper::Disj, l, r)
    }

    pub fn imp(&self, l: &ExprNode, r: &ExprNode) -> ExprNode
    {
        self.bin_op(BinOper::Impl, l, r)
    }

    pub fn neg(&self, sub: &ExprNode) -> ExprNode
    {
        self.imp(sub, &self.bott())
    }

    pub fn var(&self, name: &str) -> ExprNode
    {
        self.get_or_insert(Expr::Variable(ExprVar::Static(name.into())))
    }

    pub fn meta(&self, name: &str) -> ExprNode
    {
        self.get_or_insert(Expr::Variable(ExprVar::Meta(name.into())))
    }

    pub fn bott(&self) -> ExprNode
    {
        self.get_or_insert(Expr::Bottom)
    }
}

impl Default for ExprProvider {
    fn default() -> Self {
        Self::new()
    }
}


impl Display for UnOper
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use UnOper::*;

        match self {
            Neg => f.write_char('!'),
        }
    }
}

impl Display for BinOper
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BinOper::*;

        match self {
            Conj => f.write_char('&'),
            Disj => f.write_char('|'),
            Impl => f.write_str("->"),
        }
    }
}

impl Display for Expr
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expr::*;

        match self {
            Variable(name) => name.fmt(f),
            UnOp(op, sub) => f.write_fmt(format_args!("({}{})", op, sub)),
            BiOp(op, l, r) => f.write_fmt(format_args!("({} {} {})", l, op, r)),
            Bottom => f.write_str("_|_"),
        }
    }
}
