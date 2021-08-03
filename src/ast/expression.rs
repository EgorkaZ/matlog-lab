use std::{cell::RefCell, collections::HashSet, fmt::{Display, Write}, hash::Hash, marker::PhantomData, ptr, rc::Rc};

use crate::tree::{ChildrenIter, Tree};

use super::{TermNode, TermVar, helpers::{VarType, Variable}, node_provider::{OperNode, OperNodeProvider}};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprVarType;

impl VarType for ExprVarType
{
    fn type_name() -> &'static str {
        "Pred"
    }
}

pub type ExprPred = Variable<ExprVarType>;

#[derive(Debug, Clone, Eq)]
pub enum Expr
{
    Pred(ExprPred),
    UnOp(UnOper, ExprNode),
    BiOp(BinOper, ExprNode, ExprNode),
    Eq(TermNode, TermNode),
}

pub type ExprNode = Rc<Expr>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOper
{
    Conj,
    Disj,
    Impl,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnOper
{
    Neg,
    Any(TermVar),
    Ext(TermVar),
}

impl Display for UnOper
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use UnOper::*;

        match self {
            Neg => f.write_char('!'),
            Any(var) => f.write_fmt(format_args!("@{}.", var)),
            Ext(var) => f.write_fmt(format_args!("?{}.", var)),
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
            Pred(name) => name.fmt(f),
            UnOp(op, sub) => f.write_fmt(format_args!("({}{})", op, sub)),
            BiOp(op, l, r) => f.write_fmt(format_args!("({} {} {})", l, op, r)),
            Eq(l, r) => f.write_fmt(format_args!("({} = {})", l, r)),
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

impl Hash for Expr
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use Expr::*;

        match self {
            Pred(name) => {
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
            Eq(l, r) => {
                state.write_u8(3);
                ptr::hash(Rc::as_ptr(l), state);
                ptr::hash(Rc::as_ptr(r), state)
            },
        }
    }
}

impl PartialEq for Expr
{
    fn eq(&self, other: &Self) -> bool {
        use Expr::*;

        match (self, other) {
            (Pred(l), Pred(r)) => l == r,
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
            (Eq(l_l, l_r), Eq(r_l, r_r)) => {
                Rc::ptr_eq(l_l, r_l) &&
                    Rc::ptr_eq(l_r, r_r)
            },
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

pub struct ExprProvider
{
    saved: RefCell<HashSet<ExprNode>>,
}

impl OperNodeProvider for ExprProvider
{
    type Node = Expr;

    fn node_set(&self) -> std::cell::RefMut<'_, HashSet<Rc<Self::Node>>> {
        self.saved.borrow_mut()
    }
}

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
        self.un_op(UnOper::Neg, sub)
    }

    pub fn any(&self, var: TermVar, sub: &ExprNode) -> ExprNode
    {
        self.un_op(UnOper::Any(var), sub)
    }

    pub fn ext(&self, var: TermVar, sub: &ExprNode) -> ExprNode
    {
        self.un_op(UnOper::Ext(var), sub)
    }

    pub fn eq(&self, l: &TermNode, r: &TermNode) -> ExprNode
    {
        self.get_or_insert(Expr::Eq(Rc::clone(l), Rc::clone(r)))
    }

    pub fn pred(&self, name: char) -> ExprNode
    {
        self.get_or_insert(Expr::Pred(ExprPred::Static(name, PhantomData)))
    }

    pub fn repl(&self, name: &str) -> ExprNode
    {
        self.get_or_insert(Expr::Pred(ExprPred::Dynamic(String::from(name))))
    }
}
