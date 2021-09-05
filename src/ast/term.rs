use std::{cell::RefCell, fmt::{Display, Write}, hash::Hash, marker::PhantomData, ptr, rc::Rc};

use rustc_hash::FxHashSet;

use super::{node_provider::{OperNode, OperNodeProvider}, helpers::{Variable, VarType}};
use crate::tree::{ChildrenIter, Tree};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TermVarType;

impl VarType for TermVarType
{
    fn type_name() -> &'static str {
        "Var"
    }
}

pub type TermVar = Variable<TermVarType>;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOper
{
    Next
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOper
{
    Add,
    Mul,
}

#[derive(Debug, Clone, Eq)]
pub enum Term
{
    Var(TermVar),
    Zero,
    BiOp(BinOper, TermNode, TermNode),
    UnOp(UnOper, TermNode),
}

pub type TermNode = Rc<Term>;


impl Hash for Term
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use Term::*;

        match self {
            Var(var) => {
                state.write_u8(0);
                var.hash(state)
            },
            Zero => state.write_u8(1),
            UnOp(op, sub) => {
                state.write_u8(2);
                op.hash(state);
                ptr::hash(Rc::as_ptr(sub), state)
            },
            BiOp(op, l, r) => {
                state.write_u8(3);
                op.hash(state);
                ptr::hash(Rc::as_ptr(l), state);
                ptr::hash(Rc::as_ptr(r), state)
            },
        }
    }
}

impl PartialEq for Term
{
    fn eq(&self, other: &Self) -> bool {
        use Term::*;

        match (self, other) {
            (Var(l), Var(r)) => l == r,
            (Zero, Zero) => true,
            (UnOp(l_op, l_sub), UnOp(r_op, r_sub)) => {
                l_op == r_op &&
                    Rc::ptr_eq(l_sub, r_sub)
            },
            (BiOp(l_op, l_l, l_r), BiOp(r_op, r_l, r_r)) => {
                l_op == r_op &&
                    Rc::ptr_eq(l_l, r_l) &&
                    Rc::ptr_eq(l_r, r_r)
            },
            _ => false
        }
    }
}

impl Tree for Term
{
    type Child = TermNode;

    fn children(&self) -> crate::tree::ChildrenIter<'_, Self> {
        use Term::*;

        match self {
            UnOp(_, sub) => ChildrenIter::new(vec![sub]),
            BiOp(_, l, r) => ChildrenIter::new(vec![l, r]),
            _ => ChildrenIter::new(Vec::new())
        }
    }
}

impl OperNode for Term
{
    type UnOp = UnOper;

    type BiOp = BinOper;

    fn un_op(op: UnOper, sub: std::rc::Rc<Self>) -> Self {
        Self::UnOp(op, sub)
    }

    fn bin_op(op: BinOper, l: std::rc::Rc<Self>, r: std::rc::Rc<Self>) -> Self {
        Self::BiOp(op, l, r)
    }
}


#[derive(Debug, Clone)]
pub struct TermProvider
{
    saved: RefCell<FxHashSet<Rc<Term>>>
}

impl OperNodeProvider for TermProvider
{
    type Node = Term;

    fn node_set(&self) -> std::cell::RefMut<'_, FxHashSet<Rc<Self::Node>>> {
        self.saved.borrow_mut()
    }
}

impl TermProvider
{
    pub fn new() -> Self
    {
        TermProvider{ saved: Default::default() }
    }

    pub fn add(&self, l: &TermNode, r: &TermNode) -> TermNode
    {
        self.bin_op(BinOper::Add, l, r)
    }

    pub fn mul(&self, l: &TermNode, r: &TermNode) -> TermNode
    {
        self.bin_op(BinOper::Mul, l, r)
    }

    pub fn next(&self, sub: &TermNode) -> TermNode
    {
        self.un_op(UnOper::Next, sub)
    }

    pub fn zero(&self) -> TermNode
    {
        self.get_or_insert(Term::Zero)
    }

    pub fn var(&self, name: char) -> TermNode
    {
        self.get_or_insert(Term::Var(TermVar::Static(name, PhantomData)))
    }

    pub fn repl(&self, name: &str) -> TermNode
    {
        self.get_or_insert(Term::Var(TermVar::Dynamic(String::from(name))))
    }
}

impl Display for Term
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Term::*;

        match self {
            Var(var) => var.fmt(f),
            Zero => f.write_char('0'),
            UnOp(UnOper::Next, sub) => f.write_fmt(format_args!("{}'", sub)),
            BiOp(op, l, r) => f.write_fmt(format_args!("({}{}{})", l, op, r)),
        }
    }
}

impl Display for BinOper
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BinOper::*;

        match self {
            Add => f.write_char('+'),
            Mul => f.write_char('*')
        }
    }
}
