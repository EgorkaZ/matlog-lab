use std::{cell::RefCell, collections::HashSet, fmt::{Display, Write}, hash::Hash, rc::Rc, ptr};

use super::node_provider::{OperNode, OperNodeProvider};

#[derive(Debug, Clone, Eq)]
pub enum Term
{
    Variable(char),
    Zero,
    BiOp(BinOper, TermNode, TermNode),
    UnOp(UnOper, TermNode),
}

pub type TermNode = Rc<Term>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOper
{
    Add,
    Mul,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOper
{
    Next
}

impl Display for Term
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Term::*;

        match self {
            Variable(name) => f.write_char(*name),
            Zero => f.write_char('0'),
            UnOp(UnOper::Next, sub) => f.write_char('(')
                .and_then(|()| sub.fmt(f))
                .and_then(|()| f.write_str("')")),
            BiOp(op, l, r) => f.write_fmt(format_args!("({} {} {})", l, op, r)),
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

impl Hash for Term
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use Term::*;

        match self {
            Variable(name) => {
                state.write_u8(0);
                name.hash(state)
            },
            Zero => state.write_u8(1),
            UnOp(op, sub) => {
                state.write_u8(3);
                op.hash(state);
                ptr::hash(Rc::as_ptr(sub), state)
            },
            BiOp(op, l, r) => {
                state.write_u8(3);
                op.hash(state);
                ptr::hash(Rc::as_ptr(l), state);
                ptr::hash(Rc::as_ptr(r), state)
            }
        }
    }
}

impl PartialEq for Term
{
    fn eq(&self, other: &Self) -> bool {
        use Term::*;

        match (self, other) {
            (Variable(l), Variable(r)) => l == r,
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

pub struct TermProvider
{
    saved: RefCell<HashSet<Rc<Term>>>
}

impl OperNodeProvider for TermProvider
{
    type Node = Term;

    fn node_set(&self) -> std::cell::RefMut<'_, HashSet<Rc<Self::Node>>> {
        self.saved.borrow_mut()
    }
}

impl TermProvider
{
    pub fn new() -> Self
    {
        TermProvider{ saved: RefCell::new(HashSet::new()) }
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
        self.get_or_insert(Term::Variable(name))
    }
}
