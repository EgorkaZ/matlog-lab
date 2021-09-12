use std::{cell::RefCell, fmt::Display, hash::Hash, ptr, rc::{Rc}};

use rustc_hash::{FxHashSet};
use smallvec::SmallVec;

use crate::{ast::{ExprNode}, tree::NodeProvider};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum NodeKind
{
    Axiom,
    EraseImplication,
    IntroduceImplication,
    IntroduceConjunction,
    EraseConjunctionLeft,
    EraseConjunctionRight,
    InsertDisjunctionLeft,
    InsertDisjunctionRight,
    EraseDisjunction,
    EliminateLies,
}

impl From<&NodeKind> for &'static str
{
    fn from(kind: &NodeKind) -> Self {
        match kind {
            NodeKind::Axiom => "Ax",
            NodeKind::EraseImplication => "E->",
            NodeKind::IntroduceImplication => "I->",
            NodeKind::IntroduceConjunction => "I&",
            NodeKind::EraseConjunctionLeft => "El&",
            NodeKind::EraseConjunctionRight => "Er&",
            NodeKind::InsertDisjunctionLeft => "Il|",
            NodeKind::InsertDisjunctionRight => "Ir|",
            NodeKind::EraseDisjunction => "E|",
            NodeKind::EliminateLies => "E_|_",
        }
    }
}

impl From<&str> for NodeKind
{
    fn from(kind: &str) -> Self {
        match kind {
            "Ax" => NodeKind::Axiom,
            "E->" => NodeKind::EraseImplication,
            "I->" => NodeKind::IntroduceImplication,
            "I&" => NodeKind::IntroduceConjunction,
            "El&" => NodeKind::EraseConjunctionLeft,
            "Er&" => NodeKind::EraseConjunctionRight,
            "Il|" => NodeKind::InsertDisjunctionLeft,
            "Ir|" => NodeKind::InsertDisjunctionRight,
            "E|" => NodeKind::EraseDisjunction,
            "E_|_" => NodeKind::EliminateLies,
            _ => panic!("Cannot get NodeKind from \"{}\"", kind),
        }
    }
}

impl Display for NodeKind
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.into())
    }
}

#[derive(Debug, Eq, Clone)]
pub struct Based
{
    kind: NodeKind,
    children: SmallVec<[(BaseNode, Option<ExprNode>); 3]>,
    current: ExprNode,
}

pub type BaseNode = Rc<Based>;

impl Based
{
    pub fn curr(&self) -> &ExprNode
    { &self.current }

    pub fn children(&self) -> &[(BaseNode, Option<ExprNode>)]
    { &self.children }

    pub fn shift(&self) -> &'static str
    { From::from(&self.kind) }
}

impl PartialEq for Based
{
    fn eq(&self, other: &Self) -> bool {
        if !(self.kind == other.kind && self.current == other.current) {
            return false
        }

        if self.children.len() != other.children.len() {
            return false
        }

        self.children.iter()
            .zip(other.children.iter())
            .all(|((l_base, l_hyp), (r_base, r_hyp))| {
                Rc::ptr_eq(l_base, r_base) && l_hyp == r_hyp
            })
    }
}

impl Hash for Based
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        self.current.hash(state);
        self.children.iter()
            .for_each(|(child, hyp)| {
                hyp.hash(state);
                ptr::hash(Rc::as_ptr(child), state)
            })
    }
}

#[derive(Default)]
pub struct BaseProvider
{
    cached: RefCell<FxHashSet<BaseNode>>,
}

impl BaseProvider
{
    pub fn provide(
        &self,
        kind: &'static str,
        children: SmallVec<[(BaseNode, Option<ExprNode>); 3]>,
        current: ExprNode) -> BaseNode
    {
        let kind = kind.into();
        self.get_or_insert(Based{ kind, children, current })
    }
}

impl NodeProvider for BaseProvider
{
    type Node = Based;

    fn node_set(&self) -> std::cell::RefMut<'_, FxHashSet<Rc<Self::Node>>> {
        self.cached.borrow_mut()
    }
}
