use std::{cell::RefCell, fmt::Display, hash::Hash, ptr, rc::{self, Rc}};

use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;

use crate::{ast::{ExprNode, node_provider::OperNodeProvider}, tree::NodeProvider};

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
    children: SmallVec<[Rc<Based>; 3]>,
    current: ExprNode,
    new_hyp: Option<ExprNode>,
}

pub type BaseNode = Rc<Based>;

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
            .all(|(l, r)| {
                Rc::ptr_eq(l, r)
            })
    }
}

impl Hash for Based
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        self.current.hash(state);
        self.children.iter()
            .for_each(|child| ptr::hash(Rc::as_ptr(child), state))
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
        children: SmallVec<[BaseNode; 3]>,
        current: ExprNode,
        new_hyp: Option<ExprNode>) -> BaseNode
    {
        let kind = kind.into();
        self.get_or_insert(Based{ kind, children, current, new_hyp })
    }
}

impl NodeProvider for BaseProvider
{
    type Node = BaseNode;

    fn node_set(&self) -> std::cell::RefMut<'_, FxHashSet<Rc<Self::Node>>> {
        self.cached.borrow_mut()
    }
}
