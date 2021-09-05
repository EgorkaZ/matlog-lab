use std::{fmt::{Display, Write}};

use crate::ast::{ExprNode};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum QuanRule
{
    Exist,
    Any,
}

impl Display for QuanRule
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Exist => f.write_char('?'),
            Self::Any => f.write_char('@'),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Based
{
    Scheme(u8),
    FromHyp,
    MP{ from: usize, imp: usize },
}

impl Display for Based
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scheme(num) => f.write_str("Ax. sch. ").and_then(|()| num.fmt(f)),
            Self::FromHyp => f.write_str("Hyp."),
            Self::MP{ from, imp } => f.write_fmt(format_args!("M.P. {}, {}", from, imp)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Wrong
{
    Unproved,
}

impl Display for Wrong
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unproved => f.write_str("is not proved"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BaseExpr
{
    pub expr: ExprNode,
    pub proof: Result<Based, Wrong>,
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
