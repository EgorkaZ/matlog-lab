use std::{fmt::{Display, Write}};

use smallvec::SmallVec;

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
    Scheme(u8, SmallVec<[ExprNode; 3]>),
    FromHyp(ExprNode),
    MP{ from: ExprNode, imp: ExprNode },
}

impl Display for Based
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scheme(num, ..) => f.write_str("Ax. sch. ").and_then(|()| num.fmt(f)),
            Self::FromHyp(..) => f.write_str("Hyp."),
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
