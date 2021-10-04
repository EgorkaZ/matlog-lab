use std::{cmp::Ordering, fmt::{Display, Write}};

use crate::ast::{TermNode, ExprNode};

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

#[derive(Debug, Clone, Copy)]
pub enum Based
{
    Scheme(u8),
    Axiom(u8),
    MP{ from: usize, imp: usize },
    Rule{ orig: usize, rule: QuanRule},
}

impl Display for Based
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scheme(num) => f.write_str("Ax. sch. ").and_then(|()| num.fmt(f)),
            Self::Axiom(num @ 1..=8) => f.write_str("Ax. A").and_then(|()| num.fmt(f)),
            Self::Axiom(9) => f.write_str("Ax. sch. A9"),
            Self::MP{ from, imp } => f.write_fmt(format_args!("M.P. {}, {}", from, imp)),
            Self::Rule{ orig, rule } => f.write_fmt(format_args!("{}-intro {}", rule, orig)),
            _ => panic!("Unexpected base: {:?}", self),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Wrong
{
    FreeVarInRule{ var: char, rule: QuanRule },
    NonFreeToSubst{ var: char, substed: TermNode, rule: QuanRule },
    Unproved,
}

// TODO I beg you, rename it before you send
impl Wrong /*i.e. not based*/
{
    pub fn get_priority(&self) -> u8
    {
        use Wrong::*;

        match self {
            FreeVarInRule{ rule: QuanRule::Exist, .. } => 0,
            FreeVarInRule{ rule: QuanRule::Any, .. } => 1,
            NonFreeToSubst{ rule: QuanRule::Exist, .. } => 2,
            NonFreeToSubst{ rule: QuanRule::Any, .. } => 3,
            Unproved => 4,
        }
    }

    fn get_var(&self) -> Option<char>
    {
        match self {
            Self::FreeVarInRule{ var, .. } => Some(*var),
            Self::NonFreeToSubst{ var, .. } => Some(*var),
            Self::Unproved => None,
        }
    }
}

impl Ord for Wrong
{
    fn cmp(&self, other: &Self) -> Ordering {
        match self.get_priority().cmp(&other.get_priority()) {
            Ordering::Equal => {
                if let Some(l_var) = self.get_var() {
                    l_var.cmp(&other.get_var().unwrap())
                } else {
                    Ordering::Equal
                }
            },
            lt_or_gt => lt_or_gt,
        }
    }
}

impl PartialOrd for Wrong
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Display for Wrong
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unproved => f.write_str("is not proved"),
            Self::NonFreeToSubst{ var, substed, rule } =>
                f.write_fmt(format_args!("variable {} is not free for term {} in {}-axiom", var, substed, rule)),
            Self::FreeVarInRule{ var, rule } =>
                f.write_fmt(format_args!("variable {} occurs free in {}-rule", var, rule)),
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

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::ast::Term;

    use super::*;

    #[test]
    fn errors_order() {
        use Wrong::*;
        use QuanRule::*;

        let some_term = Rc::new(Term::Zero);

        let unproved = Unproved;
        let non_free_any_a = NonFreeToSubst{ var: 'a', rule: Any, substed: some_term.clone() };
        let non_free_any_b = NonFreeToSubst{ var: 'b', rule: Any, substed: some_term.clone() };

        let non_free_ext_c = NonFreeToSubst{ var: 'c', rule: Exist, substed: some_term.clone() };
        let non_free_ext_d = NonFreeToSubst{ var: 'd', rule: Exist, substed: some_term };

        let free_ext_e = FreeVarInRule{ var: 'e', rule: Exist };
        let free_ext_g = FreeVarInRule{ var: 'g', rule: Exist };

        let free_any_f = FreeVarInRule{ var: 'f', rule: Any };
        let free_any_h = FreeVarInRule{ var: 'h', rule: Any };

        let mut shuffled = [non_free_any_b.clone(), free_any_f.clone(), non_free_ext_d.clone(),
            non_free_any_a.clone(), unproved.clone(), free_ext_g.clone(),
            free_ext_e.clone(), free_any_h.clone(), non_free_ext_c.clone()];

        let expected = [free_ext_e, free_ext_g, free_any_f, free_any_h, non_free_ext_c, non_free_ext_d,
            non_free_any_a, non_free_any_b, unproved];

        shuffled.sort();

        assert_eq!(shuffled, expected);

    }
}
