use std::{collections::HashMap, rc::Rc};

use crate::ast::{Expr, ExprBinOp, ExprNode};

use super::{BaseExpr, Based, Cringe};

pub fn check_rules(checked: &ExprNode, previous: &[BaseExpr]) -> Result<Based, Cringe>
{
    check_modus_ponens(checked, previous)
        .or_else(|_| check_quan_rules(checked, previous))
}

fn check_modus_ponens(checked: &ExprNode, previous: &[BaseExpr]) -> Result<Based, Cringe>
{
    let possible_from: HashMap<_, _> = previous.iter()
        .map(|BaseExpr{ expr, .. }| expr)
        .enumerate()
        .filter_map(|(impl_idx, expr)| {
            use Expr::BiOp;
            use ExprBinOp::Impl;

            match &**expr {
                BiOp(Impl, from, to) if to == checked => Some((Rc::clone(from), impl_idx)),
                _ => None,
            }
        })
        .collect();

    previous.iter()
        .map(|BaseExpr{ expr, .. }| expr)
        .enumerate()
        .find_map(|(from_idx, expr)| {
            possible_from.get(expr).map(|impl_idx| (from_idx, *impl_idx))
        })
        .map_or(Err(Cringe::casual_cringe()), |(from_idx, impl_idx)| {
            Ok(Based::MP { from: from_idx + 1, imp: impl_idx + 1 })
        })
}

fn check_quan_rules(checked: &ExprNode, previous: &[BaseExpr]) -> Result<Based, Cringe>
{
    // todo!();
    Err(Cringe::casual_cringe())
}
