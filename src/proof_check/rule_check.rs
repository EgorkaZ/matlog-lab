use std::{rc::Rc};

use rustc_hash::FxHashMap;

use crate::{ast::{Expr, ExprBinOp, ExprNode}};

use super::{BaseExpr, Based, Wrong};

pub fn check_rules(checked: &ExprNode, previous: &[BaseExpr]) -> Result<Based, Wrong>
{
    check_modus_ponens(checked, previous)
}

fn check_modus_ponens(checked: &ExprNode, previous: &[BaseExpr]) -> Result<Based, Wrong>
{
    let possible_from: FxHashMap<_, _> = previous.iter()
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
        .fold(Default::default(), |mut res, (expr, idx)| {
            match res.get_mut(&expr) {
                Some(found) => {
                    *found = idx.min(*found);
                },
                None => {
                    res.insert(expr, idx);
                }
            }
            res
        });

    previous.iter()
        .map(|BaseExpr{ expr, .. }| expr)
        .enumerate()
        .filter_map(|(from_idx, expr)| {
            possible_from.get(expr).map(|impl_idx| (from_idx, *impl_idx))
        })
        .min()
        .map_or(Err(Wrong::Unproved), |(from_idx, impl_idx)| {
            Ok(Based::MP { from: from_idx + 1, imp: impl_idx + 1 })
        })
}
