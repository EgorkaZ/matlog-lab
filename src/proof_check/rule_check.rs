use std::{rc::Rc};

use rustc_hash::FxHashMap;

use crate::{ast::{Expr, ExprBinOp, ExprNode}};

use super::{Based, Wrong};

pub fn check_rules(checked: &ExprNode, previous: &FxHashMap<ExprNode, Based>) -> Result<Based, Wrong>
{
    check_modus_ponens(checked, previous)
}

fn check_modus_ponens(checked: &ExprNode, previous: &FxHashMap<ExprNode, Based>) -> Result<Based, Wrong>
{
    if let Some(proved_base) = previous.get(checked) {
        return Ok(proved_base.clone())
    }

    previous.iter()
        .map(|(expr,..)| expr)
        .find_map(|imp| {
            use Expr::BiOp;
            use ExprBinOp::Impl;

            match &**imp {
                BiOp(Impl, from, to) if to == checked => {
                    previous.get(from).map(|_| Based::MP{ from: Rc::clone(from), imp: Rc::clone(imp) })
                },
                _ => None,
            }
        })
        .ok_or(Wrong::Unproved)
}
