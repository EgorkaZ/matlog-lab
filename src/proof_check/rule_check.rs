use std::{rc::Rc};

use rustc_hash::FxHashMap;

use crate::{ast::{Expr, ExprBinOp, ExprNode, ExprUnOp, TermVar}, matcher::Matcher, proof_check::QuanRule};

use super::{BaseExpr, Based, Cringe};

pub fn check_rules(checked: &ExprNode, previous: &[BaseExpr]) -> Result<Based, Cringe>
{
    check_modus_ponens(checked, previous)
        .or_else(|mp_err| check_quan_rules(checked, previous).map_err(|quan_err| quan_err.min(mp_err)))
}

fn check_modus_ponens(checked: &ExprNode, previous: &[BaseExpr]) -> Result<Based, Cringe>
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
        .map_or(Err(Cringe::casual_cringe()), |(from_idx, impl_idx)| {
            Ok(Based::MP { from: from_idx + 1, imp: impl_idx + 1 })
        })
}

fn check_quan_rules(checked: &ExprNode, previous: &[BaseExpr]) -> Result<Based, Cringe>
{
    use Expr::BiOp;
    use ExprBinOp::Impl;

    if let BiOp(Impl, from, to) = &**checked {
        let implications = previous.iter()
            .enumerate()
            .filter_map(|(idx, BaseExpr{ expr, .. })| {
                if let BiOp(Impl, found_from, found_to) = &**expr {
                    Some((idx, found_from, found_to))
                } else {
                    None
                }
            });
        check_exist_rule((from, to), implications.clone())
            .or_else(|exist_err| check_any_rule((from, to), implications)
                .map_err(|any_err| any_err.min(exist_err)))
    } else {
        Err(Cringe::casual_cringe())
    }
}

fn check_exist_rule<'a>(
    (from, to): (&ExprNode, &ExprNode),
    impls_it: impl Iterator<Item = (usize, &'a ExprNode, &'a ExprNode)>) -> Result<Based, Cringe>
{
    let candidates_it = impls_it
        .filter_map(|(idx, found_from, found_to)| {
            if Rc::ptr_eq(found_to, to) {
                use Expr::UnOp;
                use ExprUnOp::Ext;

                if let UnOp(Ext(TermVar::Static(quan_var, ..)), sub_from) = &**from {
                    if Rc::ptr_eq(sub_from, found_from) {
                        return Some((idx, *quan_var))
                    }
                }
            }

            None
        });
    check_rule_detail(to, QuanRule::Exist, candidates_it)
}

fn check_any_rule<'a>(
    (from, to): (&ExprNode, &ExprNode),
    impls_it: impl Iterator<Item = (usize, &'a ExprNode, &'a ExprNode)>) -> Result<Based, Cringe>
{
    let candidates_it = impls_it
        .filter_map(|(idx, found_from, found_to)| {
            if Rc::ptr_eq(from, found_from) {
                use Expr::UnOp;
                use ExprUnOp::Any;

                if let UnOp(Any(TermVar::Static(quan_var, ..)), sub_to) = &**to {
                    if Rc::ptr_eq(sub_to, found_to) {
                        return Some((idx, *quan_var))
                    }
                }
            }

            None
        });
    check_rule_detail(from, QuanRule::Any, candidates_it)
}

fn check_rule_detail(
    non_quan: &ExprNode,
    rule: QuanRule,
    candidates_it: impl Iterator<Item = (usize, char)>) -> Result<Based, Cringe>
{
    let mut cached_free_vars = None;
    candidates_it
        .fold(Err(Cringe::casual_cringe()), |res, (idx, var)| {
            let free_vars = cached_free_vars.get_or_insert_with(|| Matcher::all_free_vars(non_quan));
            res.or_else(|prev_err| {
                if !free_vars.contains(&var) {
                    Ok(Based::Rule{ orig: idx + 1, rule })
                } else {
                    Err(prev_err.min(Cringe::FreeVarInRule{ var, rule: QuanRule::Exist }))
                }
            })
        })
}
