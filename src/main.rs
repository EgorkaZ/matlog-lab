
#[macro_use] extern crate lalrpop_util;

pub mod ast;
pub mod parser;
pub mod matcher;
pub mod tree;


use std::rc::Rc;

use crate::{ast::{Expr, ExprNode}, matcher::{GetSubsts}, parser::ExprManager};

fn main() {
    let manager = ExprManager::new();
    let parsed = manager.parse("a=a -> b=b -> a=a $");

    println!("I parsed: {}", parsed);
    println!("Here's debug version: {:?}", parsed);

    let matcher_expr = manager.parse("a:Pred -> b:Pred -> a:Pred $");
    println!("Matcher: {}", matcher_expr);
    println!("Debug matcher: {:?}", matcher_expr);

    let matcher = manager.matcher_node(matcher_expr);
    let match_res = match matcher.match_expression(&parsed) {
        Ok(substs) => substs.all_substs(|_, subst: &ExprNode| matches!(&**subst, Expr::Eq(..))),
        Err(_) => false,
    };
    println!("Match result: {}", match_res);

    let scheme_matcher = manager.matcher_str("subst:Pred -> ?x:Var.orig:Pred $");
    let match_res = scheme_matcher
        .match_expression(&manager.parse("a + b = 0 -> ?c.c = 0 $")).unwrap();

    let var_name = match_res.get_substs().iter().next().unwrap().1;
    let pred_substs = match_res.get_substs();

    let orig_matcher = manager.matcher_node(Rc::clone(pred_substs.get("orig").unwrap()));
    let result = orig_matcher.match_expr_free_var_subst(pred_substs.get("subst").unwrap(), *var_name).unwrap();

    println!("Scheme match res: {:?}", result);
}
