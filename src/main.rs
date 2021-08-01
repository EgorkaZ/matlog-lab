
#[macro_use] extern crate lalrpop_util;

pub mod ast;
pub mod parser;

use ast::TermProvider;
use parser::ExprParser;

use crate::ast::ExprProvider;

fn main() {
    let term_provider = TermProvider::new();
    let expr_provider = ExprProvider::new();
    let parser = ExprParser::new();
    let parsed = parser.parse(&term_provider, &expr_provider, "0=0&(@x.x=x -> x'=x')->x=x$").unwrap();
    println!("I parsed: {}", parsed);
    println!("Here's debug version: {:?}", parsed);
}
