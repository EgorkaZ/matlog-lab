pub mod term;

pub use term::{Term, TermProvider, TermNode, TermVar};
pub use term::UnOper as TermUnOp;
pub use term::BinOper as TermBiOp;

pub mod node_provider;

pub mod expression;
pub use expression::{Expr, ExprNode, ExprProvider, ExprPred};
pub use expression::UnOper as ExprUnOp;
pub use expression::BinOper as ExprBinOp;

mod helpers {
use std::{fmt::{Display, Write}, marker::PhantomData};

pub trait VarType
{
    fn type_name() -> &'static str;
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Variable<Type: VarType> {
    Static(char, PhantomData<Type>),
    Dynamic(String),
}

impl<Type: VarType> Display for Variable<Type>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Variable::*;

        match self {
            Static(name, ..) => f.write_char(*name),
            Dynamic(name) => f.write_fmt(format_args!("{}:{}", name, Type::type_name())),
        }
    }
}
} // mod helpers
