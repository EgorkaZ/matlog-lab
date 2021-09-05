#[allow(clippy::all)] lalrpop_mod!(pub terms, "/parser/expressions.rs");

pub mod manager;
pub use manager::ExprManager;

mod tests;
