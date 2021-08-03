#[allow(clippy::all)] lalrpop_mod!(pub terms, "/parser/terms.rs");

pub mod manager;
pub use manager::ExprManager;
