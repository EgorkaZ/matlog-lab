#[allow(clippy::all)] lalrpop_mod!(pub terms, "/parser/terms.rs");

pub mod manager;
pub use manager::ExprManager;

mod tests;
// mod glue_parser;

mod preprocessor;
pub use preprocessor::preprocess;
