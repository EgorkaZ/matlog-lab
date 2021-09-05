use crate::{ast::{ExprNode, ExprProvider}, matcher::Matcher};

use super::{terms::{ExprParser, ProvedParser}};

pub struct ExprManager
{
    parser: ExprParser,
    expr_provider: ExprProvider,
}

impl Default for ExprManager
{
    fn default() -> Self {
        ExprManager{ parser: ExprParser::new(), expr_provider: ExprProvider::new() }
    }
}

impl ExprManager
{
    pub fn new() -> Self
    {
        Self::default()
    }

    pub fn parse(&self, str: &str) -> ExprNode
    {
        self.parser.parse(&self.expr_provider, str)
            .unwrap_or_else(|parse_err| panic!("I tried to parse: '{}' and failed. Error: {}", str, parse_err))
    }

    pub fn parse_proved(&self, str: &str) -> (mset::MultiSet<ExprNode>, ExprNode)
    {
        let (hypothesis, expr) = ProvedParser::new().parse(&self.expr_provider, str)
            .unwrap_or_else(|parse_err| panic!("I tried to parse: '{}' and failed. Error: {}", str, parse_err));
        let hypothesis = hypothesis.into_iter().collect();
        (hypothesis, expr)
    }

    pub fn matcher_str(&self, matcher: &str) -> Matcher
    {
        self.matcher_node(self.parse(matcher))
    }

    pub fn matcher_node(&self, matcher: ExprNode) -> Matcher
    {
        Matcher::new(matcher)
    }
}
