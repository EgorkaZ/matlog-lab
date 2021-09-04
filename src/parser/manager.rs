use crate::{ast::{ExprNode, ExprProvider, TermProvider}, matcher::Matcher};

use super::{preprocess, terms::{ExprParser, ProvedParser}};

pub struct ExprManager
{
    parser: ExprParser,
    term_provider: TermProvider,
    expr_provider: ExprProvider,
}

impl Default for ExprManager
{
    fn default() -> Self {
        ExprManager{ parser: ExprParser::new(), term_provider: TermProvider::new(), expr_provider: ExprProvider::new() }
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
        self.parser.parse(&self.term_provider, &self.expr_provider, &preprocess(str.chars()))
            .expect(&format!("I tried to parse: '{}' and failed. I'm so sorry", str))
    }

    pub fn parse_proved(&self, str: &str) -> ExprNode
    {
        ProvedParser::new().parse(&self.term_provider, &self.expr_provider, &preprocess(str.chars())).unwrap()
    }

    pub fn matcher_str(&self, matcher: &str) -> Matcher<'_>
    {
        self.matcher_node(self.parse(matcher))
    }

    pub fn matcher_node(&self, matcher: ExprNode) -> Matcher<'_>
    {
        Matcher::new(matcher, &self.term_provider)
    }
}
