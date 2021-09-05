use std::fmt::{Debug};

use lalrpop_util::ParseError;
use lalrpop_util::lexer::Token;

use crate::ast::{ExprNode, ExprProvider};

use super::terms::{ExprParser};

struct ParserTester
{
    expr_provider: ExprProvider,

    expr_parser: ExprParser,
}

#[allow(unused)]
impl ParserTester
{
    fn new() -> Self
    {
        ParserTester{
            expr_provider: ExprProvider::new(),
            expr_parser: ExprParser::new(),
        }
    }

    fn assert_parse_res<Node>(&self, input_and_expected: &[(&str, Node)])
        where Self: Parse<Node>,
              Node: Debug + Eq,
    {
        input_and_expected.iter()
            .for_each(|(input, expected)| {
                match self.parse(*input) {
                    Ok(res) => assert_eq!(&res, expected),
                    Err(err) =>
                        panic!("couldn't parse expr: '{}', resulted in error: {}", input, err),
                }
            })
    }
}

trait Parse<Node>
{
    fn parse<'input>(&self, as_str: &'input str) -> Result<Node, ParseError<usize, Token<'input>, &'static str>>;
}

impl Parse<ExprNode> for ParserTester
{
    fn parse<'input>(&self, as_str: &'input str) -> Result<ExprNode, ParseError<usize, Token<'input>, &'static str>> {
        self.expr_parser.parse(&self.expr_provider, as_str)
    }
}
