use std::fmt::{Debug};

use lalrpop_util::ParseError;
use lalrpop_util::lexer::Token;

use crate::ast::{ExprNode, ExprProvider, TermNode, TermProvider};

use super::{preprocess};
use super::terms::{TermParser, ExprParser};

struct ParserTester
{
    term_provider: TermProvider,
    expr_provider: ExprProvider,

    term_parser: TermParser,
    expr_parser: ExprParser,
}

#[allow(unused)]
impl ParserTester
{
    fn new() -> Self
    {
        ParserTester{
            term_provider: TermProvider::new(),
            expr_provider: ExprProvider::new(),
            term_parser: TermParser::new(),
            expr_parser: ExprParser::new(),
        }
    }

    fn assert_parse_res<Node>(&self, input_and_expected: &[(&str, Node)])
        where Self: Parse<Node>,
              Node: Debug + Eq,
    {
        input_and_expected.iter()
            .for_each(|(input, expected)| {
                let preprocessed = preprocess(input.chars());
                match self.parse(&preprocessed) {
                    Ok(res) => assert_eq!(&res, expected),
                    Err(err) =>
                        panic!("couldn't parse expr: '{}', after preprocessing: '{}', resulted in error: {}", input, preprocessed, err),
                }
            })
    }
}

trait Parse<Node>
{
    fn parse<'input>(&self, as_str: &'input str) -> Result<Node, ParseError<usize, Token<'input>, &'static str>>;
}

impl Parse<TermNode> for ParserTester
{
    fn parse<'input>(&self, as_str: &'input str) -> Result<TermNode, ParseError<usize, Token<'input>, &'static str>> {
        self.term_parser.parse(&self.term_provider, &self.expr_provider, as_str)
    }
}

impl Parse<ExprNode> for ParserTester
{
    fn parse<'input>(&self, as_str: &'input str) -> Result<ExprNode, ParseError<usize, Token<'input>, &'static str>> {
        self.expr_parser.parse(&self.term_provider, &self.expr_provider, as_str)
    }
}

#[cfg(test)]
#[test]
fn parse_vars()
{
    let tester = ParserTester::new();
    let t_pr = || &tester.term_provider;

    let tests = [
        ("a", t_pr().var('a')),
        ("(b)", t_pr().var('b')),
        ("  c  ", t_pr().var('c')),
        ("  d", t_pr().var('d')),
        ("(e  )", t_pr().var('e')),
        ("meta:Var", t_pr().repl("meta"))
    ];

    tester.assert_parse_res(&tests);
}

#[cfg(test)]
#[test]
fn parse_terms()
{
    let tester = ParserTester::new();
    let t_pr = || &tester.term_provider;

    let a = t_pr().var('a');
    let b = t_pr().var('b');
    let c = t_pr().var('c');

    let a_plus_a= t_pr().add(&a, &a);
    let b_mul_b = t_pr().mul(&b, &b);

    let b_plus_c = t_pr().add(&b, &c);
    let a_mul_b_plus_c = t_pr().mul(&a, &b_plus_c);

    tester.assert_parse_res(&[
        ("a + a", a_plus_a.clone()),
        ("( a) + ( a )", a_plus_a.clone()),
        ("((a) +   a)", a_plus_a.clone()),

        ("b*b", b_mul_b.clone()),
        ("b * (((   b )))", b_mul_b.clone()),

        ("(b + c)", b_plus_c.clone()),
        ("(((((b))+c)))", b_plus_c.clone()),
        ("   b     +  (c)    ", b_plus_c.clone()),

        ("a * (b + c)", a_mul_b_plus_c.clone()),
        ("(((  a)   ) * (((b)) + c  )) ", a_mul_b_plus_c.clone())
    ])
}

#[cfg(test)]
#[test]
fn parse_preds()
{
    let tester = ParserTester::new();
    let e_pr = || &tester.expr_provider;

    tester.assert_parse_res(&[
        ("A ", e_pr().pred('A')),
        ("(((B))) ", e_pr().pred('B')),
        ("   (C) ", e_pr().pred('C')),
        (" ((  sth:Pred)   ) ", e_pr().repl("sth"))
    ])
}

#[cfg(test)]
#[test]
fn parse_equality()
{
    let tester = ParserTester::new();
    let t_pr = || &tester.term_provider;
    let e_pr = || &tester.expr_provider;

    let a = t_pr().var('a');
    let b = t_pr().var('b');

    let a_plus_b = t_pr().add(&a, &b);
    let a_mul_a = t_pr().mul(&a, &a);

    tester.assert_parse_res(&[
        ("a+b=a*a ", e_pr().eq(&a_plus_b, &a_mul_a))
    ])
}

#[cfg(test)]
#[test]
fn parse_folded_quantors()
{
    use std::marker::PhantomData;

    use crate::ast::TermVar;

    let tester = ParserTester::new();
    let t_pr = || &tester.term_provider;
    let e_pr = || &tester.expr_provider;

    let x = t_pr().var('x');
    let y = t_pr().var('y');
    let x_eq_y = e_pr().eq(&x, &y);
    let xist_y = e_pr().ext(TermVar::Static('y', PhantomData), &x_eq_y);
    let xist_x = e_pr().ext(TermVar::Static('x', PhantomData), &xist_y);

    let a_pred = e_pr().pred('A');
    let b_pred = e_pr().pred('B');
    let a_and_b = e_pr().conj(&a_pred, &b_pred);

    let scheme = e_pr().imp(&a_and_b, &e_pr().imp(&xist_x, &a_and_b));

    tester.assert_parse_res(&[
        ("(?y.x=y)", xist_y),
        ("?x.?y.x=y", xist_x),
        ("A & B -> (?x.?y. x = y) -> A&B", scheme),
    ]);
}
