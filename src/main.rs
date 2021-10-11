
#[macro_use] extern crate lalrpop_util;
#[macro_use] extern crate smallvec;

pub mod ast;
pub mod parser;
pub mod matcher;
pub mod tree;
pub mod proof_check;
pub mod natural_proof;


use std::{env::args, error::Error, fs::File, io::{self, BufRead, BufReader, BufWriter, Write}, rc::{Rc}};

use ast::{ExprNode};
use mset::MultiSet;
use natural_proof::BaseNode;
use proof_check::{ProofChecker};
use rustc_hash::FxHashMap;

use crate::{natural_proof::rebuild_to_natural, parser::ExprManager, proof_check::{check_rules}};

fn get_reader() -> Box<dyn BufRead>
{
    let mut args_iter = args();
    args_iter.next();
    if let Some(file_name) = args_iter.next() {
        Box::new(BufReader::new(File::open(&file_name).unwrap()))
    } else {
        Box::new(BufReader::new(io::stdin()))
    }
}

fn print_nat_base_tree(
    writer: &mut impl Write,
    base: &BaseNode,
    hyp: &[ExprNode]) -> Result<(), io::Error>
{
    let mut added_hyp = vec![];
    print_nat_base_tree_dfs(writer, base, hyp, &mut added_hyp, 0)
}

fn print_hypothesis(writer: &mut impl Write, hyp: &[ExprNode]) -> Result<(), io::Error>
{
    let mut iter = hyp.iter();
    if let Some(first) = iter.next() {
        write!(writer, "{}", first)?;
        iter.for_each(|curr| write!(writer, ",{}", curr).unwrap());
    }
    Ok(())
}

fn print_nat_base_tree_dfs(
    writer: &mut impl Write,
    base: &BaseNode,
    hyp: &[ExprNode],
    added_hyp: &mut Vec<ExprNode>,
    depth: u32) -> Result<(), io::Error>
{
    for (child, new_hyp) in base.children() {
        if let Some(to_push) = new_hyp {
            added_hyp.push(Rc::clone(to_push));
        }
        print_nat_base_tree_dfs(writer, child, hyp, added_hyp, depth + 1)?;
        if new_hyp.is_some() {
            added_hyp.pop();
        }
    }

    let neither_is_empty = !(hyp.is_empty() || added_hyp.is_empty());

    write!(writer, "[{}] ", depth)?;
    print_hypothesis(writer, hyp)?;
    if neither_is_empty {
        write!(writer, ", ")?;
    }
    print_hypothesis(writer, added_hyp)?;
    writeln!(writer, "|- {} [{}]", base.curr(), base.shift())?;
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let manager = ExprManager::new();
    let mut reader = get_reader();

    let mut to_prove = String::new();
    reader.read_line(&mut to_prove).expect("I wanted to read this line so hard...");
    let (hypothesis, proved) = manager.parse_proved(&to_prove);

    let hypothesis_set = hypothesis.iter()
        .fold(Default::default(), |mut set: MultiSet<_>, expr| {
            set.insert(Rc::clone(expr));
            set
        });
    let proof_checker = ProofChecker::new(&manager, &hypothesis_set);

    let expressions: Vec<_> = reader.lines()
        .map(|mb_line| mb_line.expect("I wanted this line soo much..."))
        .map(|line| manager.parse(&line))
        .collect();

    match expressions.last() {
        Some(last) if last == &proved => (/* ok */),
        _ => {
            println!("The proof does not proof the required expression");
            return Ok(())
        }
    }

    let proof_or_fst_wrong = expressions.into_iter()
        .map(|expr| {
            let proof = proof_checker.check_single_expr(&expr);
            (expr, proof)
        })
        .enumerate()
        .fold(Ok(FxHashMap::default()), |mb_proved, (num, (expr, proof))| {
            mb_proved.and_then(|mut proved| {
                proof
                    .or_else(|_| check_rules(&expr, &proved))
                    .map(move |base| {
                        proved.insert(expr, base);
                        proved
                    })
                    .map_err(|_| num + 1)
            })
        });

    let stdout = io::stdout();
    let mut writer = BufWriter::new(stdout.lock());

    match proof_or_fst_wrong {
        Ok(proofs) => {
            let natural_proof = rebuild_to_natural(&proofs, manager.provider(), &proved);
            print_nat_base_tree(&mut writer, &natural_proof, &hypothesis)?;
        },
        Err(first_wrong) => writeln!(writer, "Proof is incorrect at line {}", first_wrong + 1)?,
    }


    Ok(())
}
