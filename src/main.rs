
#[macro_use] extern crate lalrpop_util;
#[macro_use] extern crate smallvec;

pub mod ast;
pub mod parser;
pub mod matcher;
pub mod tree;
pub mod proof_check;
pub mod natural_proof;


use std::{env::args, error::Error, fs::File, io::{self, BufRead, BufReader}};

use proof_check::{ProofChecker};
use rustc_hash::FxHashMap;

use crate::{parser::ExprManager, proof_check::{check_rules}};

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

fn main() -> Result<(), Box<dyn Error>> {
    let manager = ExprManager::new();
    let mut reader = get_reader();

    let mut to_prove = String::new();
    reader.read_line(&mut to_prove).expect("I wanted to read this line so hard...");
    let (hypothesis, proved) = manager.parse_proved(&to_prove);

    let proof_checker = ProofChecker::new(&manager, &hypothesis);

    let expressions: Vec<_> = reader.lines()
        .map(|mb_line| mb_line.expect("I wanted this line soo much..."))
        .map(|line| manager.parse(&line))
        .collect();

    match expressions.last() {
        Some(last) if last == &proved => (/* ok */),
        _ => {
            println!("The proof does not proof the required expression");
            return Ok(())
        },
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

    match proof_or_fst_wrong {
        Ok(_proofs) => (),
        Err(first_wrong) => println!("The proof is incorrect at line {}", first_wrong),
    }


    Ok(())
}
