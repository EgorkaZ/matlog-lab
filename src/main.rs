
#[macro_use] extern crate lalrpop_util;

pub mod ast;
pub mod parser;
pub mod matcher;
pub mod tree;
pub mod proof_check;


use std::{env::args, error::Error, fs::File, io::{self, BufRead, BufReader, Write}};

use proof_check::{Wrong, ProofChecker};

use crate::{parser::ExprManager, proof_check::{BaseExpr, check_rules}};

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

    let proof = reader.lines()
        .map(|mb_line| mb_line.expect("I wanted this line soo much..."))
        .map(|line| manager.parse(&line))
        .map(|expr| proof_checker.check_single_expr(&expr))
        .fold(Vec::new(), |mut proved, BaseExpr{ expr, mut proof }| {
            proof = proof.or_else(|_| check_rules(&expr, &proved));
            proved.push(BaseExpr{ expr, proof });
            proved
        });

    proof.iter()
        .enumerate()
        .for_each(|(num, BaseExpr{ expr, proof })| {
            let num = num + 1;
            print!("{}: {} ", num, expr);
            match proof {
                Ok(base) => println!("[{}]", base),
                Err(..) => println!("[Unproved]"),
            }
        });
    Ok(())
}
