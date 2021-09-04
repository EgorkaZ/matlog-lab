
#[macro_use] extern crate lalrpop_util;

pub mod ast;
pub mod parser;
pub mod matcher;
pub mod tree;
pub mod proof_check;


use std::{env::args, error::Error, fs::File, io::{self, BufRead, BufReader}, iter};

use proof_check::{Cringe, ProofChecker};

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
    let proof_checker = ProofChecker::new(&manager);
    let mut reader = get_reader();

    let mut to_prove = String::new();
    reader.read_line(&mut to_prove);
    let to_prove = manager.parse_proved(&to_prove);

    let proof = reader.lines()
        .map(|mb_line| mb_line.expect("I really wanted to read this line"))
        .map(|line| manager.parse(&line))
        .map(|expr| {
            proof_checker.match_schemes_and_axioms(&expr)
        })
        .fold(Vec::new(), |mut proved, BaseExpr{expr, mut proof} | {
            proof = proof.or_else(|_| check_rules(&expr, &proved));
            proved.push(BaseExpr{ expr, proof });
            proved
        })
        .into_iter();

    iter::once(BaseExpr{ expr: to_prove, proof: Err(Cringe::casual_cringe()) })
        .chain(proof)
        .enumerate()
        .for_each(|(idx, BaseExpr{expr, proof})| {
            print!("{}: ", idx);
            match &proof {
                Ok(base) => println!("{} [{:?}]", expr, base),
                Err(_) => println!("{} [Wrong]", expr),
            };
        });

    // let proof = Proof::new(manager.parse_proved(&to_prove), some_proved);

    // proof_checker.check_proof(&proof)
    //     .into_iter()
    //     .for_each(|BaseExpr{expr, proof}| match proof {
    //         Ok(base) => println!("{} [{:?}]", expr, base),
    //         Err(_) => println!("{} [Wrong]", expr),
    //     });
    Ok(())
}
