
#[macro_use] extern crate lalrpop_util;

pub mod ast;
pub mod parser;
pub mod matcher;
pub mod tree;
pub mod proof_check;


use std::{env::args, error::Error, fs::File, io::{self, BufRead, BufReader, Write}};

use proof_check::{ProofChecker};

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
    reader.read_line(&mut to_prove).expect("I wanted to read this line so hard...");
    let proved = manager.parse_proved(&to_prove);

    let line_proofs: Vec<_> = reader.lines()
        .map(|mb_line| mb_line.expect("I really wanted to read this line"))
        .map(|line| manager.parse(&line))
        .map(|expr| {
            proof_checker.match_schemes_and_axioms(&expr)
        })
        .fold(Vec::new(), |mut proved, BaseExpr{expr, mut proof} | {
            proof = proof.or_else(|err| {
                check_rules(&expr, &proved)
                    .map_err(|rules_err| rules_err.min(err))
            });
            proved.push(BaseExpr{ expr, proof });
            proved
        });

    let mut bases = Vec::new();
    let fst_wrong = line_proofs.iter()
        .find_map(|BaseExpr { expr, proof }| {
            match proof {
                Ok(base) => {
                    bases.push((expr, base));
                    None
                },
                Err(err) => {
                    Some(err)
                }
            }
        });
    {
        let stdout = io::stdout();
        let mut out_lock = stdout.lock();

        writeln!(out_lock, "|-{}", proved).unwrap();
        bases.iter()
            .enumerate()
            .for_each(|(num, (expr, base))| {
                let num = num + 1;
                writeln!(out_lock, "[{}. {}] {}", num, base, expr).unwrap();
            });

        let (last, _) = bases.last()
            .unwrap_or_else(|| panic!("Empty proof proves nothing."));

        if let Some(err) = fst_wrong {
            writeln!(out_lock, "Expression {}: {}.", bases.len() + 1, err).unwrap();
        } else if *last != &proved {
            writeln!(out_lock, "The proof proves different expression.").unwrap();
        }
    }
    Ok(())
}
