
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
    let proof_checker = ProofChecker::new(&manager);
    let mut reader = get_reader();

    let mut to_prove = String::new();
    reader.read_line(&mut to_prove).expect("I wanted to read this line so hard...");
    let proved = manager.parse_proved(&to_prove);

    let mut line_proofs: Vec<_> = reader.lines()
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

    {
        let stdout = io::stdout();
        let mut out_lock = stdout.lock();

        write!(out_lock, "{}", to_prove).unwrap();
        let last = line_proofs.pop();
        let first_wrong = {
            line_proofs[..line_proofs.len() - 1].iter()
                .enumerate()
                .find_map(|(num, BaseExpr{expr, proof})| {
                    let num = num + 1;
                    match proof {
                        Ok(base) => {
                            writeln!(out_lock, "[{}. {}] {}", num, base, expr).unwrap();
                            None
                        },
                        Err(error) => {
                            Some((num, error))
                        }
                    }
                })
        };

        let mut print_error = |num, error| {
            write!(out_lock, "Expression {}", num).unwrap();
            if !matches!(error, &Wrong::Unproved) {
                write!(out_lock, ":").unwrap();
            }
            writeln!(out_lock, " {}.", error).unwrap();
        };

        if let Some((num, error)) = first_wrong {
            print_error(num, error);
        } else {
            if let Some(BaseExpr{ expr, proof }) = last {
                let num = line_proofs.len() + 1;
                match proof {
                    Ok(base) if proved == expr => writeln!(out_lock, "[{}. {}] {}", num, base, expr).unwrap(),
                    Err(error) => print_error(num, &error),
                    _ => writeln!(out_lock, "The proof proves different expression.").unwrap(),
                }
            } else {
                writeln!(out_lock, "The proof proves different expression.").unwrap();
            }
        }
    }
    Ok(())
}
