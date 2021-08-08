
#[macro_use] extern crate lalrpop_util;

pub mod ast;
pub mod parser;
pub mod matcher;
pub mod tree;
pub mod proof_check;


use std::{env::args, error::Error, fs::File, io::{self, BufRead, BufReader, Read}, rc::Rc};

use proof_check::Proof;

use crate::{ast::{Expr, ExprNode}, matcher::{GetSubsts}, parser::ExprManager};

fn consume_reader<R: Read>(mut reader: BufReader<R>) -> (String, Vec<String>)
{
    let mut to_prove = String::new();
    reader.read_line(&mut to_prove).unwrap();
    to_prove += "$";

    let proof = reader.lines()
        .map(|line| line.unwrap() + "$")
        .collect();

    (to_prove, proof)
}

fn read_all() -> (String, Vec<String>)
{
    let mut args_iter = args();
    args_iter.next();
    if let Some(file_name) = args_iter.next() {
        consume_reader(BufReader::new(File::open(&file_name).unwrap()))
    } else {
        consume_reader(BufReader::new(io::stdin()))
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let manager = ExprManager::new();
    let (to_prove, proof) = read_all();

    let to_prove = manager.parse_proved(&to_prove);
    let proof = proof.into_iter()
        .map(|as_str| manager.parse(&as_str))
        .collect();

    let proof = Proof::new(to_prove, proof);
    Ok(())
}
