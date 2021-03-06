extern crate lazy_static;
mod ast;
mod ir;
mod parser;
mod scanner;
mod sized_string;
mod type_check;

use crate::ast::Ast;
use crate::ir::ast_to_instructions;
use crate::parser::Parser;
use crate::type_check::type_check;

use std::error;

type Result<T> = std::result::Result<T, Box<dyn error::Error>>;

fn read_input() -> Result<Vec<char>> {
    let filename = match std::env::args().nth(1) {
        Some(filename) => filename,
        None => panic!("Expected a filename"),
    };
    let file_content = std::fs::read_to_string(&filename)
        .unwrap_or_else(|_| panic!("Cannot read file {}", filename));
    let bytes = file_content.as_bytes();
    let chars: Vec<char> = bytes.iter().map(|b| *b as char).collect();
    Ok(chars)
}

fn parse(input: Vec<char>) -> Result<Vec<Ast>> {
    let mut parser = Parser::new(input);
    parser.parse().map_err(|e| e.into())
}

fn main() {
    match read_input() {
        Ok(chars) => match parse(chars) {
            Ok(asts) => {
                // println!("{:?}", asts);
                match type_check(asts) {
                    Ok(typed_ast) => {
                        println!("{}", typed_ast);
                        let instrs = ast_to_instructions(typed_ast);
                        instrs.iter().for_each(move |instr| println!("{}", instr));
                    }
                    Err(e) => println!("{}", e),
                }
            }
            Err(e) => {
                println!("{}", e)
            }
        },
        Err(e) => println!("{}", e),
    };
}
