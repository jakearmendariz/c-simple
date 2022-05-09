extern crate lazy_static;
mod ast;
mod parser;
mod scanner;
mod sized_string;

use crate::parser::Parser;

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

fn parse(input: Vec<char>) -> Result<()> {
    let mut parser = Parser::new(input);
    parser.parse().map_err(|e| e.into())
}

fn main() {
    match read_input() {
        Ok(chars) => match parse(chars) {
            Ok(_) => println!("Successful parse"),
            Err(e) => {
                println!("{}", e)
            }
        },
        Err(e) => println!("{}", e),
    };
}
