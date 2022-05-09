use crate::sized_string::SizedString;
use std::fmt::Display;
use std::string::ToString;
// With match statements
//  1.76s user 0.04s system 98% cpu 1.823 total
#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    ID,
    FOR,
    IF,
    WHILE,
    ELSE,
    INT,
    FLOAT,
    INTLIT,
    FLOATLIT,
    PLUS,
    MINUS,
    MULT,
    DIV,
    ASSIGN,
    EQUALITY,
    LESS,
    LESSEQ,
    GREATER,
    GREATEREQ,
    SEMI,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    EOF,
}

fn is_num(a: char) -> bool {
    ('0'..='9').contains(&a)
}

fn is_alpha(a: char) -> bool {
    ('a'..='z').contains(&a) | ('A'..='Z').contains(&a)
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub token_type: TokenType,
    pub value: SizedString,
    pub row: u32,
    col: u32,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{:?}, {} at ({}, {})",
            self.token_type, self.value, self.row, self.col
        )
    }
}

#[derive(Default)]
pub struct Scanner {
    contents: Vec<char>,
    row: u32,
    col: u32,
    index: usize,
}

// Creating these lazy static variable seems to slow down computation.
// use std::collections::HashMap;
// lazy_static::lazy_static! {
//     pub static ref KEYWORDS: HashMap<&'static str, TokenType> = {
//         use TokenType::{*};
//         let mut reserved: HashMap<&'static str, TokenType> = HashMap::with_capacity(6);
//         reserved.insert("for", FOR);
//         reserved.insert("while", WHILE);
//         reserved.insert("int", INT);
//         reserved.insert("float", FLOAT);
//         reserved.insert("else", ELSE);
//         reserved.insert("if", IF);
//         reserved
//     };
// }
// lazy_static::lazy_static! {
//     pub static ref SINGLE_CHARACTER_TOKENS: HashMap<char, TokenType> = {
//         use TokenType::{*};
//         let mut tokens: HashMap<char, TokenType> = HashMap::with_capacity(10);
//         tokens.insert('{', LBRACE);
//         tokens.insert('}', RBRACE);
//         tokens.insert('(', LPAREN);
//         tokens.insert(')', RPAREN);
//         tokens.insert('+', PLUS);
//         tokens.insert('-', MINUS);
//         tokens.insert('*', MULT);
//         tokens.insert('/', DIV);
//         tokens.insert(';', SEMI);
//         tokens
//     };
// }

impl Scanner {
    pub fn new(contents: Vec<char>) -> Self {
        Self {
            contents,
            row: 1,
            col: 0,
            index: 0,
        }
    }

    fn new_token<T: ToString>(&mut self, token_type: TokenType, value: T) -> Token {
        Token {
            token_type,
            value: SizedString::new(value).unwrap(),
            row: self.row,
            col: self.col,
        }
    }

    fn reserved_words(&mut self, literal: String) -> Token {
        // TODO: Changing this to a static hashmap would be faster.
        use TokenType::*;
        let token_type = match literal.as_str() {
            "for" => FOR,
            "if" => IF,
            "while" => WHILE,
            "else" => ELSE,
            "int" => INT,
            "float" => FLOAT,
            _ => ID,
        };
        self.new_token(token_type, literal)
    }

    pub fn token(&mut self) -> Option<Token> {
        if self.index >= self.contents.len() {
            return None;
        }
        self.col += 1;
        // match SINGLE_CHARACTER_TOKENS.get(&contents[self.index]) {
        //     Some(token_type) => {
        //         self.index += 1;
        //         return Some(self.new_token(*token_type, contents[self.index]));
        //     },
        //     None => (),
        // };

        match self.contents[self.index] {
            '{' => {
                self.index += 1;
                Some(self.new_token(TokenType::LBRACE, "{"))
            }
            '}' => {
                self.index += 1;
                Some(self.new_token(TokenType::RBRACE, "}"))
            }
            ' ' | '\t' => {
                self.index += 1;
                self.token()
            }
            '\n' => {
                self.index += 1;
                self.row += 1;
                self.col = 0;
                // print!("newline");
                self.token()
            }
            '=' => {
                self.index += 1;
                Some(if self.contents[self.index + 1] == '=' {
                    self.new_token(TokenType::EQUALITY, "==")
                } else {
                    self.new_token(TokenType::ASSIGN, "=")
                })
            }
            '+' => {
                self.index += 1;
                Some(self.new_token(TokenType::PLUS, "+"))
            }
            '-' => {
                self.index += 1;
                Some(self.new_token(TokenType::MINUS, "-"))
            }
            '*' => {
                self.index += 1;
                Some(self.new_token(TokenType::MULT, "*"))
            }
            '/' => {
                self.index += 1;
                Some(self.new_token(TokenType::DIV, "/"))
            }
            '<' => {
                self.index += 1;
                if self.contents[self.index] == '=' {
                    Some(self.new_token(TokenType::LESS, "<"))
                } else {
                    Some(self.new_token(TokenType::LESSEQ, "<="))
                }
            }
            '>' => {
                self.index += 1;
                if self.contents[self.index] == '=' {
                    Some(self.new_token(TokenType::GREATER, ">"))
                } else {
                    Some(self.new_token(TokenType::GREATEREQ, ">="))
                }
            }
            ';' => {
                self.index += 1;
                Some(self.new_token(TokenType::SEMI, ";"))
            }
            ')' => {
                self.index += 1;
                Some(self.new_token(TokenType::RPAREN, ")"))
            }
            '(' => {
                self.index += 1;
                Some(self.new_token(TokenType::LPAREN, "("))
            }
            _ => {
                let mut literal = String::new();
                if is_num(self.contents[self.index]) {
                    // NUMBER
                    while self.index < self.contents.len() && is_num(self.contents[self.index]) {
                        literal.push(self.contents[self.index]);
                        self.index += 1;
                    }
                    if self.contents[self.index] == '.' {
                        literal.push('.');
                        self.index += 1;
                        while self.index < self.contents.len() && is_num(self.contents[self.index])
                        {
                            literal.push(self.contents[self.index]);
                            self.index += 1;
                        }
                        Some(self.new_token(TokenType::FLOATLIT, &literal))
                    } else {
                        Some(self.new_token(TokenType::INTLIT, &literal))
                    }
                } else if is_alpha(self.contents[self.index]) {
                    // ID
                    while is_alpha(self.contents[self.index]) | is_num(self.contents[self.index]) {
                        literal.push(self.contents[self.index]);
                        self.index += 1;
                    }
                    Some(self.reserved_words(literal))
                } else {
                    panic!("Unexpected character: {}", self.contents[self.index]);
                }
            }
        }
    }
}
