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
    QUOTATION,
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
        write!(f, "{}", self.value)
    }
}

impl Token {
    pub fn to_string(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Default)]
pub struct Scanner {
    contents: Vec<char>,
    row: u32,
    col: u32,
    index: usize,
}

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

    /// UNSAFE
    /// Trusts that the left value read was ", looking for next "
    pub fn _read_string(&mut self) -> String {
        let mut result = String::new();
        while self.contents[self.index] != '"' {
            result.push(self.contents[self.index]);
            self.index += 1;
        }
        self.index += 1;
        result
    }

    pub fn token(&mut self) -> Option<Token> {
        if self.index >= self.contents.len() {
            return None;
        }
        self.col += 1;
        self.index += 1;
        Some(match self.contents[self.index - 1] {
            '{' => self.new_token(TokenType::LBRACE, "{"),
            '}' => self.new_token(TokenType::RBRACE, "}"),
            ';' => self.new_token(TokenType::SEMI, ";"),
            ')' => self.new_token(TokenType::RPAREN, ")"),
            '(' => self.new_token(TokenType::LPAREN, "("),
            '+' => self.new_token(TokenType::PLUS, "+"),
            '-' => self.new_token(TokenType::MINUS, "-"),
            '*' => self.new_token(TokenType::MULT, "*"),
            '"' => self.new_token(TokenType::QUOTATION, "\""),
            ' ' | '\t' => self.token()?,
            '\n' => {
                // self.index += 1;
                self.row += 1;
                self.col = 0;
                // print!("newline");
                self.token()?
            }
            '=' => {
                // self.index += 1;
                if self.contents[self.index] == '=' {
                    self.index += 1;
                    self.new_token(TokenType::EQUALITY, "==")
                } else {
                    self.new_token(TokenType::ASSIGN, "=")
                }
            }

            '/' => {
                // self.index += 1;
                if self.contents[self.index] == '/' {
                    // Single line comment
                    while self.contents[self.index] != '\n' {
                        self.index += 1;
                    }
                    self.index += 1;
                    self.token()?
                } else if self.contents[self.index] == '*' {
                    // Multiple line comment
                    while self.contents[self.index] != '*' && self.contents[self.index + 1] != '/' {
                        self.index += 1;
                    }
                    self.index += 2;
                    self.token()?
                } else {
                    self.new_token(TokenType::DIV, "/")
                }
            }
            '<' => {
                // self.index += 1;
                if self.contents[self.index] == '=' {
                    self.index += 1;
                    self.new_token(TokenType::LESSEQ, "<=")
                } else {
                    self.new_token(TokenType::LESS, "<")
                }
            }
            '>' => {
                // self.index += 1;
                if self.contents[self.index] == '=' {
                    self.index += 1;
                    self.new_token(TokenType::GREATEREQ, ">=")
                } else {
                    self.new_token(TokenType::GREATER, ">")
                }
            }
            _ => {
                self.index -= 1;
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
                        self.new_token(TokenType::FLOATLIT, &literal)
                    } else {
                        self.new_token(TokenType::INTLIT, &literal)
                    }
                } else if is_alpha(self.contents[self.index]) {
                    // ID
                    while is_alpha(self.contents[self.index]) | is_num(self.contents[self.index]) {
                        literal.push(self.contents[self.index]);
                        self.index += 1;
                    }
                    self.reserved_words(literal)
                } else {
                    panic!("Unexpected character: {}", self.contents[self.index]);
                }
            }
        })
    }
}
