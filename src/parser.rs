use crate::scanner::{Scanner, Token, TokenType};
pub struct Parser {
    scanner: Scanner,
    to_match: Option<Token>,
}
#[derive(Debug)]
pub struct ParseError {
    line: u32,
    actual: TokenType,
    expected: Vec<TokenType>,
}

impl ParseError {
    fn new(actual: Token, expected: Vec<TokenType>) -> Self {
        Self {
            line: actual.row,
            actual: actual.token_type,
            expected,
        }
    }

    fn eof(expected: Vec<TokenType>) -> Self {
        Self {
            line: 0,
            actual: EOF,
            expected,
        }
    }
}

use TokenType::*;

use std::fmt;
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&match self.actual {
            TokenType::EOF => format!("Parsing Error, end of file, expected {:?}", self.expected),
            _ => format!(
                "Parsing Error on line {}, expected {:?}, saw {:?}",
                self.line, self.expected, self.actual
            ),
        })
    }
}

impl std::error::Error for ParseError {
    fn description(&self) -> &str {
        "Parse Error"
    }
}

type ParseResult<T> = Result<T, ParseError>;

impl Parser {
    pub fn new(contents: Vec<char>) -> Self {
        Self {
            scanner: Scanner::new(contents),
            to_match: None,
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        if self.to_match.is_none() {
            self.to_match = self.scanner.token();
        }
        self.to_match
    }

    /// Eats the next token, if too match is not none it will eat that
    fn eat(&mut self, expect: Vec<TokenType>) -> ParseResult<Token> {
        if self.to_match.is_none() {
            self.to_match = self.next_token();
        }
        let token = match self.to_match {
            Some(token) => token,
            None => {
                return Err(ParseError {
                    line: 0,
                    actual: EOF,
                    expected: expect,
                })
            }
        };
        self.to_match = None;
        if !expect.contains(&token.token_type) {
            Err(ParseError::new(token, expect))
        } else {
            Ok(token)
        }
    }

    fn parse_expr(&mut self) -> ParseResult<()> {
        self.parse_addsub()?;
        self.parse_expr2()?;
        Ok(())
    }

    fn parse_expr2(&mut self) -> ParseResult<()> {
        let token = self
            .next_token()
            .ok_or_else(|| ParseError::eof(vec![RPAREN, LBRACE, SEMI, EQUALITY, LESS, GREATER]))?;
        match token.token_type {
            RPAREN | LBRACE | SEMI => Ok(()),
            EQUALITY | LESS | GREATER => {
                self.to_match = None;
                self.parse_addsub()?;
                self.parse_expr2()?;
                Ok(())
            }
            _ => Err(ParseError::new(
                token,
                vec![RPAREN, LBRACE, SEMI, EQUALITY, LESS, GREATER],
            )),
        }
    }

    fn parse_addsub(&mut self) -> ParseResult<()> {
        self.parse_factor()?;
        self.parse_addsub2()?;
        Ok(())
    }

    fn parse_addsub2(&mut self) -> ParseResult<()> {
        let token = self.next_token().ok_or_else(|| {
            ParseError::eof(vec![
                RPAREN, LBRACE, SEMI, EQUALITY, LESS, GREATER, PLUS, MINUS,
            ])
        })?;
        match token.token_type {
            PLUS | MINUS => {
                self.to_match = None;
                self.parse_addsub()?;
                self.parse_expr2()?;
                Ok(())
            }
            RPAREN | LBRACE | SEMI | EQUALITY | LESS | GREATER => Ok(()),
            _ => Err(ParseError::new(
                token,
                vec![RPAREN, LBRACE, SEMI, EQUALITY, LESS, GREATER, PLUS, MINUS],
            )),
        }
    }

    fn parse_factor(&mut self) -> ParseResult<()> {
        let token = self.eat(vec![LPAREN, ID, INTLIT, FLOATLIT])?;
        match token.token_type {
            LPAREN => {
                self.parse_expr()?;
                self.eat(vec![RPAREN])?;
            }
            _ => {
                self.to_match = None;
            }
        };
        Ok(())
    }

    /// Parses `if expr statement (else statement)?`
    fn parse_if(&mut self) -> ParseResult<()> {
        self.eat(vec![TokenType::IF])?;
        self.to_match = None;
        self.parse_expr()?;
        self.parse_statement()?;
        if let Some(token) = self.next_token() {
            if token.token_type == TokenType::ELSE {
                self.eat(vec![ELSE])?;
                self.parse_statement()?;
            }
        }
        Ok(())
    }

    fn parse_for(&mut self) -> ParseResult<()> {
        Ok(())
    }

    fn parse_block(&mut self) -> ParseResult<()> {
        self.eat(vec![LBRACE])?;
        while let Some(token) = self.next_token() {
            match token.token_type {
                RBRACE => break,
                _ => self.parse_statement()?,
            };
        }
        if self.to_match.is_none() {
            return Err(ParseError::eof(vec![RBRACE]));
        }
        self.eat(vec![RBRACE])?;
        Ok(())
    }

    fn parse_assignment(&mut self) -> ParseResult<()> {
        use TokenType::*;
        match self.next_token().unwrap().token_type {
            INT | FLOAT => {
                self.to_match = None;
                self.eat(vec![TokenType::ID])?;
            }
            ID => {
                self.to_match = None;
            }
            _ => unreachable!(),
        };
        self.eat(vec![ASSIGN])?;
        self.parse_expr()?;
        self.eat(vec![SEMI])?;
        Ok(())
    }

    fn parse_statement(&mut self) -> ParseResult<Option<()>> {
        match self.next_token() {
            Some(token) => {
                use TokenType::*;
                match token.token_type {
                    IF => self.parse_if()?,
                    FOR => self.parse_for()?,
                    INT | FLOAT | ID => self.parse_assignment()?,
                    LBRACE => self.parse_block()?,
                    actual => {
                        return Err(ParseError {
                            line: token.row,
                            expected: vec![IF, FOR, ID],
                            actual,
                        })
                    }
                };
                Ok(Some(()))
            }
            None => Ok(None),
        }
    }

    pub fn parse(&mut self) -> Result<(), ParseError> {
        while self.parse_statement()?.is_some() {}
        Ok(())
    }
}
