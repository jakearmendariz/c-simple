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

    /// End of file error
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

static DEBUG: bool = false;
type ParseResult<T> = Result<T, ParseError>;

impl Parser {
    pub fn new(contents: Vec<char>) -> Self {
        Self {
            scanner: Scanner::new(contents),
            to_match: None,
        }
    }

    fn debug(&mut self, s: &str) {
        if DEBUG {
            println!("{}", s);
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        if self.to_match.is_none() {
            self.to_match = self.scanner.token();
        }
        self.to_match
    }

    fn eat_single(&mut self, expect: TokenType) -> ParseResult<Token> {
        self.eat(vec![expect])
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
        self.debug("parse_expr");
        self.parse_addsub()?;
        self.parse_expr2()?;
        Ok(())
    }

    fn parse_expr2(&mut self) -> ParseResult<()> {
        self.debug("parse_expr2");
        let token = self
            .next_token()
            .ok_or_else(|| ParseError::eof(vec![RPAREN, LBRACE, SEMI, EQUALITY, LESS, GREATER, LESSEQ]))?;
        match token.token_type {
            RPAREN | LBRACE | SEMI => Ok(()),
            EQUALITY | LESS | LESSEQ | GREATER => {
                self.to_match = None;
                self.parse_addsub()?;
                self.parse_expr2()?;
                Ok(())
            }
            _ => Err(ParseError::new(
                token,
                vec![RPAREN, LBRACE, SEMI, EQUALITY, LESS, LESSEQ, GREATER],
            )),
        }
    }

    fn parse_addsub(&mut self) -> ParseResult<()> {
        self.debug("parse_addsub");
        self.parse_term()?;
        self.parse_addsub2()?;
        Ok(())
    }

    fn parse_addsub2(&mut self) -> ParseResult<()> {
        self.debug("parse_addsub2");
        let token = self.next_token().ok_or_else(|| {
            ParseError::eof(vec![
                RPAREN, LBRACE, SEMI, EQUALITY, LESS, GREATER, PLUS, MINUS, LESSEQ,
            ])
        })?;
        match token.token_type {
            PLUS | MINUS => {
                self.to_match = None;
                self.parse_term()?;
                self.parse_addsub2()?;
                Ok(())
            }
            RPAREN | LBRACE | SEMI | EQUALITY | LESS | GREATER | LESSEQ => Ok(()),
            _ => Err(ParseError::new(
                token,
                vec![RPAREN, LBRACE, SEMI, EQUALITY, LESS, LESSEQ, GREATER, PLUS, MINUS],
            )),
        }
    }

    fn parse_term(&mut self) -> ParseResult<()> {
        self.debug("parse_term");
        self.parse_factor()?;
        self.parse_term2()?;
        Ok(())
    }

    fn parse_term2(&mut self) -> ParseResult<()> {
        self.debug("parse_term2");
        let token = self.next_token().ok_or_else(|| {
            ParseError::eof(vec![
                RPAREN, LBRACE, SEMI, EQUALITY, LESS, GREATER, PLUS, MINUS, MULT, DIV,
            ])
        })?;
        match token.token_type {
            MULT | DIV => {
                self.to_match = None;
                self.parse_factor()?;
                self.parse_term2()?;
                Ok(())
            }
            RPAREN | LBRACE | SEMI | EQUALITY | LESS | LESSEQ | GREATER | PLUS | MINUS => Ok(()),
            _ => Err(ParseError::new(
                token,
                vec![RPAREN, LBRACE, SEMI, EQUALITY, LESS, GREATER, PLUS, MINUS, MULT, DIV],
            )),
        }
    }

    fn parse_factor(&mut self) -> ParseResult<()> {
        self.debug("parse_factor");
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
        self.eat_single(FOR)?;
        self.eat_single(LPAREN)?;
        // Before
        self.parse_assignment()?;
        // Boolean check
        self.parse_expr()?;
        self.eat_single(SEMI)?;
        // Update
        self.eat_single(ID)?;
        self.eat_single(ASSIGN)?;
        self.parse_expr()?;
        // Close
        self.eat_single(RPAREN)?;
        // Statement
        self.parse_statement()?;
        Ok(())
    }

    fn parse_block(&mut self) -> ParseResult<()> {
        self.eat(vec![LBRACE])?;
        while let Some(token) = self.next_token() {
            self.debug("block");
            match token.token_type {
                RBRACE => break,
                _ => self.parse_statement()?,
            };
        }
        if self.to_match.is_none() {
            return Err(ParseError::eof(vec![RBRACE]));
        }
        self.eat(vec![RBRACE])?;
        self.debug("block-complete");
        Ok(())
    }

    /// Parse an assignment or declaration statement
    fn parse_assignment(&mut self) -> ParseResult<()> {
        use TokenType::*;
        match self.next_token().unwrap().token_type {
            INT | FLOAT => {
                self.to_match = None;
                self.eat(vec![TokenType::ID])?;
                if let Some(token) = self.next_token() {
                    match token.token_type {
                        // Assignment
                        ASSIGN => self.eat(vec![ASSIGN])?,
                        SEMI => {
                            // Declaration statement 
                            self.to_match = None;
                            return Ok(());
                        }
                        _ => return Err(ParseError::new(token, vec![ASSIGN, SEMI])),
                    };
                } else {
                    return Err(ParseError::eof(vec![ASSIGN, SEMI]));
                }
            }
            ID => {
                self.to_match = None;
                self.eat(vec![ASSIGN])?;
            }
            a => panic!("{:?}", a),
        };
        self.parse_expr()?;
        self.eat(vec![SEMI])?;
        Ok(())
    }

    /// Parsing a statement of a If, For, Assignment, Block
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
