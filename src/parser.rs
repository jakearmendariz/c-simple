use crate::ast::*;
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

fn expect_statement() -> ParseError {
    ParseError::eof(vec![IF, FOR, INT, FLOAT, ID])
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

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.debug("parse_expr");
        let expr = self.parse_factor()?;
        let result = self.parse_expr2(expr)?;
        Ok(result)
    }

    fn parse_expr2(&mut self, lhs: Expr) -> ParseResult<Expr> {
        self.debug("parse_expr2");
        let token = self.next_token().ok_or_else(|| {
            ParseError::eof(vec![RPAREN, LBRACE, SEMI, EQUALITY, LESS, GREATER, LESSEQ])
        })?;
        match token.token_type {
            RPAREN | LBRACE | SEMI => Ok(lhs),
            EQUALITY | LESS | LESSEQ | GREATER => {
                self.to_match = None;
                let rhs = self.parse_factor()?;
                let expr = Expr::Binary(lhs.into(), token.into(), rhs.into());
                self.parse_expr2(expr)
            }
            _ => Err(ParseError::new(
                token,
                vec![RPAREN, LBRACE, SEMI, EQUALITY, LESS, LESSEQ, GREATER],
            )),
        }
    }

    fn parse_factor(&mut self) -> ParseResult<Expr> {
        self.debug("parse_factor");
        let expr = self.parse_term()?;
        self.parse_factor2(expr)
    }

    fn parse_factor2(&mut self, lhs: Expr) -> ParseResult<Expr> {
        self.debug("parse_factor2");
        let token = self.next_token().ok_or_else(|| {
            ParseError::eof(vec![
                RPAREN, LBRACE, SEMI, EQUALITY, LESS, GREATER, PLUS, MINUS, LESSEQ,
            ])
        })?;
        match token.token_type {
            PLUS | MINUS => {
                self.to_match = None;
                let rhs = self.parse_term()?;
                let expr = Expr::Binary(lhs.into(), token.into(), rhs.into());
                self.parse_factor2(expr)
            }
            RPAREN | LBRACE | SEMI | EQUALITY | LESS | GREATER | LESSEQ => Ok(lhs),
            _ => Err(ParseError::new(
                token,
                vec![
                    RPAREN, LBRACE, SEMI, EQUALITY, LESS, LESSEQ, GREATER, PLUS, MINUS,
                ],
            )),
        }
    }

    fn parse_term(&mut self) -> ParseResult<Expr> {
        self.debug("parse_term");
        let expr = self.parse_unit()?;
        self.parse_term2(expr)
    }

    fn parse_term2(&mut self, lhs: Expr) -> ParseResult<Expr> {
        self.debug("parse_term2");
        let token = self.next_token().ok_or_else(|| {
            ParseError::eof(vec![
                RPAREN, LBRACE, SEMI, EQUALITY, LESS, GREATER, PLUS, MINUS, MULT, DIV,
            ])
        })?;
        match token.token_type {
            MULT | DIV => {
                self.to_match = None;
                let rhs = self.parse_unit()?;
                let expr = Expr::Binary(lhs.into(), token.into(), rhs.into());
                self.parse_term2(expr)
            }
            RPAREN | LBRACE | SEMI | EQUALITY | LESS | LESSEQ | GREATER | PLUS | MINUS => Ok(lhs),
            _ => Err(ParseError::new(
                token,
                vec![
                    RPAREN, LBRACE, SEMI, EQUALITY, LESS, GREATER, PLUS, MINUS, MULT, DIV,
                ],
            )),
        }
    }

    /// Parse the ID, literals, or ()
    fn parse_unit(&mut self) -> ParseResult<Expr> {
        self.debug("parse_unit");
        let token = self.eat(vec![LPAREN, ID, INTLIT, FLOATLIT])?;
        match token.token_type {
            LPAREN => {
                let expr = self.parse_expr()?;
                self.eat(vec![RPAREN])?;
                Ok(expr)
            }
            _ => Ok(Expr::Terminal(token.into())),
        }
    }

    /// Parses `if expr statement (else statement)?`
    fn parse_if(&mut self) -> ParseResult<Ast> {
        self.eat(vec![TokenType::IF])?;
        self.to_match = None;
        let conditional = self.parse_expr()?;
        let statement = self.parse_statement()?.ok_or_else(expect_statement)?;
        let mut else_statement = None;
        if let Some(token) = self.next_token() {
            if token.token_type == TokenType::ELSE {
                self.eat(vec![ELSE])?;
                else_statement = self.parse_statement()?;
            }
        }
        Ok(Ast::If {
            conditional: conditional.into(),
            statement: statement.into(),
            else_statement: else_statement.into(),
        })
    }

    fn parse_for(&mut self) -> ParseResult<Ast> {
        self.eat_single(FOR)?;
        self.eat_single(LPAREN)?;
        // Before
        let initial = self.parse_assignment()?;
        // Boolean check
        let conditional = self.parse_expr()?;
        self.eat_single(SEMI)?;
        // Update
        let id = self.eat_single(ID)?;
        self.eat_single(ASSIGN)?;
        let expr = self.parse_expr()?;
        let update = Ast::Assignment {
            vtype: None,
            id: id.into(),
            expr: expr.into(),
        };
        // Close
        self.eat_single(RPAREN)?;
        // Statement
        let body = self.parse_statement()?.ok_or_else(expect_statement)?;
        Ok(Ast::For {
            initial: initial.into(),
            conditional: conditional.into(),
            update: update.into(),
            body: body.into(),
        })
    }

    fn parse_block(&mut self) -> ParseResult<Ast> {
        self.eat(vec![LBRACE])?;
        let mut statements = Vec::new();
        while let Some(token) = self.next_token() {
            self.debug("block");
            match token.token_type {
                RBRACE => break,
                _ => {
                    let statement = self.parse_statement()?.ok_or_else(expect_statement)?;
                    statements.push(statement)
                }
            };
        }
        if self.to_match.is_none() {
            return Err(ParseError::eof(vec![RBRACE]));
        }
        self.eat(vec![RBRACE])?;
        self.debug("block-complete");
        Ok(Ast::Block(statements))
    }

    /// Parse an assignment or declaration statement
    fn parse_assignment(&mut self) -> ParseResult<Ast> {
        use TokenType::*;
        let id: Token;
        let vtype: Option<Token>;
        match self.next_token().unwrap().token_type {
            INT | FLOAT => {
                vtype = self.to_match;
                // println!("vtype: {:?}", vtype);
                self.to_match = None;
                id = self.eat(vec![TokenType::ID])?;
                if let Some(token) = self.next_token() {
                    match token.token_type {
                        // Assignment
                        ASSIGN => self.eat(vec![ASSIGN])?,
                        SEMI => {
                            // Declaration statement
                            self.to_match = None;
                            return Ok(Ast::Declaration {
                                vtype: vtype.unwrap(),
                                id: id.into(),
                            });
                        }
                        _ => return Err(ParseError::new(token, vec![ASSIGN, SEMI])),
                    };
                } else {
                    return Err(ParseError::eof(vec![ASSIGN, SEMI]));
                }
            }
            ID => {
                id = self.to_match.unwrap();
                vtype = None;
                self.to_match = None;
                self.eat(vec![ASSIGN])?;
            }
            a => panic!("{:?}", a),
        };
        let expr = self.parse_expr()?;
        self.eat(vec![SEMI])?;
        Ok(Ast::Assignment {
            vtype,
            id: id.into(),
            expr: expr.into(),
        })
    }

    /// Parsing a statement of a If, For, Assignment, Block
    fn parse_statement(&mut self) -> ParseResult<Option<Ast>> {
        match self.next_token() {
            Some(token) => {
                use TokenType::*;
                Ok(Some(match token.token_type {
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
                }))
            }
            None => Ok(None),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Ast>, ParseError> {
        let mut statements = Vec::new();
        while let Some(stm) = self.parse_statement()? {
            statements.push(stm);
        }
        Ok(statements)
    }
}
