use crate::scanner::Token;
use std::fmt;

#[derive(Debug)]
pub enum Ast {
    Assignment {
        vtype: Option<Token>,
        id: Token,
        expr: Box<Expr>,
    },
    Declaration {
        vtype: Token,
        id: Token,
    },
    If {
        conditional: Box<Expr>,
        statement: Box<Ast>,
        else_statement: Box<Option<Ast>>,
    },
    For {
        initial: Box<Ast>,
        conditional: Box<Expr>,
        update: Box<Ast>,
        body: Box<Ast>,
    },
    Block(Vec<Ast>),
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&match self {
            Ast::Assignment { vtype, id, expr } => match vtype {
                Some(vartype) => format!("{} {} = {};", vartype, id, expr),
                None => format!("{} = {};", id, expr),
            },
            Ast::Declaration { vtype, id } => format!("{} {};", vtype, id),
            Ast::If {
                conditional,
                statement,
                else_statement,
            } => match &**else_statement {
                Some(else_statement) => format!(
                    "if {}\n{}\nelse\n{}",
                    conditional, statement, else_statement
                ),
                None => format!("if {}\n{}", conditional, statement),
            },
            Ast::For {
                initial,
                conditional,
                update,
                body,
            } => format!("for ({} {}; {})\n{}", initial, conditional, update, body),
            Ast::Block(body) => {
                let mut result = String::new();
                for stm in body {
                    result.push_str(&format!("\t{}\n", stm));
                }
                //  format!("{{\n{}\n}}", body)
                result
            }
        })
    }
}

pub enum _VarType {
    Float,
    Int,
}

#[derive(Debug)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    // Unary(Token, Box<Expr>),
    Terminal(Token),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&match self {
            Expr::Binary(e1, t, e2) => format!("({} {} {})", *e1, t, *e2),
            Expr::Terminal(t) => format!("{}", t),
        })
    }
}
