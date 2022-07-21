use crate::ast::*;
use crate::scanner::{Token, TokenType};
use crate::sized_string::SizedString;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub struct SymbolTableData(pub VarType, u32);

/// Keeping track of variable scope
pub struct SymbolTable<T: Copy> {
    pub var_map: HashMap<String, T>,
    pub var_stack: Vec<(String, u32)>,
    pub stack_lvl: u32,
    pub vcounter: u32,
}

impl<T: Copy> Default for SymbolTable<T> {
    fn default() -> SymbolTable<T> {
        SymbolTable {
            var_map: HashMap::new(),
            var_stack: Vec::new(),
            stack_lvl: 0,
            vcounter: 0,
        }
    }
}

impl<T: Copy> SymbolTable<T> {
    // increase stack level
    pub fn push_scope(&mut self) {
        self.stack_lvl += 1;
    }

    /// pops all variables off the stack that are on a lower level of the stack
    pub fn pop_scope(&mut self) {
        self.stack_lvl -= 1;
        // loop, deleting the variables that are out of scope of the current level
        while !self.var_stack.is_empty() {
            let last_val = self.var_stack.last().unwrap();
            if last_val.1 > self.stack_lvl {
                self.var_map.remove(&last_val.0);
                self.var_stack.pop();
            } else {
                break;
            }
        }
    }

    /// save variable to stack
    pub fn save_variable(&mut self, var_name: String, value: T) {
        match self.var_map.get(&var_name) {
            Some(_) => (), // variable was already inserted
            None => self.var_stack.push((var_name.clone(), self.stack_lvl)),
        }
        self.var_map.insert(var_name, value);
    }

    pub fn get_variable(&self, var_name: &str) -> Option<T> {
        match self.var_map.get(var_name) {
            Some(value) => Some(*value),
            None => None,
        }
    }
}

#[derive(Debug)]
pub struct SymbolError {
    line: u32,
    symbol: SizedString,
}

impl SymbolError {
    fn new(line: u32, symbol: SizedString) -> Self {
        Self { line, symbol }
    }
}

use std::fmt;
impl fmt::Display for SymbolError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&format!(
            "Symbol Error on line {},undefined value {}",
            self.line, self.symbol,
        ))
    }
}

impl std::error::Error for SymbolError {
    fn description(&self) -> &str {
        "Type Error"
    }
}

type SymbolResult<T> = Result<T, SymbolError>;

fn get_type(token: Token, memory: &mut SymbolTable<SymbolTableData>) -> SymbolResult<VarType> {
    Ok(match token.token_type {
        TokenType::FLOAT => VarType::Float,
        TokenType::INT => VarType::Int,
        TokenType::INTLIT => VarType::Int,
        TokenType::FLOATLIT => VarType::Float,
        TokenType::ID => match memory.get_variable(&token.to_string()) {
            Some(vtype) => vtype.0,
            None => return Err(SymbolError::new(token.row, token.value)),
        },
        _ => panic!("Parsing error. Type isn't a type"),
    })
}

pub fn type_check(statements: Vec<Ast>) -> SymbolResult<Ast> {
    let mut memory = SymbolTable::<SymbolTableData>::default();
    check_ast(Ast::Block(statements), &mut memory)
    // statements.map(|statement| check_ast(statement))
}

fn check_ast(ast: Ast, memory: &mut SymbolTable<SymbolTableData>) -> SymbolResult<Ast> {
    match ast {
        Ast::Assignment { vtype, id, expr } => {
            if let Some(vtype) = vtype {
                let vtype = match vtype.token_type {
                    TokenType::FLOAT => VarType::Float,
                    TokenType::INT => VarType::Int,
                    _ => panic!("Parsing error. Type isn't a type"),
                };
                let uid = memory.vcounter;
                memory.vcounter += 1;
                let st_data = SymbolTableData(vtype, uid);
                memory.save_variable(id.to_string(), st_data);
                // println!("{}", id.token.to_string());
            }
            // Save the type of ID into AST node
            let id_type = get_type(id.token, memory)?;

            let id_terminal = Terminal {
                token: id.token,
                vtype: Some(id_type),
                uid: None,
            };
            // Build expression
            let (new_expr, expr_type) = type_expr((*expr).clone(), memory)?;
            let expected_vartype = memory.get_variable(&id.to_string());
            match expected_vartype {
                Some(expected_vartype) => {
                    if expr_type != expected_vartype.0 {
                        if expr_type == VarType::Int {
                            Ok(Ast::Assignment {
                                vtype: vtype,
                                id: id_terminal,
                                expr: Box::new(Expr::TypeConvert(VarType::Int, new_expr.into())),
                            })
                        } else {
                            Ok(Ast::Assignment {
                                vtype: vtype,
                                id: id_terminal,
                                expr: Box::new(Expr::TypeConvert(VarType::Float, new_expr.into())),
                            })
                        }
                    } else {
                        Ok(Ast::Assignment {
                            vtype: vtype,
                            id: id_terminal,
                            expr: new_expr.into(),
                        })
                    }
                }
                None => panic!(
                    "Undeclared value \"{}\" on line {}",
                    id.to_string(),
                    id.token.row
                ),
            }
        }
        Ast::If {
            conditional,
            statement,
            else_statement,
        } => {
            let (new_conditional, cond_type) = type_expr((*conditional).clone(), memory)?;
            assert_eq!(
                cond_type,
                VarType::Int,
                "Boolean expression does not have vartype of int"
            );
            // check_ast(statement, memory)?;
            let else_stm = match *else_statement {
                Some(else_statement) => else_statement,
                None => Ast::Skip,
            };
            let new_else = check_ast(else_stm, memory)?;
            let new_statement = check_ast(*statement, memory)?;
            Ok(Ast::If {
                conditional: new_conditional.into(),
                statement: new_statement.into(),
                else_statement: Some(new_else).into(),
            })
        }
        Ast::Block(statements) => {
            let mut new_statements = Vec::with_capacity(statements.len());
            memory.push_scope();
            for statement in statements {
                new_statements.push(check_ast(statement, memory)?);
            }
            memory.pop_scope();
            Ok(Ast::Block(new_statements))
        }
        _ => Ok(ast),
    }
}

/// Converts an expression to the proper type
fn type_expr(
    expr: Expr,
    memory: &mut SymbolTable<SymbolTableData>,
) -> SymbolResult<(Expr, VarType)> {
    match expr {
        Expr::Terminal(terminal) => {
            let mut uid = None;
            let vtype = if TokenType::ID == terminal.token.token_type {
                match memory.get_variable(&terminal.token.to_string()) {
                    Some(vtype) => {
                        uid = Some(vtype.1);
                        vtype.0
                    }
                    None => return Err(SymbolError::new(terminal.token.row, terminal.token.value)),
                }
            } else {
                get_type(terminal.token, memory)?
            };
            let new_terminal = Terminal {
                token: terminal.token,
                vtype: Some(vtype),
                uid,
            };
            Ok((Expr::Terminal(new_terminal), vtype))
        }
        Expr::Binary(e1, op, e2) => {
            let (mut e1, type_e1) = type_expr(*e1, memory)?;
            let (mut e2, type_e2) = type_expr(*e2, memory)?;
            if type_e1 != type_e2 {
                let op_terminal = Terminal {
                    token: op.token,
                    vtype: Some(VarType::Float),
                    uid: None,
                };
                // If not equal, then promote to Float
                if type_e1 == VarType::Int {
                    e1 = Expr::TypeConvert(VarType::Float, Box::new(e1));
                } else {
                    e2 = Expr::TypeConvert(VarType::Float, Box::new(e2));
                }
                Ok((
                    Expr::Binary(e1.into(), op_terminal, e2.into()),
                    VarType::Float,
                ))
            } else {
                let op_terminal = Terminal {
                    token: op.token,
                    vtype: Some(type_e2),
                    uid: None,
                };
                Ok((Expr::Binary(e1.into(), op_terminal, e2.into()), type_e1))
            }
        }
        Expr::TypeConvert(vtype, e) => {
            let (e, e_type) = type_expr(*e, memory)?;
            if e_type == vtype {
                panic!("Trying to convert a type to itself");
            } else {
                Ok((e, e_type))
            }
        }
    }
}
