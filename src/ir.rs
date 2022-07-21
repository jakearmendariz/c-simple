use std::fmt;

use crate::ast::*;
use crate::scanner::{Token, TokenType};
use crate::sized_string::*;
use crate::type_check::SymbolTable;

#[derive(Debug, Clone, Copy)]
struct Vreg(u32);

impl fmt::Display for Vreg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&format!("vr{:?}", self.0))
    }
}

#[derive(Debug, Clone)]
struct Label(u32);

#[derive(Debug, Clone, Copy)]
struct VarName(Option<u32>, SizedString);

impl fmt::Display for VarName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            Some(num) => f.write_str(&format!("prog_var{:?}", num)),
            None => f.write_str(&format!("{}", self.1)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum OpType {
    Plus,
    Minus,
    Multiply,
    Divide,
}

impl OpType {
    fn new(token: Token) -> Self {
        use OpType::*;
        match token.token_type {
            TokenType::PLUS => Plus,
            TokenType::MULT => Multiply,
            TokenType::MINUS => Minus,
            TokenType::DIV => Divide,
            unexpected_token => panic!("Expected Op token, recieved {:?}", unexpected_token),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Op(OpType, VarType);

impl Op {
    fn new(terminal: Terminal) -> Self {
        Op(
            OpType::new(terminal.token),
            terminal.vtype.expect("Operation missing type"),
        )
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&format!("{:?}{:?}", self.0, self.1))
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    BinaryOp(Op, Vreg, Vreg, Vreg),
    Branch(Label),
    // BranchConditonal(Label, CondType, Vreg, Vreg),
    TypeConvert(VarType, Vreg, Vreg),
    /// (Variable Type, Destination Register, String value)
    ToVr(VarType, Vreg, SizedString),
    VregAssignment(Vreg, Vreg),
    BasicAssignment(VarName, Vreg),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Instruction::*;
        match self {
            BinaryOp(op, reg1, reg2, reg3) => {
                f.write_str(&format!("{} = {}({}, {});", reg1, op, reg2, reg3))
            }
            Branch(label) => f.write_str(&format!("{:?}", label)),
            TypeConvert(vtype, dest, arg) => {
                if vtype == &VarType::Float {
                    f.write_str(&format!("{} = int2float({});", dest, arg))
                } else {
                    // Add types
                    f.write_str(&format!("{} = float2int({});", dest, arg))
                }
            }
            ToVr(vtype, dest, arg) => {
                if vtype == &VarType::Float {
                    f.write_str(&format!("{} = float2vr({});", dest, arg))
                } else {
                    // Add types
                    f.write_str(&format!("{} = int2vr({});", dest, arg))
                }
            }
            VregAssignment(dest, arg) => f.write_str(&format!("{} = {};", dest, arg)),
            BasicAssignment(dest, arg) => f.write_str(&format!("{} = {};", dest.1, arg)),
        }
    }
}

#[derive(Debug, Clone, Default)]
struct NameGenerator {
    reg_counter: u32,
    label_counter: u32,
    var_counter: u32,
}

impl NameGenerator {
    fn new_register(&mut self) -> Vreg {
        self.reg_counter += 1;
        Vreg(self.reg_counter)
    }

    fn new_label(&mut self) -> Label {
        self.label_counter += 1;
        Label(self.label_counter)
    }

    fn new_var_name(&mut self, var_name: SizedString) -> VarName {
        self.var_counter += 1;
        VarName(Some(self.var_counter), var_name)
    }
}

pub fn ast_to_instructions(ast: Ast) -> Vec<Instruction> {
    let mut ng = NameGenerator::default();
    let mut st = SymbolTable::<VarName>::default();
    ast_to_instr_helper(ast, &mut ng, &mut st)
}

fn ast_to_instr_helper(
    ast: Ast,
    ng: &mut NameGenerator,
    st: &mut SymbolTable<VarName>,
) -> Vec<Instruction> {
    match ast {
        Ast::Assignment { vtype, id, expr } => {
            match vtype {
                Some(_) => {
                    // First time id is being definfed
                    let vname = ng.new_var_name(id.token.value);
                    st.save_variable(id.to_string(), vname)
                }
                None => (),
            };
            let (mut expr_instrs, resulting_vreg) = expr_to_instructions(*expr, ng, st);
            expr_instrs.push(Instruction::BasicAssignment(
                st.get_variable(&id.to_string()).unwrap(),
                resulting_vreg,
            ));
            expr_instrs
        }
        Ast::Block(statements) => statements
            .iter()
            .flat_map(move |ast| ast_to_instr_helper(ast.to_owned(), ng, st))
            .collect(),
        _ => vec![],
    }
}

fn expr_to_instructions(
    expr: Expr,
    ng: &mut NameGenerator,
    st: &mut SymbolTable<VarName>,
) -> (Vec<Instruction>, Vreg) {
    match expr {
        Expr::Binary(e1, op, e2) => {
            let (inst1, vr1) = expr_to_instructions(*e1, ng, st);
            let (inst2, vr2) = expr_to_instructions(*e2, ng, st);
            let vr3 = ng.new_register();
            let inst3 = Instruction::BinaryOp(Op::new(op), vr3, vr2, vr1);
            ([&inst1[..], &inst2[..], &vec![inst3][..]].concat(), vr3)
        }
        Expr::TypeConvert(vtype, e1) => {
            let (inst1, vr1) = expr_to_instructions(*e1, ng, st);
            let vr2 = ng.new_register();
            let inst2 = Instruction::TypeConvert(vtype, vr2, vr1);
            ([&inst1[..], &vec![inst2][..]].concat(), vr2)
        }
        Expr::Terminal(terminal) => {
            let vr1 = ng.new_register();
            let vtype = terminal
                .vtype
                .expect("Terminal missing type when converting to IR");
            let vname = VarName(terminal.uid, terminal.token.value);
            match terminal.token.token_type {
                TokenType::ID => (vec![], vr1),
                TokenType::INTLIT => (vec![Instruction::ToVr(vtype, vr1, vname.1)], vr1),
                TokenType::FLOATLIT => (vec![Instruction::ToVr(vtype, vr1, vname.1)], vr1),
                _ => panic!("Terminal is not of type ID, INTLIT, FLOATLIT"),
            }
        }
    }
}
