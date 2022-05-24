use crate::ast::{*};
use crate::sized_string::{*};
use crate::scanner::TokenType;
use type_check::SymbolTable;

type Vreg = u32;
struct Label(u32);
struct VarName(u32, SizedString);

enum Op {
    Plus, 
    Minus,
    Multiply, 
    Divide,
}

impl Op {
    fn new(token: Token) -> Self {
        use Op::{*};
        match token.token_type {
            TokenType::PLUS => Plus,
            TokenType::MULT => Multiply,
            TokenType::MINUS => Minus,
            TokenType::DIV => Divide,
        }
    }
}

enum Instruction {
    BinaryOp(Op, Vreg, Vreg, Vreg),
    Branch(Label),
    BranchConditonal(Vreg, Vreg, Vreg),
    TypeConvert(VarType, Vreg, Vreg),
    ToVr(Vreg, VarName)
}


struct NameGenerator {
    reg_counter: u32,
    label_counter: u32,
    var_counter: u32,
}

impl NameGenerator {
    fn new_register(&mut self) -> Vreg {
        self.reg_counter += 1;
        self.reg_counter
    }

    fn new_label(&mut self) -> Label {
        self.label_counter += 1;
        Label(self.label_counter)
    }
    
    fn new_var_counter(&mut self, var_name: SizedString) -> Label {
        self.var_counter += 1;
        VarName(self.var_counter, var_name)
    }
}

pub fn ast_to_instructions(ast: Ast, &mut ng: NameGenerator, st: &mut SymbolTable) -> Vec<Instruction> {
    match ast {
        Ast::Assignment {vtype, id, expr} => {
            match vtype {

            }
        }
    }
}

fn expr_to_instructions(expr: Expr, &mut ng: NameGenerator, st: &mut SymbolTable) -> (Vec<Instruction>, Vreg) {
    match expr {
        Expr::Binary(e1, op, e2) => {
            let (inst1, vr1) = expr_to_instructions(*e1);
            let (inst2, vr2) = expr_to_instructions(*e2);
            let vr3 = ng.new_register();
            let inst3 = Instruction::BinaryOp(Op::new(op), vr3, vr2, vr1);
            ([&inst1[..], &inst2[..], inst3].concat(), vr3)
        },
        Expr::TypeConvert(vtype, e1) => {
            let (inst1, vr1) = expr_to_instructions(*e1);
            let vr2 = ng.new_register();
            let inst2 = Instruction::TypeConvert(vtype, vr1, vr2);
            ([&inst1[..], &inst2[..]].concat(), vr2)
        },
        Expr::Terminal(token) => {
            let vr1 = ng.new_register();
            match token.token_type {
                TokenType::ID => (vec![Instruction::ToVr(vr1, token.value)], vr1),
                TokenType::INTLIT => token.value,

            }
        }

    }
}
