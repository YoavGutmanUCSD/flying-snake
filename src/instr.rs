use dynasmrt::x64::Rq;

#[derive(Clone, Copy, PartialEq, derive_more::Display)]
#[display("{}", self.fmt_loc())]
pub enum Loc {
    Reg(Rq),
    Offset(Rq, i32),
}

#[derive(Clone, Copy, PartialEq, derive_more::Display)]
pub enum Val {
    #[display("{_0}")]
    Place(Loc),
    #[display("{_0}")]
    Imm(i64),
}

#[derive(derive_more::Display)]
pub enum OpCode {
    #[display("mov")]
    Mov,
    #[display("add")]
    Add,
    #[display("sub")]
    Sub,
    #[display("imul")]
    Mul,
    #[display("xor")]
    Xor,
    #[display("cmp")]
    Cmp,
    #[display("test")]
    Test,
    #[display("cmove")]
    CMove,
    #[display("cmovg")]
    CMovg,
    #[display("cmovge")]
    CMovge,
    #[display("cmovl")]
    CMovl,
    #[display("cmovle")]
    CMovle,
    #[display("sal")]
    Lsh,
    #[display("sar")]
    Rsh,
}

#[derive(derive_more::Display)]
pub enum BranchCode {
    #[display("jne")]
    Jne,
    #[display("je")]
    Je,
    #[display("jmp")]
    Jmp,
    #[display("jo")]
    Jo,
}

#[derive(derive_more::Display)]
pub enum JumpDst {
    #[display("{_0}")]
    Label(String),
}

#[derive(derive_more::Display)]
#[display("{}", self.fmt_instr())]
pub enum Instr {
    Label(String),
    Jump(BranchCode, JumpDst),
    TwoArg(OpCode, Loc, Val),
    Neg(Loc),
    Ret,
    MovLabel(Rq, String),
    CallPrint(Rq),
    MovRegImm64(Rq, i64),
    CallRax,
}

const TYPE_ERROR_CODE: i64 = 0b111;
const OVERFLOW_ERROR_CODE: i64 = 0b1111;
const CAST_ERROR_CODE: i64 = 0b10111;

pub const TYPE_ERROR: [Instr; 3] = [
    Instr::TwoArg(
        OpCode::Mov,
        Loc::Reg(Rq::RSP),
        Val::Place(Loc::Reg(Rq::RBP)),
    ),
    Instr::TwoArg(OpCode::Mov, Loc::Reg(Rq::RAX), Val::Imm(TYPE_ERROR_CODE)),
    Instr::Ret,
];

pub const OVERFLOW_ERROR: [Instr; 3] = [
    Instr::TwoArg(
        OpCode::Mov,
        Loc::Reg(Rq::RSP),
        Val::Place(Loc::Reg(Rq::RBP)),
    ),
    Instr::TwoArg(
        OpCode::Mov,
        Loc::Reg(Rq::RAX),
        Val::Imm(OVERFLOW_ERROR_CODE),
    ),
    Instr::Ret,
];
pub const CAST_ERROR: [Instr; 3] = [
    Instr::TwoArg(
        OpCode::Mov,
        Loc::Reg(Rq::RSP),
        Val::Place(Loc::Reg(Rq::RBP)),
    ),
    Instr::TwoArg(OpCode::Mov, Loc::Reg(Rq::RAX), Val::Imm(CAST_ERROR_CODE)),
    Instr::Ret,
];

fn as_string(r: &Rq) -> String {
    match r {
        Rq::RAX => "rax",
        Rq::RCX => "rcx",
        Rq::RDX => "rdx",
        Rq::RBX => "rbx",
        Rq::RSP => "rsp",
        Rq::RBP => "rbp",
        Rq::RSI => "rsi",
        Rq::RDI => "rdi",
        Rq::R8 => "r8",
        Rq::R9 => "r9",
        Rq::R10 => "r10",
        Rq::R11 => "r11",
        Rq::R12 => "r12",
        Rq::R13 => "r13",
        Rq::R14 => "r14",
        Rq::R15 => "r15",
    }
    .to_string()
}

impl Loc {
    fn fmt_loc(&self) -> String {
        match self {
            Loc::Reg(reg) => as_string(reg),
            Loc::Offset(reg, n) => {
                if *n == 0 {
                    format!("[{}]", as_string(reg))
                } else if *n < 0 {
                    format!("[{}{n}]", as_string(reg))
                } else {
                    format!("[{}+{n}]", as_string(reg))
                }
            }
        }
    }
}

impl Instr {
    fn fmt_instr(&self) -> String {
        match self {
            Instr::TwoArg(op, loc, val) => match (loc, val) {
                (Loc::Offset(_, _), _) => format!("{} QWORD {}, {}", op, loc, val),
                (_, Val::Place(Loc::Offset(_, _))) => format!("{} {}, QWORD {}", op, loc, val),
                _ => format!("{} {}, {}", op, loc, val),
            },
            Instr::Label(name) => format!("{name}:"),
            Instr::Jump(branch, name) => format!("{} {}", branch, name),
            Instr::Neg(loc) => format!("neg {}", loc),
            Instr::Ret => "ret".to_string(),
            Instr::MovLabel(reg, label) => format!("mov {}, {}", as_string(reg), label),
            Instr::CallPrint(reg) => format!(
                "mov {}, QWORD snek_print\ncall {}",
                as_string(reg),
                as_string(reg)
            ),
            Instr::MovRegImm64(_, _) | Instr::CallRax => {
                unimplemented!("Only the JIT mode uses this instruction; implementation deferred")
            }
        }
    }
}
