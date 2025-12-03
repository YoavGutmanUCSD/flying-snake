use dynasmrt::x64::Rq;

// Instrs
#[derive(Clone, Copy, PartialEq)]
pub enum Loc {
    Reg(Rq),
    Offset(Rq, i32),
}

#[derive(Clone, Copy, PartialEq)]
pub enum Val {
    Place(Loc),
    Imm(i64),
}

// i want to remove the I later
#[derive(Clone, Copy)]
pub enum OpCode {
    IMov,
    IAdd,
    ISub,
    IMul,
    IXor,
    ICmp,
    ITest,
    ICMove,
    ICMovg,
    ICMovge,
    ICMovl,
    ICMovle,
    ILsh,
    IRsh,
}

#[derive(Clone, Copy)]
pub enum BranchCode {
    Jne,
    Je,
    Jmp,
    Jo,
}

pub enum JumpDst {
    Label(String),
    Pointer(Rq),
}

pub enum Instr {
    Label(String),
    Jump(BranchCode, JumpDst),
    TwoArg(OpCode, Loc, Val),
    Neg(Loc),
    Ret,
    MovLabel(Rq, String),
    CallPrint(Rq),
}

const TYPE_ERROR_CODE: i64 = 0b111;
const OVERFLOW_ERROR_CODE: i64 = 0b1111;
const CAST_ERROR_CODE: i64 = 0b10111;

pub const TYPE_ERROR: [Instr; 3] = [
    Instr::TwoArg(
        OpCode::IMov,
        Loc::Reg(Rq::RSP),
        Val::Place(Loc::Reg(Rq::RBP)),
    ),
    Instr::TwoArg(OpCode::IMov, Loc::Reg(Rq::RAX), Val::Imm(TYPE_ERROR_CODE)),
    Instr::Ret,
];

pub const OVERFLOW_ERROR: [Instr; 3] = [
    Instr::TwoArg(
        OpCode::IMov,
        Loc::Reg(Rq::RSP),
        Val::Place(Loc::Reg(Rq::RBP)),
    ),
    Instr::TwoArg(
        OpCode::IMov,
        Loc::Reg(Rq::RAX),
        Val::Imm(OVERFLOW_ERROR_CODE),
    ),
    Instr::Ret,
];
pub const CAST_ERROR: [Instr; 3] = [
    Instr::TwoArg(
        OpCode::IMov,
        Loc::Reg(Rq::RSP),
        Val::Place(Loc::Reg(Rq::RBP)),
    ),
    Instr::TwoArg(OpCode::IMov, Loc::Reg(Rq::RAX), Val::Imm(CAST_ERROR_CODE)),
    Instr::Ret,
];

impl ToString for JumpDst {
    fn to_string(self: &Self) -> String {
        match self {
            JumpDst::Label(name) => name.to_string(),
            JumpDst::Pointer(reg) => as_string(reg),
        }
    }
}

impl ToString for BranchCode {
    fn to_string(self: &Self) -> String {
        match self {
            BranchCode::Jne => "jne",
            BranchCode::Je => "je",
            BranchCode::Jmp => "jmp",
            BranchCode::Jo => "jo",
        }
        .to_string() // OH MY GOD
    }
}

impl ToString for OpCode {
    fn to_string(self: &Self) -> String {
        match self {
            OpCode::IMov => "mov",
            OpCode::IAdd => "add",
            OpCode::ISub => "sub",
            OpCode::IMul => "imul",
            OpCode::IXor => "xor",
            OpCode::ICmp => "cmp",
            OpCode::ITest => "test",
            OpCode::ICMove => "cmove",
            OpCode::ICMovg => "cmovg",
            OpCode::ICMovge => "cmovge",
            OpCode::ICMovl => "cmovl",
            OpCode::ICMovle => "cmovle",
            OpCode::ILsh => "sal",
            OpCode::IRsh => "sar",
        }
        .to_string()
    }
}

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

impl ToString for Loc {
    fn to_string(self: &Self) -> String {
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

impl ToString for Val {
    fn to_string(self: &Self) -> String {
        match self {
            Val::Imm(n) => n.to_string(),
            Val::Place(loc) => loc.to_string(),
        }
    }
}

impl ToString for Instr {
    fn to_string(self: &Self) -> String {
        match self {
            Instr::TwoArg(op, loc, val) => match (loc, val) {
                (Loc::Offset(_, _), _) => format!(
                    "{} QWORD {}, {}",
                    op.to_string(),
                    loc.to_string(),
                    val.to_string()
                ),
                (_, Val::Place(Loc::Offset(_, _))) => format!(
                    "{} {}, QWORD {}",
                    op.to_string(),
                    loc.to_string(),
                    val.to_string()
                ),
                _ => format!(
                    "{} {}, {}",
                    op.to_string(),
                    loc.to_string(),
                    val.to_string()
                ),
            },
            Instr::Label(name) => format!("{name}:"),
            Instr::Jump(branch, name) => format!("{} {}", branch.to_string(), name.to_string()),
            Instr::Neg(loc) => format!("neg {}", loc.to_string()), // special case for now
            Instr::Ret => "ret".to_string(),
            Instr::MovLabel(reg, label) => format!("mov {}, {}", as_string(reg), label),
            Instr::CallPrint(reg) => format!(
                "mov {}, QWORD snek_print\ncall {}",
                as_string(reg),
                as_string(reg)
            ),
        }
    }
}

pub fn instrs_to_str(is: &Vec<Instr>) -> String {
    is.iter()
        .map(|i| i.to_string())
        .collect::<Vec<String>>()
        .join("\n")
}
// END instrs
