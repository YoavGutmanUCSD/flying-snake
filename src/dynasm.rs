use crate::instr::{BranchCode, Instr, JumpDst, Loc, OpCode, Val};
// `Rq` macros are provided by dynasm at macro expansion time, so no explicit import is needed.
use dynasmrt::{dynasm, DynamicLabel, DynasmApi, DynasmLabelApi};
use std::collections::HashMap;

fn snek_print(i: i64) -> i64 {
    match i & 7 {
        0b111 => match (i >> 3) & 7 {
            0 => eprintln!("Invalid arguments to one or more functions."),
            1 => eprintln!("Integer overflow."),
            _ => eprintln!("Unknown error"),
        },
        0b011 => println!("true"),
        0b001 => println!("false"),
        _ if i & 1 == 0 => {
            println!("{}", i >> 1)
        }
        _ => eprintln!("Unknown error"),
    }
    i
}

pub fn update_label_table(
    ops: &mut dynasmrt::x64::Assembler,
    is: &[Instr],
    table: &mut HashMap<String, DynamicLabel>,
) {
    for i in is {
        if let Instr::Label(label) = i {
            if table.get(label).is_none() {
                let label_real = ops.new_dynamic_label();
                table.insert(label.to_string(), label_real);
            }
        }
    }
}

macro_rules! shift_to_asm {
    ($ops:expr, $code:ident, $dst:expr, $src:expr) => {
        match $src {
            // it can't be more than i8, but i is i32
            Val::Imm(i) if *i < 128 => match $dst {
                Loc::Reg(reg) => dynasm!($ops ; .arch x64 ; $code Rq(*reg as u8), (*i as i8)),
                Loc::Offset(reg, off) => dynasm!($ops ; .arch x64 ; $code QWORD [Rq(*reg as u8) + *off], (*i as i8)),
            }
            _ => panic!("invalid shift asm")
        }
    }
}
macro_rules! cmov_to_asm {
    ($ops:expr, $code:ident, $dst:expr, $src:expr) => {
        match ($dst, $src) {
            (Loc::Reg(reg1), Val::Place(Loc::Offset(reg2, i))) => dynasm!($ops ; .arch x64 ; $code Rq(*reg1 as u8), QWORD [Rq(*reg2 as u8) + (*i as i32)]),
            (Loc::Reg(reg1), Val::Place(Loc::Reg(reg2))) => dynasm!($ops ; .arch x64 ; $code Rq(*reg1 as u8), Rq(*reg2 as u8)),
            _ => panic!("Invalid cmov instruction, per regulation. What did you write, Yoav?")
        }
    };
}

macro_rules! instr_to_asm {
    // (ops: dynasmrt::x64::Assembler, code: OpCode, dst: Loc, src: Val)
    // small constraint: no mem to mem instructions, no reading [rax], no reading into registers
    // outside of rax
    ($ops:expr, imul, $dst:expr, $src:expr) => {
        match ($dst, $src) {
            (Loc::Reg(reg1), Val::Place(Loc::Offset(reg2, i))) => dynasm!($ops ; .arch x64 ; imul Rq(*reg1 as u8), QWORD [Rq(*reg2 as u8) + *i]),
            (Loc::Reg(reg1), Val::Place(Loc::Reg(reg2))) => dynasm!($ops ; .arch x64 ; imul Rq(*reg1 as u8), Rq(*reg2 as u8)),
            _ => panic!("Invalid imul instruction, per regulation. What did you write, Yoav?")
        }
    };
    ($ops:expr, mov, $dst:expr, $src:expr) => {
        match ($dst, $src) {
            (Loc::Reg(reg1), Val::Place(Loc::Offset(reg2, i))) => dynasm!($ops ; .arch x64 ; mov Rq(*reg1 as u8), QWORD [Rq(*reg2 as u8) + (*i as i32)]),

            (Loc::Reg(reg), Val::Imm(imm)) => dynasm!($ops ; .arch x64 ; mov Rq(*reg as u8), QWORD *imm),

            // NOTE: DO NOT USE RDX FOR ANYTHING ELSE!!!!
            (Loc::Offset(reg, i), Val::Imm(imm)) => dynasm!($ops ; .arch x64
                ; mov rdx, QWORD *imm
                ; mov QWORD [Rq(*reg as u8)+*i], rdx),

            (Loc::Offset(reg1, i), Val::Place(Loc::Reg(reg2))) => dynasm!($ops ; .arch x64 ; mov QWORD [Rq(*reg1 as u8) + *i], Rq(*reg2 as u8)),

            (Loc::Reg(reg1), Val::Place(Loc::Reg(reg2))) => dynasm!($ops ; .arch x64 ; mov Rq(*reg1 as u8), Rq(*reg2 as u8)),
            _ => panic!("Invalid instruction, per regulation. What did you write, Yoav?"),
        }
    };
    ($ops:expr, $code:ident, $dst:expr, $src:expr) => {
        match ($dst, $src) {
            (Loc::Reg(reg1), Val::Place(Loc::Offset(reg2, i))) => dynasm!($ops ; .arch x64 ; $code Rq(*reg1 as u8), QWORD [Rq(*reg2 as u8) + (*i as i32)]),

            // HACK: move into rdx before moving into the memory location. I didn't know 64-bit
            //       immediates were illegal!
            (Loc::Reg(reg), Val::Imm(imm)) if *imm >= 1<<32 => dynasm!($ops ; .arch x64
                ; mov rdx, QWORD *imm
                ; $code Rq(*reg as u8), rdx),

            (Loc::Reg(reg), Val::Imm(imm)) => dynasm!($ops ; .arch x64 ; $code Rq(*reg as u8), *imm as i32),

            (Loc::Offset(reg, i), Val::Imm(imm)) if *imm < 1<<32 => dynasm!($ops ; .arch x64 ; $code QWORD [Rq(*reg as u8)+*i], *imm as i32),

            (Loc::Offset(reg, i), Val::Imm(imm)) => dynasm!($ops ; .arch x64
                ; mov rdx, QWORD *imm
                ; $code QWORD [Rq(*reg as u8) + *i], rdx),

            (Loc::Offset(reg1, i), Val::Place(Loc::Reg(reg2))) => dynasm!($ops ; .arch x64 ; $code QWORD [Rq(*reg1 as u8) + *i], Rq(*reg2 as u8)),
            _ => panic!("Invalid instruction, per regulation. What did you write, Yoav?"),
        }
    };
    // no need for a macro here, but just for completeness sake...
    ($ops:expr, neg, $dst:expr) => {
        match $dst {
            Loc::Reg(reg) => dynasm!($ops ; .arch x64 ; neg Rq(*reg as u8)),
            Loc::Offset(reg, i) => dynasm!($ops ; .arch x64 ; neg QWORD [Rq(*reg as u8) + *i]),
        }
    }
}

fn jmp_to_asm(
    ops: &mut dynasmrt::x64::Assembler,
    branch: &BranchCode,
    dst: &JumpDst,
    label_table: &HashMap<String, DynamicLabel>,
) -> Option<()> {
    let JumpDst::Label(label_name) = dst;
    let label_true = label_table.get(label_name)?;
    match branch {
        BranchCode::Jmp => dynasm!(ops ; .arch x64 ; jmp => *label_true),
        BranchCode::Je => dynasm!(ops ; .arch x64 ; je => *label_true),
        BranchCode::Jne => dynasm!(ops ; .arch x64 ; jne => *label_true),
        BranchCode::Jo => dynasm!(ops ; .arch x64 ; jo => *label_true),
    }
    Some(())
}

pub fn instr_to_asm(
    ops: &mut dynasmrt::x64::Assembler,
    i: &Instr,
    label_table: &HashMap<String, DynamicLabel>,
) -> Option<()> {
    match i {
        Instr::Label(label) => {
            if let Some(label_real) = label_table.get(label) {
                dynasm!(ops ; .arch x64 ; =>*label_real);
            } else {
                return None;
            }
        }
        Instr::Jump(branch, dst) => {
            // this is the end of execution if it gets here
            return jmp_to_asm(ops, branch, dst, label_table);
        }
        _a @ Instr::TwoArg(op, dst, src) => match op {
            OpCode::IMov => {
                // println!("running: {}", a.to_string());
                instr_to_asm!(ops, mov, dst, src)
            }
            OpCode::IAdd => instr_to_asm!(ops, add, dst, src),
            OpCode::ISub => instr_to_asm!(ops, sub, dst, src),
            OpCode::IMul => instr_to_asm!(ops, imul, dst, src),
            OpCode::IXor => instr_to_asm!(ops, xor, dst, src),
            OpCode::ICmp => instr_to_asm!(ops, cmp, dst, src),
            OpCode::ITest => instr_to_asm!(ops, test, dst, src),
            OpCode::ICMove => cmov_to_asm!(ops, cmove, dst, src),
            OpCode::ICMovg => cmov_to_asm!(ops, cmovg, dst, src),
            OpCode::ICMovge => cmov_to_asm!(ops, cmovge, dst, src),
            OpCode::ICMovl => cmov_to_asm!(ops, cmovl, dst, src),
            OpCode::ICMovle => cmov_to_asm!(ops, cmovle, dst, src),
            OpCode::ILsh => shift_to_asm!(ops, shl, dst, src),
            OpCode::IRsh => shift_to_asm!(ops, shr, dst, src),
        },
        Instr::Neg(dst) => instr_to_asm!(ops, neg, dst),
        Instr::Ret => dynasm!(ops ; .arch x64 ; ret),
        Instr::MovLabel(reg, label) => {
            if let Some(label_real) = label_table.get(label) {
                dynasm!(ops ; .arch x64 ; lea Rq(*reg as u8), [=>*label_real]);
            } else {
                return None;
            }
        }
        Instr::CallPrint(reg) => {
            #[allow(clippy::fn_to_numeric_cast)]
            {
                dynasm!(ops ; .arch x64
                    ; mov Rq(*reg as u8), QWORD snek_print as _
                    ; call Rq(*reg as u8))
            }
        }
    }
    Some(())
}

pub fn i2a_slice(
    ops: &mut dynasmrt::x64::Assembler,
    instrs: &[Instr],
    label_map: &HashMap<String, DynamicLabel>,
) -> Option<()> {
    for i in instrs {
        if instr_to_asm(ops, i, label_map).is_none() {
            println!("[ERR] we got it right here: {}", i.to_string())
        }
    }
    Some(())
}
