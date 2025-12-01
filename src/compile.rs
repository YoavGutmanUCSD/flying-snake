use crate::ast::{Expr, Op1, Op2};
use crate::context::CompilerContext;
use crate::errors::CompileError;
use crate::instr::{BranchCode, Instr, JumpDst, Loc, OpCode, Val};
use crate::types::Type;
use dynasmrt::x64::Rq::*;

// this can be used later to only typecheck what hasn't been typechecked already
fn type_check_loc(loc: Loc, err_label: String, jump_cond: BranchCode) -> Vec<Instr> {
    vec![
        Instr::TwoArg(OpCode::ITest, loc, Val::Imm(1)),
        Instr::Jump(jump_cond, JumpDst::Label(err_label.clone())),
    ]
}

pub fn compile_to_instrs(
    e: &Expr,
    mut context: CompilerContext,
    mut base: Vec<Instr>,
) -> Result<Vec<Instr>, CompileError> {
    match e {
        Expr::Number(i) => {
            base.push(Instr::TwoArg(
                OpCode::IMov,
                Loc::Reg(RAX),
                Val::Imm((*i << 1) as i64),
            ));
            return Ok(base);
        }
        Expr::Boolean(b) => {
            if *b {
                base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(3)));
            } else {
                base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(1)));
            }
            return Ok(base);
        }
        Expr::Id(name) => {
            if context.shared.fn_name != None && name == "input" {
                return Err(CompileError::IllegalInput(name.clone()));
            };
            match context.value_map.get(name) {
                Some(val) => base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), *val)),
                None => return Err(CompileError::UnboundIdentifier(name.to_string())),
            }
            return Ok(base);
        }
        Expr::UnOp(op, e1) => {
            let type_err_label = context.shared.type_err_label.clone();
            let overflow_err_label = context.shared.overflow_err_label.clone();
            let mut base = compile_to_instrs(&e1, context, base)?;
            match op {
                // only do these if equal
                Op1::Add1 => {
                    let typecheck =
                        type_check_loc(Loc::Reg(RAX), type_err_label.clone(), BranchCode::Jne);
                    base.extend(typecheck);
                    base.push(Instr::TwoArg(OpCode::IAdd, Loc::Reg(RAX), Val::Imm(2)));
                    base.push(Instr::Jump(
                        BranchCode::Jo,
                        JumpDst::Label(overflow_err_label.clone()),
                    ));
                }
                Op1::Sub1 => {
                    let typecheck =
                        type_check_loc(Loc::Reg(RAX), type_err_label.clone(), BranchCode::Jne);
                    base.extend(typecheck);

                    // if good
                    base.push(Instr::TwoArg(OpCode::ISub, Loc::Reg(RAX), Val::Imm(2)));
                    base.push(Instr::Jump(
                        BranchCode::Jo,
                        JumpDst::Label(overflow_err_label.clone()),
                    ));
                }

                // only do these if not equal
                Op1::IsNum => {
                    base.push(Instr::TwoArg(OpCode::ITest, Loc::Reg(RAX), Val::Imm(1)));

                    // default false
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(1)));

                    // if eq then true
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RCX), Val::Imm(3)));
                    base.push(Instr::TwoArg(
                        OpCode::ICMove,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Reg(RCX)),
                    ));
                }
                Op1::IsBool => {
                    base.push(Instr::TwoArg(OpCode::ITest, Loc::Reg(RAX), Val::Imm(1)));

                    // default true
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(3)));

                    // if eq then false
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RCX), Val::Imm(1)));
                    base.push(Instr::TwoArg(
                        OpCode::ICMove,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Reg(RCX)),
                    ));
                }
            }
            // hoping for tail call optimization here
            return Ok(base);
        }
        Expr::BinOp(op, e1, e2) => {
            // 1. compile e1
            let e1_instr: Vec<Instr> = compile_to_instrs(&e1, context.clone(), Vec::new())?;
            // 2. compile e2
            let e2_context = CompilerContext {
                si: context.si - 8,
                ..context.clone()
            };
            let e2_instr: Vec<Instr> = compile_to_instrs(&e2, e2_context, Vec::new())?;
            // 3. insert e1 instructions
            base.extend(e1_instr);
            // 4. mov e1 result (rax) to stack ptr
            base.push(Instr::TwoArg(
                OpCode::IMov,
                Loc::Offset(RSP, context.si),
                Val::Place(Loc::Reg(RAX)),
            ));
            // 5. insert e2 instructions
            base.extend(e2_instr);
            // 6. insert type check code between Rax and wherever e2 is
            let overflow_err_label = context.shared.overflow_err_label.clone();
            let type_err_label = context.shared.type_err_label.clone();
            if let Op2::Equal = op {
                // briefly use context.si-8 so that xor doesn't destroy rax's value
                base.push(Instr::TwoArg(
                    OpCode::IMov,
                    Loc::Offset(RSP, context.si - 8),
                    Val::Place(Loc::Reg(RAX)),
                ));
                base.push(Instr::TwoArg(
                    OpCode::IXor,
                    Loc::Reg(RAX),
                    Val::Place(Loc::Offset(RSP, context.si)),
                ));
                base.push(Instr::TwoArg(OpCode::ITest, Loc::Reg(RAX), Val::Imm(1)));
                base.push(Instr::Jump(BranchCode::Jne, JumpDst::Label(type_err_label)));
                base.push(Instr::TwoArg(
                    OpCode::IMov,
                    Loc::Reg(RAX),
                    Val::Place(Loc::Offset(RSP, context.si - 8)),
                ));
            } else {
                // jump if bool, so if test doesn't create a 0
                base.extend(type_check_loc(
                    Loc::Reg(RAX),
                    type_err_label.clone(),
                    BranchCode::Jne,
                ));
                base.extend(type_check_loc(
                    Loc::Offset(RSP, context.si),
                    type_err_label,
                    BranchCode::Jne,
                ));
            }
            // note: i think arith_generic and other similar functions are unnecessary
            match op {
                Op2::Plus => {
                    // 7. insert plus operation
                    // add rax [ptr]
                    base.push(Instr::TwoArg(
                        OpCode::IAdd,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Offset(RSP, context.si)),
                    ));
                    base.push(Instr::Jump(
                        BranchCode::Jo,
                        JumpDst::Label(overflow_err_label.clone()),
                    ));
                }
                Op2::Minus => {
                    // 7. insert minus operation
                    // sub rax [ptr]
                    base.push(Instr::TwoArg(
                        OpCode::ISub,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Offset(RSP, context.si)),
                    ));
                    base.push(Instr::Jump(
                        BranchCode::Jo,
                        JumpDst::Label(overflow_err_label.clone()),
                    ));

                    // neg rax
                    base.push(Instr::Neg(Loc::Reg(RAX)));
                }
                Op2::Times => {
                    // 7. insert mul operation

                    // rsh rax and other location by 1
                    base.push(Instr::TwoArg(OpCode::IRsh, Loc::Reg(RAX), Val::Imm(1)));
                    base.push(Instr::TwoArg(
                        OpCode::IRsh,
                        Loc::Offset(RSP, context.si),
                        Val::Imm(1),
                    ));

                    // then mul
                    base.push(Instr::TwoArg(
                        OpCode::IMul,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Offset(RSP, context.si)),
                    ));
                    base.push(Instr::Jump(
                        BranchCode::Jo,
                        JumpDst::Label(overflow_err_label.clone()),
                    ));

                    // then lsh by 1
                    base.push(Instr::TwoArg(OpCode::ILsh, Loc::Reg(RAX), Val::Imm(1)));
                }
                Op2::Equal => {
                    // 7. insert equal operation
                    // cmp [ptr] and rax (so, e1 and e2)
                    base.push(Instr::TwoArg(
                        OpCode::ICmp,
                        Loc::Offset(RSP, context.si),
                        Val::Place(Loc::Reg(RAX)),
                    ));
                    // then mov 1
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(1)));
                    // then cmove 3
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RCX), Val::Imm(3)));
                    base.push(Instr::TwoArg(
                        OpCode::ICMove,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Reg(RCX)),
                    ));
                }
                Op2::Greater => {
                    // 7. insert greater than operation
                    // cmp [ptr] and rax (so, e1 and e2)
                    base.push(Instr::TwoArg(
                        OpCode::ICmp,
                        Loc::Offset(RSP, context.si),
                        Val::Place(Loc::Reg(RAX)),
                    ));
                    // then mov 1
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(1)));
                    // then cmovg 3
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RCX), Val::Imm(3)));
                    base.push(Instr::TwoArg(
                        OpCode::ICMovg,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Reg(RCX)),
                    ));
                }
                Op2::GreaterEqual => {
                    // 7. insert greater than or equal to operation
                    // cmp [ptr] and rax (so, e1 and e2)
                    base.push(Instr::TwoArg(
                        OpCode::ICmp,
                        Loc::Offset(RSP, context.si),
                        Val::Place(Loc::Reg(RAX)),
                    ));
                    // then mov 1
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(1)));
                    // then cmovge 3
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RCX), Val::Imm(3)));
                    base.push(Instr::TwoArg(
                        OpCode::ICMovge,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Reg(RCX)),
                    ));
                }
                Op2::Less => {
                    // 7. insert less than operation
                    // cmp [ptr] and rax (so, e1 and e2)
                    base.push(Instr::TwoArg(
                        OpCode::ICmp,
                        Loc::Offset(RSP, context.si),
                        Val::Place(Loc::Reg(RAX)),
                    ));
                    // then mov 1
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(1)));
                    // then cmovl 3
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RCX), Val::Imm(3)));
                    base.push(Instr::TwoArg(
                        OpCode::ICMovl,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Reg(RCX)),
                    ));
                }
                Op2::LessEqual => {
                    // 7. insert less than or equal to operation
                    // cmp [ptr] and rax (so, e1 and e2)
                    base.push(Instr::TwoArg(
                        OpCode::ICmp,
                        Loc::Offset(RSP, context.si),
                        Val::Place(Loc::Reg(RAX)),
                    ));
                    // then mov 1
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(1)));
                    // then cmovle 3
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RCX), Val::Imm(3)));
                    base.push(Instr::TwoArg(
                        OpCode::ICMovle,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Reg(RCX)),
                    ));
                }
            }
            // 9. return vector
            Ok(base)
        }
        Expr::If(econd, e1, e2) => {
            // 1. compile econd
            let econd_instr: Vec<Instr> = compile_to_instrs(&econd, context.clone(), Vec::new())?;

            // 2. compile e1
            let e1_instr: Vec<Instr> = compile_to_instrs(&e1, context.clone(), Vec::new())?;

            // 3. compile e2
            let e2_context = CompilerContext {
                si: context.si - 8,
                ..context.clone()
            };
            let e2_instr: Vec<Instr> = compile_to_instrs(&e2, e2_context, Vec::new())?;

            // labels for later
            let label_ind = context.shared.label_gen.get();
            let else_label = format!("_if_else_{}", label_ind);
            let end_label = format!("_if_end_{}", label_ind);

            // 4. insert econd instructions
            base.extend(econd_instr);

            // secret check: is this even a bool?
            base.extend(type_check_loc(
                Loc::Reg(RAX),
                context.shared.type_err_label.clone(),
                BranchCode::Je,
            ));

            // 5. cmp rax, 3
            base.push(Instr::TwoArg(OpCode::ICmp, Loc::Reg(RAX), Val::Imm(3)));
            // 6. if not equal, jump to else label
            base.push(Instr::Jump(
                BranchCode::Jne,
                JumpDst::Label(else_label.clone()),
            ));
            // 7. insert e1 instructions
            base.extend(e1_instr);
            // 8. jump to end label
            base.push(Instr::Jump(
                BranchCode::Jmp,
                JumpDst::Label(end_label.clone()),
            ));
            // 9. else label
            base.push(Instr::Label(else_label));
            // 10. insert e2 instructions
            base.extend(e2_instr);
            // 11. end label
            base.push(Instr::Label(end_label));

            Ok(base)
        }
        Expr::Let(bindings, e) => {
            let mut binding_instrs: Vec<Instr> = Vec::new();
            let mut seen_binds: std::collections::HashSet<String> =
                std::collections::HashSet::new();
            for (id, id_e) in bindings {
                if context.shared.keywords.contains(id) {
                    return Err(CompileError::SetKeyword);
                }
                if seen_binds.contains(id) {
                    return Err(CompileError::DuplicateBinding);
                }
                binding_instrs = compile_to_instrs(id_e, context.clone(), binding_instrs)?;
                binding_instrs.push(Instr::TwoArg(
                    OpCode::IMov,
                    Loc::Offset(RSP, context.si),
                    Val::Place(Loc::Reg(RAX)),
                ));
                context.value_map = context
                    .value_map
                    .update(id.to_string(), Val::Place(Loc::Offset(RSP, context.si)));
                seen_binds.insert(id.to_string());
                context.si -= 8;
            }
            base.extend(binding_instrs);
            return compile_to_instrs(e, context, base);
        }

        /* for loop and break, add new map to function to keep track of loop breakout label
         * maybe curr_loop: Option<String>
         * when break:
         *   if None, CompileError
         *   if Some(name), jmp name
         */
        // Loop(Box<Expr>),
        Expr::Loop(e) => {
            let loop_label_ind = context.shared.label_gen.get();
            let loop_start = format!("_loop_start_{}", loop_label_ind);
            let loop_end = format!("_loop_end_{}", loop_label_ind);
            let next_context = CompilerContext {
                enclosing_loop_label: Some(loop_end.clone()),
                ..context
            };
            let e_instrs = compile_to_instrs(e, next_context, Vec::new())?;

            // start label,
            base.push(Instr::Label(loop_start.clone()));

            // instructions for e go here!
            base.extend(e_instrs);

            // go back to start
            base.push(Instr::Jump(
                BranchCode::Jmp,
                JumpDst::Label(loop_start.clone()),
            ));

            // end label
            base.push(Instr::Label(loop_end.clone()));

            Ok(base)
        }
        // Break(Box<Expr>),
        Expr::Break(e) => {
            if let Some(label) = context.enclosing_loop_label {
                // pop the label here. don't want to break inside of a break, that'd be weird.
                let e_context = CompilerContext {
                    enclosing_loop_label: None,
                    ..context
                };
                let e_instrs = compile_to_instrs(e, e_context, Vec::new())?;
                base.extend(e_instrs);
                base.push(Instr::Jump(BranchCode::Jmp, JumpDst::Label(label)));

                Ok(base)
            } else {
                Err(CompileError::NoBreakTarget)
            }
        }
        /* for set: should work no matter what, so long as you put RdiOffset in location map for
         * the repl
         */
        // Set(String, Box<Expr>),
        Expr::Set(id, e) => {
            if context.shared.keywords.contains(id) {
                return Err(CompileError::SetKeyword);
            }
            match context.value_map.get(id) {
                Some(Val::Place(loc)) => {
                    // why do i have to clone context here?!
                    let e_instrs = compile_to_instrs(e, context.clone(), Vec::new())?;

                    base.extend(e_instrs);
                    base.push(Instr::TwoArg(
                        OpCode::IMov,
                        loc.clone(),
                        Val::Place(Loc::Reg(RAX)),
                    ));

                    Ok(base)
                }
                Some(Val::Imm(_)) => Err(CompileError::Other(
                    "Tried to set a value that was inlined as a constant\
                            ! This shouldn't happen. Mods, ruin this man's grade."
                        .to_string(),
                )),
                None => Err(CompileError::SetUnboundVariable),
            }
        }

        /* for block: compile and insert everything in order. not much intellect required.
         */
        // Block(Vec<Expr>),
        Expr::Block(es) => {
            for e in es.iter() {
                let e_instrs = compile_to_instrs(e, context.clone(), Vec::new())?;
                base.extend(e_instrs);
            }
            Ok(base)
        }

        // feel free to destroy RAX for this
        Expr::Call(name, args) => {
            match context.shared.function_definitions.get(name) {
                None => return Err(CompileError::FnUnbound(name.to_string())),
                Some((arg_names, _)) => {
                    let args_len = args.len() as i32;
                    let arg_names_len = arg_names.len() as i32;
                    if args_len != arg_names_len {
                        return Err(CompileError::FnBadArgs(
                            name.to_string(),
                            arg_names_len,
                            args_len,
                        ));
                    }
                }
            };
            let end_label = format!("_end_fncall_{}_{}", name, context.shared.label_gen.get());
            // let mut si = context.si - 8;
            // base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Place(Loc::Offset(RSP, context.si))));

            let original_context = -context.si;
            // put label at [rsp-si]
            base.push(Instr::MovLabel(RAX, end_label.clone()));
            base.push(Instr::TwoArg(
                OpCode::IMov,
                Loc::Offset(RSP, context.si),
                Val::Place(Loc::Reg(RAX)),
            ));
            context.si -= 8;

            // put all args at [rsp-si-(arg+1)*8]
            for e in args.iter() {
                let e_instrs = compile_to_instrs(e, context.clone(), Vec::new())?;
                base.extend(e_instrs);
                base.push(Instr::TwoArg(
                    OpCode::IMov,
                    Loc::Offset(RSP, context.si),
                    Val::Place(Loc::Reg(RAX)),
                ));
                context.si -= 8;
            }

            // subtract and jump to function
            base.push(Instr::TwoArg(
                OpCode::ISub,
                Loc::Reg(RSP),
                Val::Imm(original_context as i64),
            ));
            base.push(Instr::Jump(
                BranchCode::Jmp,
                JumpDst::Label(name.to_string()),
            ));

            // jump
            // base.push(Instr::Jump(BranchCode::Jmp, JumpDst::Pointer(RAX)));

            // restore original stack frame after funcall
            // NOTE: To avoid fixing a mistake with JumpDst, I AM ASSUMING THAT EACH FUNCTION ENDS
            // IN A RET!!!! so, subtract 8 less
            base.push(Instr::Label(end_label));
            base.push(Instr::TwoArg(
                OpCode::IAdd,
                Loc::Reg(RSP),
                Val::Imm((original_context - 8) as i64),
            ));

            Ok(base)
        }
        Expr::Print(expr) => {
            let si = context.si;
            // let after_label = format!("_after_print_{}", context.label_gen.get());
            let e_instrs = compile_to_instrs(&expr, context, Vec::new())?;
            base.extend(e_instrs);

            // push rdi
            base.push(Instr::TwoArg(
                OpCode::IMov,
                Loc::Offset(RSP, si),
                Val::Place(Loc::Reg(RDI)),
            ));

            // push rsp
            // base.push(Instr::TwoArg(OpCode::IMov, Loc::Offset(RSP, si-8), Val::Place(Loc::Reg(RSP))));

            // mov rdi, rax
            base.push(Instr::TwoArg(
                OpCode::IMov,
                Loc::Reg(RDI),
                Val::Place(Loc::Reg(RAX)),
            ));

            // sub rsp, -context.si
            base.push(Instr::TwoArg(
                OpCode::ISub,
                Loc::Reg(RSP),
                Val::Imm((-si + 8) as i64),
            ));

            // call snek_print
            base.push(Instr::CallPrint(RAX));
            // base.push(Instr::Label(after_label))

            // pop rdi
            base.push(Instr::TwoArg(
                OpCode::IMov,
                Loc::Reg(RDI),
                Val::Place(Loc::Offset(RSP, 8)),
            ));
            // pop rsp
            // base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RSP), Val::Place(Loc::Offset(RSP, 0))));
            // add rsp, -context.si
            base.push(Instr::TwoArg(
                OpCode::IAdd,
                Loc::Reg(RSP),
                Val::Imm((-si + 8) as i64),
            ));
            Ok(base)
        }
        Expr::Cast(e, e_type) => {
            let cast_err_label = context.shared.cast_err_label.clone();
            let mut e_instrs = compile_to_instrs(e, context, base)?;
            match e_type {
                Type::Num => {
                    let typecheck_instrs =
                        type_check_loc(Loc::Reg(RAX), cast_err_label.clone(), BranchCode::Jne);
                    e_instrs.extend(typecheck_instrs);
                }
                Type::Bool => {
                    let typecheck_instrs =
                        type_check_loc(Loc::Reg(RAX), cast_err_label.clone(), BranchCode::Je);
                    e_instrs.extend(typecheck_instrs);
                }
                Type::Any => {
                    // no-op, the result can be any value
                }
                Type::Nothing => {
                    e_instrs.push(Instr::Jump(BranchCode::Jmp, JumpDst::Label(cast_err_label)));
                }
            }
            Ok(e_instrs)
        }
    }
}
