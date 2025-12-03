use crate::ast::{Op1, Op2, SnekFn};
use crate::context::CompilerContext;
use crate::errors::CompileError;
use crate::instr::{BranchCode, Instr, JumpDst, Loc, OpCode, Val};
use crate::types::Type;
use crate::validate::ast::{BindingSymbol, ValidatedExpr};
use crate::validate::ValidatedFunction;
use dynasmrt::x64::Rq::*;

// this can be used later to only typecheck what hasn't been typechecked already
fn type_check_loc(loc: Loc, err_label: String, jump_cond: BranchCode) -> Vec<Instr> {
    vec![
        Instr::TwoArg(OpCode::ITest, loc, Val::Imm(1)),
        Instr::Jump(jump_cond, JumpDst::Label(err_label.clone())),
    ]
}

fn symbol_name(symbol: &BindingSymbol) -> &str {
    symbol.name.as_ref()
}

pub fn compile_validated_expr(
    e: &ValidatedExpr,
    mut context: CompilerContext,
    mut base: Vec<Instr>,
) -> Result<Vec<Instr>, CompileError> {
    match e {
        ValidatedExpr::Number(i) => {
            base.push(Instr::TwoArg(
                OpCode::IMov,
                Loc::Reg(RAX),
                Val::Imm(*i << 1),
            ));
            Ok(base)
        }
        ValidatedExpr::Boolean(b) => {
            let imm = if *b { 3 } else { 1 };
            base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(imm)));
            Ok(base)
        }
        ValidatedExpr::Symbol(symbol) => {
            if let Some(val) = context.symbol_map.get(&symbol.id) {
                base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), *val));
                return Ok(base);
            }
            let name = symbol_name(symbol);
            match context.value_map.get(name) {
                Some(val) => base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), *val)),
                None => return Err(CompileError::UnboundIdentifier(name.to_string())),
            }
            Ok(base)
        }
        ValidatedExpr::Input => {
            match context.value_map.get("input") {
                Some(val) => base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), *val)),
                None => return Err(CompileError::UnboundIdentifier("input".to_string())),
            }
            Ok(base)
        }
        ValidatedExpr::UnOp(checks, op, e1) => {
            let type_err_label = context.shared.type_err_label.clone();
            let overflow_err_label = context.shared.overflow_err_label.clone();
            let mut base = compile_validated_expr(e1, context, base)?;
            match op {
                Op1::Add1 => {
                    if checks.is_enabled(0) {
                        let typecheck =
                            type_check_loc(Loc::Reg(RAX), type_err_label.clone(), BranchCode::Jne);
                        base.extend(typecheck);
                    }
                    base.push(Instr::TwoArg(OpCode::IAdd, Loc::Reg(RAX), Val::Imm(2)));
                    base.push(Instr::Jump(
                        BranchCode::Jo,
                        JumpDst::Label(overflow_err_label.clone()),
                    ));
                }
                Op1::Sub1 => {
                    if checks.is_enabled(0) {
                        let typecheck =
                            type_check_loc(Loc::Reg(RAX), type_err_label.clone(), BranchCode::Jne);
                        base.extend(typecheck);
                    }
                    base.push(Instr::TwoArg(OpCode::ISub, Loc::Reg(RAX), Val::Imm(2)));
                    base.push(Instr::Jump(
                        BranchCode::Jo,
                        JumpDst::Label(overflow_err_label.clone()),
                    ));
                }
                Op1::IsNum => {
                    base.push(Instr::TwoArg(OpCode::ITest, Loc::Reg(RAX), Val::Imm(1)));
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(1)));
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RCX), Val::Imm(3)));
                    base.push(Instr::TwoArg(
                        OpCode::ICMove,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Reg(RCX)),
                    ));
                }
                Op1::IsBool => {
                    base.push(Instr::TwoArg(OpCode::ITest, Loc::Reg(RAX), Val::Imm(1)));
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(3)));
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RCX), Val::Imm(1)));
                    base.push(Instr::TwoArg(
                        OpCode::ICMove,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Reg(RCX)),
                    ));
                }
            }
            Ok(base)
        }
        ValidatedExpr::BinOp(checks, op, e1, e2) => {
            let e1_instr = compile_validated_expr(e1, context.clone(), Vec::new())?;
            let e2_context = CompilerContext {
                si: context.si - 8,
                ..context.clone()
            };
            let e2_instr = compile_validated_expr(e2, e2_context, Vec::new())?;
            base.extend(e1_instr);
            base.push(Instr::TwoArg(
                OpCode::IMov,
                Loc::Offset(RSP, context.si),
                Val::Place(Loc::Reg(RAX)),
            ));
            base.extend(e2_instr);
            let overflow_err_label = context.shared.overflow_err_label.clone();
            let type_err_label = context.shared.type_err_label.clone();
            if let Op2::Equal = op {
                if checks.is_enabled(0) {
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
                    base.push(Instr::Jump(
                        BranchCode::Jne,
                        JumpDst::Label(type_err_label.clone()),
                    ));
                    base.push(Instr::TwoArg(
                        OpCode::IMov,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Offset(RSP, context.si - 8)),
                    ));
                }
            } else {
                if checks.is_enabled(0) {
                    base.extend(type_check_loc(
                        Loc::Reg(RAX),
                        type_err_label.clone(),
                        BranchCode::Jne,
                    ));
                }
                if checks.is_enabled(1) {
                    base.extend(type_check_loc(
                        Loc::Offset(RSP, context.si),
                        type_err_label,
                        BranchCode::Jne,
                    ));
                }
            }
            match op {
                Op2::Plus => {
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
                    base.push(Instr::TwoArg(
                        OpCode::ISub,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Offset(RSP, context.si)),
                    ));
                    base.push(Instr::Jump(
                        BranchCode::Jo,
                        JumpDst::Label(overflow_err_label.clone()),
                    ));
                    base.push(Instr::Neg(Loc::Reg(RAX)));
                }
                Op2::Times => {
                    base.push(Instr::TwoArg(OpCode::IRsh, Loc::Reg(RAX), Val::Imm(1)));
                    base.push(Instr::TwoArg(
                        OpCode::IRsh,
                        Loc::Offset(RSP, context.si),
                        Val::Imm(1),
                    ));
                    base.push(Instr::TwoArg(
                        OpCode::IMul,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Offset(RSP, context.si)),
                    ));
                    base.push(Instr::Jump(
                        BranchCode::Jo,
                        JumpDst::Label(overflow_err_label.clone()),
                    ));
                    base.push(Instr::TwoArg(OpCode::ILsh, Loc::Reg(RAX), Val::Imm(1)));
                }
                Op2::Equal => {
                    base.push(Instr::TwoArg(
                        OpCode::ICmp,
                        Loc::Offset(RSP, context.si),
                        Val::Place(Loc::Reg(RAX)),
                    ));
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(1)));
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RCX), Val::Imm(3)));
                    base.push(Instr::TwoArg(
                        OpCode::ICMove,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Reg(RCX)),
                    ));
                }
                Op2::Greater => {
                    base.push(Instr::TwoArg(
                        OpCode::ICmp,
                        Loc::Offset(RSP, context.si),
                        Val::Place(Loc::Reg(RAX)),
                    ));
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(1)));
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RCX), Val::Imm(3)));
                    base.push(Instr::TwoArg(
                        OpCode::ICMovg,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Reg(RCX)),
                    ));
                }
                Op2::GreaterEqual => {
                    base.push(Instr::TwoArg(
                        OpCode::ICmp,
                        Loc::Offset(RSP, context.si),
                        Val::Place(Loc::Reg(RAX)),
                    ));
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(1)));
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RCX), Val::Imm(3)));
                    base.push(Instr::TwoArg(
                        OpCode::ICMovge,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Reg(RCX)),
                    ));
                }
                Op2::Less => {
                    base.push(Instr::TwoArg(
                        OpCode::ICmp,
                        Loc::Offset(RSP, context.si),
                        Val::Place(Loc::Reg(RAX)),
                    ));
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(1)));
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RCX), Val::Imm(3)));
                    base.push(Instr::TwoArg(
                        OpCode::ICMovl,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Reg(RCX)),
                    ));
                }
                Op2::LessEqual => {
                    base.push(Instr::TwoArg(
                        OpCode::ICmp,
                        Loc::Offset(RSP, context.si),
                        Val::Place(Loc::Reg(RAX)),
                    ));
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RAX), Val::Imm(1)));
                    base.push(Instr::TwoArg(OpCode::IMov, Loc::Reg(RCX), Val::Imm(3)));
                    base.push(Instr::TwoArg(
                        OpCode::ICMovle,
                        Loc::Reg(RAX),
                        Val::Place(Loc::Reg(RCX)),
                    ));
                }
            }
            Ok(base)
        }
        ValidatedExpr::If(econd, e1, e2) => {
            let econd_instr = compile_validated_expr(econd, context.clone(), Vec::new())?;
            let e1_instr = compile_validated_expr(e1, context.clone(), Vec::new())?;
            let e2_context = CompilerContext {
                si: context.si - 8,
                ..context.clone()
            };
            let e2_instr = compile_validated_expr(e2, e2_context, Vec::new())?;
            let label_ind = context.shared.label_gen.get();
            let else_label = format!("_if_else_{}", label_ind);
            let end_label = format!("_if_end_{}", label_ind);
            base.extend(econd_instr);
            base.extend(type_check_loc(
                Loc::Reg(RAX),
                context.shared.type_err_label.clone(),
                BranchCode::Je,
            ));
            base.push(Instr::TwoArg(OpCode::ICmp, Loc::Reg(RAX), Val::Imm(3)));
            base.push(Instr::Jump(
                BranchCode::Jne,
                JumpDst::Label(else_label.clone()),
            ));
            base.extend(e1_instr);
            base.push(Instr::Jump(
                BranchCode::Jmp,
                JumpDst::Label(end_label.clone()),
            ));
            base.push(Instr::Label(else_label));
            base.extend(e2_instr);
            base.push(Instr::Label(end_label));
            Ok(base)
        }
        ValidatedExpr::Let(bindings, body) => {
            let mut binding_instrs: Vec<Instr> = Vec::new();
            for binding in bindings.iter() {
                binding_instrs =
                    compile_validated_expr(&binding.value, context.clone(), binding_instrs)?;
                binding_instrs.push(Instr::TwoArg(
                    OpCode::IMov,
                    Loc::Offset(RSP, context.si),
                    Val::Place(Loc::Reg(RAX)),
                ));
                context.value_map = context.value_map.update(
                    symbol_name(&binding.symbol).to_string(),
                    Val::Place(Loc::Offset(RSP, context.si)),
                );
                context.symbol_map = context
                    .symbol_map
                    .update(binding.symbol.id, Val::Place(Loc::Offset(RSP, context.si)));
                context.si -= 8;
            }
            base.extend(binding_instrs);
            compile_validated_expr(body, context, base)
        }
        ValidatedExpr::Loop(body) => {
            let loop_label_ind = context.shared.label_gen.get();
            let loop_start = format!("_loop_start_{}", loop_label_ind);
            let loop_end = format!("_loop_end_{}", loop_label_ind);
            let next_context = CompilerContext {
                enclosing_loop_label: Some(loop_end.clone()),
                ..context
            };
            let e_instrs = compile_validated_expr(body, next_context, Vec::new())?;
            base.push(Instr::Label(loop_start.clone()));
            base.extend(e_instrs);
            base.push(Instr::Jump(
                BranchCode::Jmp,
                JumpDst::Label(loop_start.clone()),
            ));
            base.push(Instr::Label(loop_end));
            Ok(base)
        }
        ValidatedExpr::Break(e) => {
            if let Some(label) = context.enclosing_loop_label {
                let e_context = CompilerContext {
                    enclosing_loop_label: None,
                    ..context
                };
                let e_instrs = compile_validated_expr(e, e_context, Vec::new())?;
                base.extend(e_instrs);
                base.push(Instr::Jump(BranchCode::Jmp, JumpDst::Label(label)));
                Ok(base)
            } else {
                Err(CompileError::NoBreakTarget)
            }
        }
        ValidatedExpr::Set(symbol, value) => {
            let name = symbol_name(symbol);
            if context.shared.keywords.contains(name) {
                return Err(CompileError::SetKeyword);
            }
            let target = context
                .symbol_map
                .get(&symbol.id)
                .copied()
                .or_else(|| context.value_map.get(name).copied());
            match target {
                Some(Val::Place(loc)) => {
                    let e_instrs = compile_validated_expr(value, context.clone(), Vec::new())?;
                    base.extend(e_instrs);
                    base.push(Instr::TwoArg(OpCode::IMov, loc, Val::Place(Loc::Reg(RAX))));
                    Ok(base)
                }
                Some(Val::Imm(_)) => Err(CompileError::Other(
                    "Tried to set a value that was inlined as a constant! This shouldn't happen."
                        .to_string(),
                )),
                None => Err(CompileError::SetUnboundVariable),
            }
        }
        ValidatedExpr::Block(exprs) => {
            for expr in exprs.iter() {
                let e_instrs = compile_validated_expr(expr, context.clone(), Vec::new())?;
                base.extend(e_instrs);
            }
            Ok(base)
        }
        ValidatedExpr::Call(call) => {
            let end_label = format!(
                "_end_fncall_{}_{}",
                call.name,
                context.shared.label_gen.get()
            );
            let original_context = -context.si;
            base.push(Instr::MovLabel(RAX, end_label.clone()));
            base.push(Instr::TwoArg(
                OpCode::IMov,
                Loc::Offset(RSP, context.si),
                Val::Place(Loc::Reg(RAX)),
            ));
            context.si -= 8;
            for arg in call.args.iter() {
                let e_instrs = compile_validated_expr(arg, context.clone(), Vec::new())?;
                base.extend(e_instrs);
                base.push(Instr::TwoArg(
                    OpCode::IMov,
                    Loc::Offset(RSP, context.si),
                    Val::Place(Loc::Reg(RAX)),
                ));
                context.si -= 8;
            }
            base.push(Instr::TwoArg(
                OpCode::ISub,
                Loc::Reg(RSP),
                Val::Imm(original_context as i64),
            ));
            base.push(Instr::Jump(
                BranchCode::Jmp,
                JumpDst::Label(call.name.clone()),
            ));
            base.push(Instr::Label(end_label));
            base.push(Instr::TwoArg(
                OpCode::IAdd,
                Loc::Reg(RSP),
                Val::Imm((original_context - 8) as i64),
            ));
            Ok(base)
        }
        ValidatedExpr::Print(value) => {
            let si = context.si;
            let e_instrs = compile_validated_expr(value, context, Vec::new())?;
            base.extend(e_instrs);
            base.push(Instr::TwoArg(
                OpCode::IMov,
                Loc::Offset(RSP, si),
                Val::Place(Loc::Reg(RDI)),
            ));
            base.push(Instr::TwoArg(
                OpCode::IMov,
                Loc::Reg(RDI),
                Val::Place(Loc::Reg(RAX)),
            ));
            base.push(Instr::TwoArg(
                OpCode::ISub,
                Loc::Reg(RSP),
                Val::Imm((-si + 8) as i64),
            ));
            base.push(Instr::CallPrint(RAX));
            base.push(Instr::TwoArg(
                OpCode::IMov,
                Loc::Reg(RDI),
                Val::Place(Loc::Offset(RSP, 8)),
            ));
            base.push(Instr::TwoArg(
                OpCode::IAdd,
                Loc::Reg(RSP),
                Val::Imm((-si + 8) as i64),
            ));
            Ok(base)
        }
        ValidatedExpr::Cast(value, e_type) => {
            let cast_err_label = context.shared.cast_err_label.clone();
            let mut e_instrs = compile_validated_expr(value, context, base)?;
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
                Type::Any => {}
                Type::Nothing => {
                    e_instrs.push(Instr::Jump(BranchCode::Jmp, JumpDst::Label(cast_err_label)));
                }
            }
            Ok(e_instrs)
        }
    }
}

pub fn compile_validated_fn(
    fm: SnekFn,
    validated: &ValidatedFunction,
    base_context: CompilerContext,
) -> Result<(String, Vec<Instr>), CompileError> {
    let SnekFn {
        name,
        args,
        body: _,
        fn_type: _,
    } = fm;

    let mut loc_fn_context = base_context;
    let mut loc_value_map = loc_fn_context.value_map;
    let mut loc_symbol_map = loc_fn_context.symbol_map;
    if args.len() != validated.params.len() {
        return Err(CompileError::Other(
            "Validated function parameters do not match declared arity.".to_string(),
        ));
    }
    for ((arg, _), symbol) in args.into_iter().zip(validated.params.iter()) {
        let slot = Val::Place(Loc::Offset(RSP, loc_fn_context.si));
        loc_value_map = loc_value_map.update(arg, slot);
        loc_symbol_map = loc_symbol_map.update(symbol.id, slot);
        loc_fn_context.si -= 8;
    }

    loc_fn_context.value_map = loc_value_map;
    loc_fn_context.symbol_map = loc_symbol_map;

    let mut instrs = compile_validated_expr(&validated.body, loc_fn_context, Vec::new())?;
    instrs.push(Instr::Ret);
    Ok((name, instrs))
}
