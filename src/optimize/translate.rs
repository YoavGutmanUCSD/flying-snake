use im::HashMap;

use crate::ast::Op1;
use crate::instr::Val;
use crate::types::{Type, Type::*};

use super::ast::TypedBinding;
use super::ast::TypedCall;
use super::ast::{TypedExpr as TExpr, TypedExpr_::*};
use crate::validate::ast::StackVar;

fn inline_const(e: TExpr, env: &HashMap<StackVar, Val>, changed: &mut bool) -> TExpr {
    let (expr, expr_type) = (e.0, e.1);
    match expr {
        Let(bindings, let_expr) => {
            let mut good_bindings: Vec<TypedBinding> = Vec::with_capacity(bindings.len());
            let mut new_env = env.clone();
            for binding in bindings.into_iter() {
                let id = binding.symbol.id;
                match binding.value.0 {
                    Number(i) => {
                        *changed = true;
                        new_env = new_env.update(id, Val::Imm(i << 1));
                    }
                    Boolean(b) => {
                        *changed = true;
                        new_env = new_env.update(id, Val::Imm(if b { 3 } else { 1 }));
                    }
                    _ => {
                        good_bindings.push(binding);
                    }
                }
            }
            let new_let_expr = inline_const(*let_expr, &new_env, changed);
            TExpr(Let(good_bindings, Box::new(new_let_expr)), expr_type)
        }
        Symbol(symbol) => {
            let id = symbol.id;
            if let Some(Val::Imm(value)) = env.get(&id) {
                if *value == 1 || *value == 3 {
                    TExpr(Boolean(*value == 3), Bool)
                } else {
                    TExpr(Number(value >> 1), Num)
                }
            } else {
                TExpr(Symbol(symbol), expr_type)
            }
        }
        Block(exprs) => {
            let good_exprs = exprs
                .into_iter()
                .map(|expr| inline_const(expr, env, changed))
                .collect::<Vec<TExpr>>();
            TExpr(Block(good_exprs), expr_type)
        }
        Call(typed_call) => {
            let good_args = typed_call
                .args
                .into_iter()
                .map(|expr| inline_const(expr, env, changed))
                .collect::<Vec<TExpr>>();
            let new_call = TypedCall {
                args: good_args,
                ..typed_call
            };
            TExpr(Call(new_call), expr_type)
        }
        UnOp(checks, op, e) => TExpr(
            UnOp(checks, op, Box::new(inline_const(*e, env, changed))),
            expr_type,
        ),
        BinOp(checks, op, e1, e2) => TExpr(
            BinOp(
                checks,
                op,
                Box::new(inline_const(*e1, env, changed)),
                Box::new(inline_const(*e2, env, changed)),
            ),
            expr_type,
        ),
        If(checks, econd, e1, e2) => TExpr(
            If(
                checks,
                Box::new(inline_const(*econd, env, changed)),
                Box::new(inline_const(*e1, env, changed)),
                Box::new(inline_const(*e2, env, changed)),
            ),
            expr_type,
        ),
        Set(checks, set_id, set_expr) => TExpr(
            Set(
                checks,
                set_id,
                Box::new(inline_const(*set_expr, env, changed)),
            ),
            expr_type,
        ),
        Loop(loop_expr) => TExpr(
            Loop(Box::new(inline_const(*loop_expr, env, changed))),
            expr_type,
        ),
        Break(break_expr) => TExpr(
            Break(Box::new(inline_const(*break_expr, env, changed))),
            expr_type,
        ),
        Print(print_expr) => TExpr(
            Print(Box::new(inline_const(*print_expr, env, changed))),
            expr_type,
        ),
        Cast(expr) => TExpr(Cast(Box::new(inline_const(*expr, env, changed))), expr_type),
        _ => TExpr(expr, expr_type),
    }
}

fn simplify(e: TExpr, env: &HashMap<StackVar, Type>, changed: &mut bool) -> TExpr {
    let (expr, expr_type) = (e.0, e.1);
    match expr {
        Block(exprs) => {
            let good_exprs = exprs
                .into_iter()
                .map(|expr| simplify(expr, env, changed))
                .collect::<Vec<TExpr>>();
            TExpr(Block(good_exprs), expr_type)
        }
        Call(typed_call) => {
            let good_args = typed_call
                .args
                .into_iter()
                .map(|expr| simplify(expr, env, changed))
                .collect::<Vec<TExpr>>();
            let new_call = TypedCall {
                args: good_args,
                ..typed_call
            };
            TExpr(Call(new_call), expr_type)
        }
        Cast(cast_expr) => {
            if cast_expr.type_() == expr_type {
                *changed = true;
                *cast_expr
            } else {
                TExpr(Cast(cast_expr), expr_type)
            }
        }
        UnOp(checks, op, unop_expr) => match (unop_expr.type_(), op) {
            (Num, Op1::IsNum) | (Bool, Op1::IsBool) => {
                *changed = true;
                TExpr(Boolean(true), Bool)
            }
            (_, _) => TExpr(UnOp(checks, op, unop_expr), expr_type),
        },
        BinOp(checks, op, e1, e2) => TExpr(
            BinOp(
                checks,
                op,
                Box::new(simplify(*e1, env, changed)),
                Box::new(simplify(*e2, env, changed)),
            ),
            expr_type,
        ),
        If(checks, econd, e1, e2) => match *econd {
            TExpr(Boolean(b), Bool) => {
                *changed = true;
                if b {
                    *e1
                } else {
                    *e2
                }
            }
            _ => TExpr(
                If(
                    checks,
                    Box::new(simplify(*econd, env, changed)),
                    Box::new(simplify(*e1, env, changed)),
                    Box::new(simplify(*e2, env, changed)),
                ),
                expr_type,
            ),
        },
        Set(checks, set_id, set_expr) => TExpr(
            Set(checks, set_id, Box::new(simplify(*set_expr, env, changed))),
            expr_type,
        ),
        Loop(loop_expr) => TExpr(
            Loop(Box::new(simplify(*loop_expr, env, changed))),
            expr_type,
        ),
        Break(break_expr) => TExpr(
            Break(Box::new(simplify(*break_expr, env, changed))),
            expr_type,
        ),
        Print(print_expr) => TExpr(
            Print(Box::new(simplify(*print_expr, env, changed))),
            expr_type,
        ),
        Let(bindings, let_expr) => {
            let mut good_bindings: Vec<TypedBinding> = Vec::with_capacity(bindings.len());
            for binding in bindings.into_iter() {
                let simplified_expr = simplify(binding.value, env, changed);
                let new_binding = TypedBinding {
                    value: simplified_expr,
                    ..binding
                };
                good_bindings.push(new_binding);
            }
            let good_let_expr = simplify(*let_expr, env, changed);
            TExpr(Let(good_bindings, Box::new(good_let_expr)), expr_type)
        }
        expr => TExpr(expr, expr_type),
    }
}

pub fn optimize(
    e: TExpr,
    type_env: &HashMap<StackVar, Type>,
    define_env: &HashMap<StackVar, Val>,
) -> TExpr {
    // if this is ever false after the loop, can return.
    let mut change_counter: bool;
    let mut final_expr = e;

    // loop should be as follows
    loop {
        change_counter = false;
        final_expr = simplify(final_expr, type_env, &mut change_counter);
        final_expr = inline_const(final_expr, define_env, &mut change_counter);
        // add more optimizations after or before this one. should be plug and play

        if !change_counter {
            break;
        }
    }
    final_expr
}
