use im::HashMap;

use crate::ast::{Op1};
use crate::instr::Val;
use crate::types::{Type, Type::*};

use super::ast::{TypedExpr as TExpr, TypedExpr_::*};

fn inline_const(e: TExpr, env: &HashMap<String, Val>) -> TExpr {
    todo!()
//     let (e, t) = (e.0, e.1);
//     match e {
//         Id(id) if t == Num || t == Bool => {
//             todo!()
//             // let id_value = env.get(&id);
//             // if let Some(val) = id_value {
//             //     match t {
//             //         Num => TExpr(Num())
//             //     }
//             // } else {
//             //     TExpr(e, t)
//             // }
//         }
//         Let(bindings, e) => {
//             todo!()
//         }
//         UnOp(op, e) => {
//             let good_e = inline_const(*e, env);
//             TExpr(UnOp(op, Box::new(good_e)), t)
//         }
//         BinOp(op, e1, e2) => {
//             let good_e1 = inline_const(*e1, env);
//             let good_e2 = inline_const(*e2, env);
//             TExpr(BinOp(op, Box::new(good_e1), Box::new(good_e2)), t)
//         }
//         If(econd, e1, e2) => {
//             let good_econd = inline_const(*econd, env);
//             let good_e1 = inline_const(*e1, env);
//             let good_e2 = inline_const(*e2, env);
//             TExpr(
//                 If(Box::new(good_econd), Box::new(good_e1), Box::new(good_e2)),
//                 t,
//             )
//         }
//         Loop(e) => TExpr(Loop(Box::new(inline_const(*e, env))), t),
//         Break(e) => TExpr(Break(Box::new(inline_const(*e, env))), t),
//         Set(id, e) => TExpr(Set(id, Box::new(inline_const(*e, env))), t),
//         Block(exprs) => {
//             let good_exprs = exprs
//                 .into_iter()
//                 .map(|a| inline_const(a, env))
//                 .collect::<Vec<TExpr>>();
//             TExpr(Block(good_exprs), t)
//         }
//         Call(fname, args) => {
//             let good_args = args
//                 .into_iter()
//                 .map(|a| inline_const(a, env))
//                 .collect::<Vec<TExpr>>();
//             TExpr(Call(fname, good_args), t)
//         }
//         Print(e) => TExpr(Print(Box::new(inline_const(*e, env))), t),
//         Cast(e) => TExpr(Cast(Box::new(inline_const(*e, env))), t),
//         // in these cases, no constants to propagate
//         _ => TExpr(e, t),
//     }
}

fn simplify(e: TExpr, env: &HashMap<String, Type>, changed: &mut bool) -> TExpr {
    let (expr, expr_type) = (e.0, e.1);
    match expr {
        Block(exprs) => {
            let good_exprs = exprs
                .into_iter()
                .map(|expr| simplify(expr, env, changed))
                .collect::<Vec<TExpr>>();
            TExpr(Block(good_exprs), expr_type)
        }
        Call(fname, args) => {
            let good_args = args
                .into_iter()
                .map(|expr| simplify(expr, env, changed))
                .collect::<Vec<TExpr>>();
            TExpr(Call(fname, good_args), expr_type)
        }
        Cast(cast_expr) => 
            match *cast_expr {
            TExpr(inner_expr, Bool) if expr_type == Bool => TExpr(inner_expr, Bool),
            TExpr(inner_expr, Num) if expr_type == Num => TExpr(inner_expr, Bool),
            _ => TExpr(Cast(cast_expr), expr_type),
        },
        UnOp(op, unop_expr) => match (unop_expr.type_(), op) {
            (Num, Op1::IsNum) | (Bool, Op1::IsBool) => TExpr(Boolean(true), Bool),
            (_, _) => TExpr(UnOp(op, unop_expr), expr_type),
        },
        BinOp(op, e1, e2) => TExpr(
            BinOp(
                op,
                Box::new(simplify(*e1, env, changed)),
                Box::new(simplify(*e2, env, changed)),
            ),
            expr_type,
        ),
        If(econd, e1, e2) => TExpr(
            If(
                Box::new(simplify(*econd, env, changed)),
                Box::new(simplify(*e1, env, changed)),
                Box::new(simplify(*e2, env, changed)),
            ),
            expr_type,
        ),
        Set(set_id, set_expr) => TExpr(Set(set_id, Box::new(simplify(*set_expr, env, changed))), expr_type),
        Loop(loop_expr) => TExpr(Loop(Box::new(simplify(*loop_expr, env, changed))), expr_type),
        Break(break_expr) => TExpr(Break(Box::new(simplify(*break_expr, env, changed))), expr_type),
        Print(print_expr) => TExpr(Print(Box::new(simplify(*print_expr, env, changed))), expr_type),
        Let(bindings, let_expr) => {
            let mut good_bindings = Vec::with_capacity(bindings.len());
            for (id, b_expr) in bindings.into_iter() {
                good_bindings.push((id, simplify(b_expr, env, changed)));
            }
            let good_let_expr = simplify(*let_expr, env, changed);
            TExpr(Let(good_bindings, Box::new(good_let_expr)), expr_type)
        }
        expr => TExpr(expr, expr_type)
    }
}

pub fn optimize(e: TExpr, type_env: HashMap<String, Type>) {
    let mut change_counter = false;
    todo!()
}
