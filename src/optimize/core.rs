use im::HashMap;

use crate::ast::{Expr, Op1, Op2};
use crate::context::FnDefs;
use crate::errors::TypeError;
use crate::types::{Type, Type::*};

use super::ast::{TypedExpr as TExpr, TypedExpr_::*};
use super::context::StrictifyCtx;

pub fn strictify_expr(
    expr: Expr,
    env: &HashMap<String, Type>,
    fn_env: &FnDefs,
) -> Result<TExpr, TypeError> {
    let mut ctx = StrictifyCtx::new(env, fn_env);
    strictify(expr, &mut ctx)
}

fn strictify(e: Expr, ctx: &mut StrictifyCtx) -> Result<TExpr, TypeError> {
    match e {
        Expr::Number(num) => Ok(TExpr(Number(num), Num)),
        Expr::Boolean(val) => Ok(TExpr(Boolean(val), Bool)),
        Expr::Id(id) => match ctx.get_id_type(&id) {
            None => Err(TypeError::UntypedIdentifier(id)),
            Some(id_type) => Ok(TExpr(Id(id), id_type)),
        },
        Expr::UnOp(op, e) => {
            let expr = strictify(*e, ctx)?;
            let result_type = match op {
                Op1::Add1 | Op1::Sub1 => {
                    if expr.type_() <= Num {
                        Num
                    } else {
                        return Err(TypeError::DoesNotTC);
                    }
                }
                Op1::IsBool | Op1::IsNum => Bool,
            };
            let typed_expr = TExpr(UnOp(op, Box::new(expr)), result_type);
            Ok(typed_expr)
        }
        Expr::BinOp(op, e1, e2) => {
            let lhs = strictify(*e1, ctx)?;
            let rhs = strictify(*e2, ctx)?;
            let result_type = if matches!((lhs.type_(), rhs.type_()), (Nothing, Nothing)) {
                Nothing
            } else {
                match op {
                    Op2::Plus | Op2::Minus | Op2::Times => {
                        if (lhs.type_() <= Num) && (rhs.type_() <= Num) {
                            Num
                        } else {
                            return Err(TypeError::DoesNotTC);
                        }
                    }
                    Op2::Equal => {
                        let nums = (lhs.type_() <= Num) && (rhs.type_() <= Num);
                        let bools = (lhs.type_() <= Bool) && (rhs.type_() <= Bool);
                        if nums || bools {
                            Bool
                        } else {
                            return Err(TypeError::DoesNotTC);
                        }
                    }
                    Op2::Greater | Op2::Less | Op2::GreaterEqual | Op2::LessEqual => {
                        if (lhs.type_() <= Num) && (rhs.type_() <= Num) {
                            Bool
                        } else {
                            return Err(TypeError::DoesNotTC);
                        }
                    }
                }
            };
            let typed_expr = TExpr(BinOp(op, Box::new(lhs), Box::new(rhs)), result_type);
            Ok(typed_expr)
        }
        Expr::Let(bindings, body) => {
            let mut typed_bindings = Vec::with_capacity(bindings.len());
            let mut saved_env = Vec::with_capacity(bindings.len());

            for (name, binding_expr) in bindings.into_iter() {
                let typed_expr = strictify(binding_expr, ctx)?;
                let prev = ctx.update_env(&name, typed_expr.type_());
                typed_bindings.push((name.clone(), typed_expr));
                saved_env.push((name, prev));
            }

            let typed_body = strictify(*body, ctx)?;
            let body_type = typed_body.type_();

            for (name, prev) in saved_env {
                match prev {
                    Some(old) => {
                        ctx.update_env(&name, old);
                    }
                    None => ctx.remove_binding(&name),
                }
            }

            let typed_expr = TExpr(Let(typed_bindings, Box::new(typed_body)), body_type);
            Ok(typed_expr)
        }
        Expr::If(cond, then_e, else_e) => {
            let cond_typed = strictify(*cond, ctx)?;
            if cond_typed.type_() != Bool {
                return Err(TypeError::DoesNotTC);
            }

            let then_typed = strictify(*then_e, ctx)?;
            let else_typed = strictify(*else_e, ctx)?;

            let result_type = then_typed.type_().join(else_typed.type_());
            let typed_expr = TExpr(
                If(
                    Box::new(cond_typed),
                    Box::new(then_typed),
                    Box::new(else_typed),
                ),
                result_type,
            );
            Ok(typed_expr)
        }
        Expr::Loop(body) => {
            let (body_res, loop_break_type) = ctx.with_loop(|ctx| strictify(*body, ctx));
            let typed_body = body_res?;
            let typed_expr = TExpr(Loop(Box::new(typed_body)), loop_break_type);
            Ok(typed_expr)
        }
        Expr::Break(e) => {
            let value = strictify(*e, ctx)?;
            let break_type = value.type_();
            ctx.add_break(break_type)?;
            Ok(TExpr(Break(Box::new(value)), Nothing))
        }
        Expr::Set(id, value_expr) => {
            let typed_value = strictify(*value_expr, ctx)?;
            let value_type = typed_value.type_();
            if let Some(id_type) = ctx.get_id_type(&id) {
                if value_type <= id_type {
                    // ok
                } else {
                    return Err(TypeError::TypeMismatch(id_type, value_type));
                }
            } else {
                return Err(TypeError::DoesNotTC);
            }
            let typed_expr = TExpr(Set(id, Box::new(typed_value)), value_type);
            Ok(typed_expr)
        }
        Expr::Block(exprs) => {
            if exprs.is_empty() {
                return Ok(TExpr(Block(Vec::new()), Nothing));
            }

            let mut typed_exprs = Vec::with_capacity(exprs.len());
            for expr in exprs.into_iter() {
                let typed_expr = strictify(expr, ctx)?;
                typed_exprs.push(typed_expr);
            }

            let block_type = typed_exprs
                .last()
                .map(|expr| expr.type_())
                .unwrap_or(Nothing);
            Ok(TExpr(Block(typed_exprs), block_type))
        }
        Expr::Call(fname, args) => {
            let (param_types, return_type) = ctx
                .get_fn(&fname)
                .cloned()
                .ok_or_else(|| TypeError::UnboundFunctionNoType(fname.clone()))?;

            if param_types.len() != args.len() {
                return Err(TypeError::DoesNotTC);
            }

            let mut typed_args = Vec::with_capacity(args.len());
            for ((_, expected_type), arg_expr) in param_types.into_iter().zip(args.into_iter()) {
                let typed_arg = strictify(arg_expr, ctx)?;
                if typed_arg.type_() <= expected_type {
                    // ok
                } else {
                    return Err(TypeError::TypeMismatch(expected_type, typed_arg.type_()));
                }
                typed_args.push(typed_arg);
            }

            Ok(TExpr(Call(fname, typed_args), return_type))
        }
        Expr::Print(e) => {
            let typed_e = strictify(*e, ctx)?;
            let result_type = typed_e.type_();
            Ok(TExpr(Print(Box::new(typed_e)), result_type))
        }
        Expr::Cast(e, expected_type) => {
            let typed_e = strictify(*e, ctx)?;
            Ok(TExpr(Cast(Box::new(typed_e)), expected_type))
        }
    }
}
