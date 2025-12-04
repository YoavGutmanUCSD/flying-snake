use im::HashMap;

use crate::ast::{Op1, Op2};
use crate::context::FnDefs;
use crate::errors::TypeError;
use crate::types::{Type, Type::*};
use crate::validate::ast::{StackVar, ValidatedCall, ValidatedExpr};

use super::ast::{Check, TypedBinding, TypedCall, TypedExpr as TExpr, TypedExpr_::*};
use super::context::StrictifyCtx;

pub fn strictify_expr(
    expr: ValidatedExpr,
    env: &HashMap<StackVar, Type>,
    fn_env: &FnDefs,
    input_type: Option<Type>,
) -> Result<TExpr, TypeError> {
    let mut ctx = StrictifyCtx::new(env, fn_env, input_type);
    strictify(expr, &mut ctx)
}

fn strictify(e: ValidatedExpr, ctx: &mut StrictifyCtx) -> Result<TExpr, TypeError> {
    match e {
        ValidatedExpr::Number(num) => Ok(TExpr(Number(num), Num)),
        ValidatedExpr::Boolean(val) => Ok(TExpr(Boolean(val), Bool)),
        ValidatedExpr::Symbol(symbol) => {
            let id_type = ctx
                .get_symbol_type(&symbol)
                .expect("strictify: validated symbols must be bound");
            Ok(TExpr(Symbol(symbol), id_type))
        }
        ValidatedExpr::Input => match ctx.input_type() {
            Some(input_type) => Ok(TExpr(Input, input_type)),
            None => Err(TypeError::UntypedIdentifier("input".to_string())),
        },
        ValidatedExpr::UnOp(mut checks, op, e) => {
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
            if matches!(op, Op1::Add1 | Op1::Sub1) && expr.type_() <= Num {
                checks.0[0] = false;
            }
            let typed_expr = TExpr(UnOp(checks, op, Box::new(expr)), result_type);
            Ok(typed_expr)
        }
        ValidatedExpr::BinOp(mut checks, op, e1, e2) => {
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
            match op {
                Op2::Equal => {
                    let nums = (lhs.type_() <= Num) && (rhs.type_() <= Num);
                    let bools = (lhs.type_() <= Bool) && (rhs.type_() <= Bool);
                    if nums || bools {
                        checks.0[0] = false;
                        checks.0[1] = false;
                    }
                }
                _ => {
                    if lhs.type_() <= Num {
                        checks.0[0] = false;
                    }
                    if rhs.type_() <= Num {
                        checks.0[1] = false;
                    }
                }
            }
            let typed_expr = TExpr(BinOp(checks, op, Box::new(lhs), Box::new(rhs)), result_type);
            Ok(typed_expr)
        }
        ValidatedExpr::Let(bindings, body) => {
            let mut typed_bindings = Vec::with_capacity(bindings.len());
            let mut bound_symbols = Vec::with_capacity(bindings.len());

            for binding in bindings.into_iter() {
                let symbol = binding.symbol.clone();
                let typed_expr = strictify(binding.value, ctx)?;
                ctx.bind_symbol(&symbol, typed_expr.type_());
                bound_symbols.push(symbol.clone());
                typed_bindings.push(TypedBinding {
                    symbol,
                    value: typed_expr,
                });
            }

            let typed_body = strictify(*body, ctx)?;
            let body_type = typed_body.type_();

            for symbol in bound_symbols.into_iter() {
                ctx.unbind_symbol(&symbol);
            }

            let typed_expr = TExpr(Let(typed_bindings, Box::new(typed_body)), body_type);
            Ok(typed_expr)
        }
        ValidatedExpr::If(_, cond, then_e, else_e) => {
            let cond_typed = strictify(*cond, ctx)?;
            if cond_typed.type_() != Bool {
                return Err(TypeError::DoesNotTC);
            }

            let then_typed = strictify(*then_e, ctx)?;
            let else_typed = strictify(*else_e, ctx)?;

            let result_type = then_typed.type_().join(else_typed.type_());
            let mut checks = Check::all_true();
            checks.0[0] = false;
            let typed_expr = TExpr(
                If(
                    checks,
                    Box::new(cond_typed),
                    Box::new(then_typed),
                    Box::new(else_typed),
                ),
                result_type,
            );
            Ok(typed_expr)
        }
        ValidatedExpr::Loop(body) => {
            let (body_res, loop_break_type) = ctx.with_loop(|ctx| strictify(*body, ctx));
            let typed_body = body_res?;
            let typed_expr = TExpr(Loop(Box::new(typed_body)), loop_break_type);
            Ok(typed_expr)
        }
        ValidatedExpr::Break(e) => {
            let value = strictify(*e, ctx)?;
            let break_type = value.type_();
            ctx.add_break(break_type)?;
            Ok(TExpr(Break(Box::new(value)), Nothing))
        }
        ValidatedExpr::Set(mut checks, symbol, value_expr) => {
            let typed_value = strictify(*value_expr, ctx)?;
            let value_type = typed_value.type_();
            let id_type = ctx
                .get_symbol_type(&symbol)
                .expect("strictify: set target must be bound");
            if value_type <= id_type {
                // ok
            } else {
                return Err(TypeError::TypeMismatch(id_type, value_type));
            }
            checks.0[0] = false;
            let typed_expr = TExpr(Set(checks, symbol, Box::new(typed_value)), value_type);
            Ok(typed_expr)
        }
        ValidatedExpr::Block(exprs) => {
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
        ValidatedExpr::Call(ValidatedCall { name, args }) => {
            let (param_types, return_type) = ctx
                .get_fn(&name)
                .cloned()
                .ok_or_else(|| TypeError::UnboundFunctionNoType(name.clone()))?;

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

            Ok(TExpr(
                Call(TypedCall {
                    name,
                    args: typed_args,
                }),
                return_type,
            ))
        }
        ValidatedExpr::Print(e) => {
            let typed_e = strictify(*e, ctx)?;
            let result_type = typed_e.type_();
            Ok(TExpr(Print(Box::new(typed_e)), result_type))
        }
        ValidatedExpr::Cast(e, expected_type) => {
            let typed_e = strictify(*e, ctx)?;
            Ok(TExpr(Cast(Box::new(typed_e)), expected_type))
        }
    }
}
