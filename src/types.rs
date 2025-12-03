use std::cmp::Ordering;

use crate::ast::{Expr, Op1, Op2};
use crate::errors::TypeError;
use im::HashMap;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Type {
    Num,
    Bool,
    Any,
    Nothing,
}
use Type::*;

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (*self, *other) {
            (Nothing, Nothing) | (Num, Num) | (Bool, Bool) | (Any, Any) => Some(Ordering::Equal),
            (_, Any) => Some(Ordering::Less),
            (Any, _) => Some(Ordering::Greater),
            (_, Nothing) => Some(Ordering::Greater),
            (Nothing, _) => Some(Ordering::Less),
            (Num, Bool) | (Bool, Num) => None,
        }
    }
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Num => "Num",
            Type::Bool => "Bool",
            Type::Any => "Any",
            Type::Nothing => "Nothing",
        }
        .to_string()
    }
}

impl Type {
    pub fn join(&self, other: Type) -> Type {
        match (*self, other) {
            (Nothing, a) => a,
            (a, Nothing) => a,
            (Num, Num) => Num,
            (Bool, Bool) => Bool,
            _ => Any,
        }
    }
}

// TODO: switch over to a Result error type.
pub fn tc(
    e: &Expr,
    env: &HashMap<String, Type>,
    fn_env: &HashMap<String, (Vec<(String, Type)>, Type)>,
) -> Result<(Type, Type), TypeError> {
    match e {
        Expr::Number(_) => Ok((Num, Nothing)),
        Expr::Boolean(_) => Ok((Bool, Nothing)),
        Expr::Id(x) => {
            // let xt = env.get(x)?;
            match env.get(x) {
                None => Err(TypeError::UntypedIdentifier(x.to_string())),
                Some(xt) => Ok((*xt, Nothing)),
            }
        }
        Expr::UnOp(op, e) => {
            let (e_type, e_breaks) = tc(e, env, fn_env)?;
            match op {
                Op1::Add1 | Op1::Sub1 => {
                    if (e_type <= Num) {
                        Ok((Num, e_breaks))
                    } else {
                        Err(TypeError::DoesNotTC)
                    }
                }
                Op1::IsBool | Op1::IsNum => Ok((Bool, e_breaks)),
            }
        }
        Expr::BinOp(op, e1, e2) => {
            let (e1_type, e1_breaks) = tc(e1, env, fn_env)?;
            let (e2_type, e2_breaks) = tc(e2, env, fn_env)?;
            let break_subtype = e1_breaks.join(e2_breaks);
            let both_nothing = e1_type == Nothing && e2_type == Nothing;
            let final_base_type = match op {
                Op2::Plus | Op2::Minus | Op2::Times => {
                    if (e1_type <= Num) && (e2_type <= Num) {
                        Num
                    } else {
                        return Err(TypeError::DoesNotTC);
                    }
                }
                Op2::Equal => {
                    let nums = (e1_type <= Num) && (e2_type <= Num);
                    let bools = (e1_type <= Bool) && (e2_type <= Bool);
                    if nums || bools {
                        Bool
                    } else {
                        return Err(TypeError::DoesNotTC);
                    }
                }
                Op2::Greater | Op2::Less | Op2::GreaterEqual | Op2::LessEqual => {
                    if (e1_type <= Num) && (e2_type <= Num) {
                        Bool
                    } else {
                        return Err(TypeError::DoesNotTC);
                    }
                }
            };
            let final_e_type = if both_nothing {
                Nothing
            } else {
                final_base_type
            };
            Ok((final_e_type, break_subtype))
        }
        Expr::Let(bindings, e) => {
            let mut new_env = env.clone();
            let mut break_type = Nothing;
            for (id, id_expr) in bindings.iter() {
                let (id_type, id_breaks) = tc(id_expr, &new_env, fn_env)?;
                new_env = new_env.update(id.to_string(), id_type);
                break_type = id_breaks.join(break_type);
            }
            let (e_type, e_breaks) = tc(e, &new_env, fn_env)?;
            Ok((e_type, e_breaks.join(break_type)))
        }
        Expr::If(econd, e1, e2) => {
            let (econd_type, econd_breaks) = tc(econd, env, fn_env)?;
            let (e1_type, e1_breaks) = tc(e1, env, fn_env)?;
            let (e2_type, e2_breaks) = tc(e2, env, fn_env)?;
            let break_type = econd_breaks.join(e1_breaks).join(e2_breaks);
            let final_type = e1_type.join(e2_type);

            if econd_type != Bool {
                return Err(TypeError::DoesNotTC);
            } else {
                return Ok((final_type, break_type));
            }
        }
        Expr::Loop(e) => {
            let (_, e_break) = tc(e, env, fn_env)?;
            Ok((e_break, Nothing))
        }
        Expr::Break(e) => {
            let (e_type, _) = tc(e, env, fn_env)?;
            Ok((Nothing, e_type))
        }
        Expr::Set(id, e) => {
            let (e_type, e_break) = tc(e, env, fn_env)?;
            if let Some(id_type) = env.get(id) {
                if e_type <= *id_type {
                    Ok((e_type, e_break))
                } else {
                    Err(TypeError::TypeMismatch(*id_type, e_type))
                }
            } else {
                Err(TypeError::DoesNotTC)
            }
        }
        Expr::Block(exprs) => {
            // NOTE: the block type is the type of the last expression.
            // for all previous expressions, only break types count.
            // this is why I use saturating_sub here.
            let (last_type, last_breaks) = tc(&exprs[exprs.len().saturating_sub(1)], env, fn_env)?;
            let mut block_break_type = last_breaks;
            for e in exprs[0..exprs.len().saturating_sub(1)].iter() {
                let (_, e_breaks) = tc(e, env, fn_env)?;
                block_break_type = block_break_type.join(e_breaks);
            }
            Ok((last_type, block_break_type))
        }
        // TODO: implement function typechecking
        Expr::Call(fname, args) => {
            // 1. obtain the right function arguments
            let (f_types, f_type): (&Vec<(String, Type)>, &Type);
            if let Some((fts, ft)) = fn_env.get(fname) {
                f_types = fts;
                f_type = ft;
            } else {
                // type error undefined function
                return Err(TypeError::UnboundFunctionNoType(fname.to_string()));
            }
            if f_types.len() != args.len() {
                return Err(TypeError::DoesNotTC);
            }
            // 2. compare each argument to expected type
            // for a in args.iter() {
            let mut break_type = Nothing;
            for i in 0..args.len() {
                let a = &args[i];
                let (_, expected_t) = f_types[i];
                let (a_type, a_breaks) = tc(&a, env, fn_env)?;
                break_type = break_type.join(a_breaks);
                if !(a_type <= expected_t) {
                    return Err(TypeError::TypeMismatch(expected_t, a_type));
                }
            }

            // 3. return function type
            return Ok((*f_type, break_type));
        }
        Expr::Print(e) => tc(e, env, fn_env),
        Expr::Cast(e, expected_type) => {
            let (_, e_breaks) = tc(e, env, fn_env)?;

            // it turns out we are supposed to assume the expected type is correct
            Ok((*expected_type, e_breaks))
        }
    }
}
