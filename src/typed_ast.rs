use crate::ast::{Expr, Op1, Op2};
use crate::context::FnDefs;
use crate::errors::TypeError;
use crate::types::Type;
use crate::types::Type::*;
use crate::types::{leq, union};
use im::HashMap;

enum TypedExpr<T> {
    Number(T, i64),
    Boolean(T, bool),
    Id(T, String),
    Let(T, Vec<(String, T, TypedExpr<T>)>, Box<TypedExpr<T>>),
    UnOp(T, Op1, Box<TypedExpr<T>>),
    BinOp(T, Op2, Box<TypedExpr<T>>, Box<TypedExpr<T>>),
    If(T, Box<TypedExpr<T>>, Box<TypedExpr<T>>, Box<TypedExpr<T>>),
    Loop(T, Box<TypedExpr<T>>),
    Break(T, Box<TypedExpr<T>>),
    Set(T, String, Box<TypedExpr<T>>),
    Block(T, Vec<TypedExpr<T>>),
    Call(T, String, Vec<TypedExpr<T>>),
    Print(T, Box<TypedExpr<T>>),
    Cast(T, Box<TypedExpr<T>>, Type),
}

fn get_type<T>(e: &TypedExpr<T>) -> &T {
    match e {
        TypedExpr::Number(t, ..) => t,
        TypedExpr::Boolean(t, ..) => t,
        TypedExpr::Id(t, ..) => t,
        TypedExpr::Let(t, ..) => t,
        TypedExpr::UnOp(t, ..) => t,
        TypedExpr::BinOp(t, ..) => t,
        TypedExpr::If(t, ..) => t,
        TypedExpr::Loop(t, ..) => t,
        TypedExpr::Break(t, ..) => t,
        TypedExpr::Set(t, ..) => t,
        TypedExpr::Block(t, ..) => t,
        TypedExpr::Call(t, ..) => t,
        TypedExpr::Print(t, ..) => t,
        TypedExpr::Cast(t, ..) => t,
    }
}

/* Not yet! This is for generating optimized code. Not necessary in Eastern Diamondback!
 */
fn strictify(
    e: Expr,
    env: &HashMap<String, Type>,
    fn_env: &FnDefs,
) -> Result<(TypedExpr<Type>, Type), TypeError> {
    match e {
        Expr::Number(i) => Ok((TypedExpr::Number(Num, i), Nothing)),
        Expr::Boolean(b) => Ok((TypedExpr::Boolean(Bool, b), Nothing)),
        Expr::Id(id) => match env.get(&*id) {
            None => Err(TypeError::UntypedIdentifier(id.to_string())),
            Some(id_type) => Ok((TypedExpr::Id(*id_type, id.to_string()), Nothing)),
        },
        Expr::UnOp(op, e) => {
            let (typed_e, e_breaks) = strictify(*e, env, fn_env)?;
            let e_type = *get_type(&typed_e);
            let final_type = match op {
                Op1::Add1 | Op1::Sub1 => {
                    if leq(e_type, Num) {
                        Num
                    } else {
                        return Err(TypeError::DoesNotTC);
                    }
                }
                Op1::IsBool | Op1::IsNum => Bool,
            };
            let final_typed_expr = TypedExpr::UnOp(final_type, op, Box::new(typed_e));
            Ok((final_typed_expr, e_breaks))
        }
        Expr::BinOp(op, e1, e2) => {
            let (typed_e1, e1_breaks) = strictify(*e1, env, fn_env)?;
            let (typed_e2, e2_breaks) = strictify(*e2, env, fn_env)?;
            let e1_type = *get_type(&typed_e1);
            let e2_type = *get_type(&typed_e2);
            let break_subtype = union(e1_breaks, e2_breaks);
            let both_nothing = e1_type == Nothing && e2_type == Nothing;
            let final_base_type = match op {
                Op2::Plus | Op2::Minus | Op2::Times => {
                    if leq(e1_type, Num) && leq(e2_type, Num) {
                        Num
                    } else {
                        return Err(TypeError::DoesNotTC);
                    }
                }
                Op2::Equal => {
                    let nums = leq(e1_type, Num) && leq(e2_type, Num);
                    let bools = leq(e1_type, Bool) && leq(e2_type, Bool);
                    if nums || bools {
                        Bool
                    } else {
                        return Err(TypeError::DoesNotTC);
                    }
                }
                Op2::Greater | Op2::Less | Op2::GreaterEqual | Op2::LessEqual => {
                    if leq(e1_type, Num) && leq(e2_type, Num) {
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
            let final_typed_e =
                TypedExpr::BinOp(final_e_type, op, Box::new(typed_e1), Box::new(typed_e2));
            Ok((final_typed_e, break_subtype))
        }
        Expr::Let(bindings, e) => {
            let mut new_env = env.clone();
            let mut break_type = Nothing;
            let mut typed_bindings = Vec::with_capacity(bindings.len());
            for (id, id_expr) in bindings.into_iter() {
                let (typed_id_expr, id_breaks) = strictify(id_expr, &new_env, fn_env)?;
                let id_type = *get_type(&typed_id_expr);
                new_env = new_env.update(id.to_string(), id_type);
                break_type = union(id_breaks, break_type);
                typed_bindings.push((id, id_type, typed_id_expr));
            }
            let (typed_e, e_breaks) = strictify(*e, &new_env, fn_env)?;
            let e_type = *get_type(&typed_e);
            let final_typed_e = TypedExpr::Let(e_type, typed_bindings, Box::new(typed_e));
            Ok((final_typed_e, union(e_breaks, break_type)))
        }
        Expr::If(econd, e1, e2) => {
            let (typed_econd, econd_breaks) = strictify(*econd, env, fn_env)?;
            let (typed_e1, e1_breaks) = strictify(*e1, env, fn_env)?;
            let (typed_e2, e2_breaks) = strictify(*e2, env, fn_env)?;

            let econd_type = *get_type(&typed_econd);
            if econd_type != Bool {
                return Err(TypeError::DoesNotTC);
            }

            let e1_type = *get_type(&typed_e1);
            let e2_type = *get_type(&typed_e2);
            let break_type = union(econd_breaks, union(e1_breaks, e2_breaks));
            let final_type = union(e1_type, e2_type);

            let final_expr = TypedExpr::If(
                final_type,
                Box::new(typed_econd),
                Box::new(typed_e1),
                Box::new(typed_e2),
            );
            Ok((final_expr, break_type))
        }
        Expr::Loop(e) => {
            let (typed_e, e_break) = strictify(*e, env, fn_env)?;
            let final_expr = TypedExpr::Loop(e_break, Box::new(typed_e));
            Ok((final_expr, Nothing))
        }
        Expr::Break(e) => {
            let (typed_e, e_break) = strictify(*e, env, fn_env)?;
            let final_expr = TypedExpr::Break(Nothing, Box::new(typed_e));
            Ok((final_expr, e_break))
        }
        Expr::Set(id, e) => {
            let (typed_e, e_break) = strictify(*e, env, fn_env)?;
            let e_type = *get_type(&typed_e);
            if let Some(id_type) = env.get(&*id) {
                if !leq(e_type, *id_type) {
                    return Err(TypeError::TypeMismatch(*id_type, e_type));
                }
                // otherwise do nothing
            } else {
                return Err(TypeError::DoesNotTC);
            }
            let final_expr = TypedExpr::Set(e_type, id, Box::new(typed_e));
            Ok((final_expr, e_break))
        }
        Expr::Block(exprs) => {
            // if exprs is empty, which shouldn't really be legal, do nothing.
            if exprs.len() == 0 {
                return Ok((TypedExpr::Block(Nothing, Vec::new()), Nothing));
            }

            // sane case
            let mut block_break_type = Nothing;
            let mut typed_exprs = Vec::with_capacity(exprs.len());
            for e in exprs.into_iter() {
                let (typed_e, e_breaks) = strictify(e, env, fn_env)?;
                block_break_type = union(block_break_type, e_breaks);
                typed_exprs.push(typed_e);
            }
            let last_type = *get_type(&typed_exprs[typed_exprs.len() - 1]);
            let final_expr = TypedExpr::Block(last_type, typed_exprs);
            Ok((final_expr, block_break_type))
        }
        Expr::Call(fname, args) => {
            // 1. obtain the right function arguments
            let (f_types, f_type): (&Vec<(String, Type)>, &Type);
            if let Some((fts, ft)) = fn_env.get(&fname) {
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
            let mut typed_args = Vec::with_capacity(args.len());
            let mut break_type = Nothing;
            let mut args_iter = args.into_iter();
            for (_, expected_t) in f_types {
                let a = if let Some(a) = args_iter.next() {
                    a
                } else {
                    // bad arguments to function -- should be a programmer error
                    // included for completeness
                    return Err(TypeError::DoesNotTC);
                };

                let (typed_a, a_breaks) = strictify(a, env, fn_env)?;
                let a_type = *get_type(&typed_a);
                if !leq(a_type, *expected_t) {
                    return Err(TypeError::TypeMismatch(*expected_t, a_type));
                }
                break_type = union(break_type, a_breaks);
                typed_args.push(typed_a);
            }

            let final_expr = TypedExpr::Call(*f_type, fname, typed_args);
            // 3. return function type
            return Ok((final_expr, break_type));
        }
        Expr::Print(e) => {
            let (typed_e, e_breaks) = strictify(*e, env, fn_env)?;
            let final_expr = TypedExpr::Print(*get_type(&typed_e), Box::new(typed_e));
            Ok((final_expr, e_breaks))
        }
        Expr::Cast(e, expected_type) => {
            let (typed_e, e_breaks) = strictify(*e, env, fn_env)?;

            // it turns out we are supposed to assume the expected type is correct
            let final_expr = TypedExpr::Cast(expected_type, Box::new(typed_e), expected_type);
            Ok((final_expr, e_breaks))
        }
    }
}
