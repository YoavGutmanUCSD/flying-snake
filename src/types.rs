use crate::ast::{Op1, Op2};
use im::HashMap;
use crate::errors::TypeError;

#[derive(Copy, Clone, PartialEq)]
pub enum Type {
    Num,
    Bool,
    Any,
    Nothing
}

use Type::*;
use crate::ast::Expr as Expr;

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Num => "Num",
            Type::Bool => "Bool",
            Type::Any => "Any",
            Type::Nothing => "Nothing"
        }.to_string()
    }
}

fn union(t1: Type, t2: Type) -> Type {
    match (t1, t2) {
        (Nothing, a) => a,
        (a, Nothing) => a,
        (Num, Num) => Num,
        (Bool, Bool) => Bool,
        _ => Any
    }

}

// TODO: switch over to a Result error type.
pub fn tc(e: &Expr, env: &HashMap<String, Type>, fn_env: &HashMap<String, (Vec<(String, Type)>, Type)>) -> Result<(Type, Type), TypeError> {
    match e {
        Expr::Number(_) => Ok((Num, Nothing)),
        Expr::Boolean(_) => Ok((Bool, Nothing)),
        Expr::Id(x) => {
            // let xt = env.get(x)?;
            match env.get(x) {
                None => Err(TypeError::UntypedIdentifier(x.to_string())),
                Some(xt) => Ok((*xt, Nothing))
            }
        },
        Expr::UnOp(op, e) => {
            let (e_type, e_breaks) = tc(e, env, fn_env)?;
            match (op, e_type) {
                (Op1::Add1, Num) => Ok((Num, e_breaks)),
                (Op1::Sub1, Num) => Ok((Num, e_breaks)),
                (Op1::Add1, Nothing) => Ok((Num, e_breaks)),
                (Op1::Sub1, Nothing) => Ok((Num, e_breaks)),
                (Op1::IsBool, _) => Ok((Bool, e_breaks)),
                (Op1::IsNum, _) => Ok((Bool, e_breaks)),
                _ => Err(TypeError::DoesNotTC)
            }
        }
        Expr::BinOp(op, e1, e2) => {
            let (e1_type, e1_breaks) = tc(e1, env, fn_env)?;
            let (e2_type, e2_breaks) = tc(e2, env, fn_env)?;
            let break_subtype = union(e1_breaks, e2_breaks);
            let subtype = union(e1_type, e2_type);
            let final_e_type = match (op, subtype) {
                (_, Nothing) => Nothing,
                (Op2::Plus, Num) => Num,
                (Op2::Minus, Num) => Num,
                (Op2::Times, Num) => Num,
                (Op2::Equal, _) => Bool,
                (Op2::Greater, Num) => Bool,
                (Op2::Less, Num) => Bool,
                (Op2::GreaterEqual, Num) => Num,
                (Op2::LessEqual, Num) => Num,
                _ => return Err(TypeError::DoesNotTC),
            };
            return Ok((final_e_type, break_subtype));
        },
        Expr::Let(bindings, e) => {
            let mut new_env = env.clone();
            let mut break_type = Nothing;
            for (id, id_expr) in bindings.iter() {
                let (id_type, id_breaks) = tc(id_expr, &new_env, fn_env)?;
                new_env = new_env.update(id.to_string(), id_type);
                break_type = union(id_breaks, break_type);
            }
            let (e_type, e_breaks) = tc(e, &new_env, fn_env)?;
            Ok((e_type, union(e_breaks, break_type)))
        }
        Expr::If(econd, e1, e2) => {
            let (econd_type, econd_breaks) = tc(econd, env, fn_env)?;
            let (e1_type, e1_breaks) = tc(e1, env, fn_env)?;
            let (e2_type, e2_breaks) = tc(e2, env, fn_env)?;
            let break_type = union(econd_breaks,
                union(e1_breaks, e2_breaks));
            let final_type = union(e1_type, e2_type);


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
                if e_type != *id_type {
                    Err(TypeError::TypeMismatch(*id_type, e_type))
                } else {
                    Ok((e_type, e_break))
                }
            }
            else {
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
                block_break_type = union(block_break_type, e_breaks);
            }
            Ok((last_type, block_break_type))
        }
        // TODO: implement function typechecking
        Expr::Call(fname, args) => {
            // 1. obtain the right function arguments
            let (f_types, f_type): (&Vec<(String, Type)>, &Type);
            if let Some((fts, ft)) = fn_env.get(fname) {
                f_types = fts; f_type = ft;
            }
            else {
                // type error undefined function
                return Err(TypeError::UnboundFunctionNoType(fname.to_string()));
            }
            // 2. compare each argument to expected type
            // for a in args.iter() {
            let mut break_type = Nothing;
            for i in 0..args.len() {
                let a = &args[i];
                let (_, expected_t) = f_types[i];
                let (a_type, a_breaks) = tc(&a, env, fn_env)?;
                break_type = union(break_type, a_breaks);
                if expected_t != a_type {
                    return Err(TypeError::TypeMismatch(expected_t, a_type));
                }
            }

            // 3. return function type
            return Ok((*f_type, Nothing));
        }
        Expr::Print(e) => {
            tc(e, env, fn_env)
        }
        Expr::Cast(e, expected_type) => {
            let (_, e_breaks) = tc(e, env, fn_env)?;

            // it turns out we are supposed to assume the expected type is correct
            Ok((*expected_type, e_breaks))
        }
    }
}
