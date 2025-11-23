// You can add as many files as you want
use sexp::Atom::*;
use sexp::*;
use crate::errors::ParseError;

use crate::ast::Expr;
use crate::ast::SnekFn;
use crate::ast::Op1::*;
use crate::ast::Op2::*;
use crate::types::Type;

pub fn parse_expr(s: &Sexp) -> Result<Expr, ParseError> {
    match s {
        Sexp::Atom(I(n)) => Ok(Expr::Number(i64::try_from(*n).unwrap())),
        Sexp::Atom(S(id)) if id == "true" => Ok(Expr::Boolean(true)),
        Sexp::Atom(S(id)) if id == "false" => Ok(Expr::Boolean(false)),
        Sexp::Atom(S(id)) => Ok(Expr::Id(id.to_string())),
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => Ok(Expr::UnOp(Add1, Box::new(parse_expr(e)?))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => Ok(Expr::UnOp(Sub1, Box::new(parse_expr(e)?))),
                [Sexp::Atom(S(op)), e] if op == "isnum" => Ok(Expr::UnOp(IsNum, Box::new(parse_expr(e)?))),
                [Sexp::Atom(S(op)), e] if op == "isbool" => Ok(Expr::UnOp(IsBool, Box::new(parse_expr(e)?))),
                [Sexp::Atom(S(op)), e] if op == "loop" => Ok(Expr::Loop(Box::new(parse_expr(e)?))),
                [Sexp::Atom(S(op)), e] if op == "break" => Ok(Expr::Break(Box::new(parse_expr(e)?))),
                [Sexp::Atom(S(op)), Sexp::List(vec2), e] if op == "let" => Ok(Expr::Let(parse_binds(vec2)?, Box::new(parse_expr(e)?))),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Ok(Expr::BinOp(Plus, Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Ok(Expr::BinOp(Minus, Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Ok(Expr::BinOp(Times, Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Ok(Expr::BinOp(Greater, Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Ok(Expr::BinOp(GreaterEqual, Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Ok(Expr::BinOp(Less, Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Ok(Expr::BinOp(LessEqual, Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Ok(Expr::BinOp(Equal, Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),
                [Sexp::Atom(S(op)), Sexp::Atom(S(id)), e2] if op == "set!" => Ok(Expr::Set(id.to_string(), Box::new(parse_expr(e2)?))),
                [Sexp::Atom(S(op)), econd, e1, e2] if op == "if" => Ok(Expr::If(Box::new(parse_expr(econd)?), Box::new(parse_expr(e1)?), Box::new(parse_expr(e2)?))),
                [Sexp::Atom(S(op)), _] if op == "define" => Err(ParseError::UnexpectedDefine),
                [Sexp::Atom(S(op)), es @ ..] if op == "block" => {
                    let mut exprs: Vec<Expr> = Vec::with_capacity(es.len());
                    for e in es.iter() {
                        exprs.push(parse_expr(e)?);
                    }
                    Ok(Expr::Block(exprs))
                },
                [Sexp::Atom(S(op)), e] if op == "print" => {
                    Ok(Expr::Print(Box::new(parse_expr(e)?)))
                },
                [Sexp::Atom(S(cast)), e, Sexp::Atom(S(e_type))] if cast == "cast" => {
                    Ok(Expr::Cast(Box::new(parse_expr(e)?), parse_type(e_type)?))
                }
                [Sexp::Atom(S(fname)), es @ .. ] => {
                    let mut args: Vec<Expr> = Vec::with_capacity(es.len());
                    for e in es.iter() {
                        args.push(parse_expr(e)?);
                    }
                    Ok(Expr::Call(fname.to_string(), args))
                }
                _ => Err(ParseError::BadList)
            }
        },
        _ => Err(ParseError::InvalidProgram)
    }
}

fn parse_type(s: &str) -> Result<Type, ParseError> {
    match s {
        "Num" => Ok(Type::Num),
        "Bool" => Ok(Type::Bool),
        "Any" => Ok(Type::Any),
        _ => Err(ParseError::BadType)
    }
}


pub fn parse_fn(s: &Sexp) -> Result<SnekFn, ParseError> {
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(fun)), Sexp::List(subvec), Sexp::Atom(S(arrow)), Sexp::Atom(S(fn_type)), e] 
                if fun == "fun" && arrow == "->" => {
                    match &subvec[..] {
                        [Sexp::Atom(S(fname)), fnargs @ ..] => {
                            Ok(SnekFn{
                                name: fname.clone(),
                                args: typed_args(fnargs)?,
                                body: parse_expr(e)?,
                                fn_type: parse_type(fn_type)?,
                            })
                        },
                        _ => Err(ParseError::BadFnExpr)
                    }
                },
            [Sexp::Atom(S(fun)), Sexp::List(subvec), e] if fun == "fun" =>
                match &subvec[..] {
                    [Sexp::Atom(S(fname)), fnargs @ ..] => {
                        Ok(SnekFn{
                            name: fname.clone(),
                            args: untyped_args(fnargs)?,
                            body: parse_expr(e)?,
                            fn_type: Type::Any
                        })
                    },
                    _ => Err(ParseError::BadFnExpr)
                },
            _ => Err(ParseError::BadFnExpr)
        }
        _ => Err(ParseError::BadFnExpr)
    }
}

fn typed_args(args: &[Sexp]) -> Result<Vec<(String, Type)>, ParseError> {
    let mut retval: Vec<(String, Type)> = Vec::with_capacity(args.len());
    for arg in args.iter() {
        match arg {
            Sexp::List(vec) => match &vec[..] {
                [Sexp::Atom(S(arg_name)), Sexp::Atom(S(colon)), Sexp::Atom(S(arg_type))] if colon == ":"
                    => retval.push((arg_name.to_string(), parse_type(arg_type)?)),
                _ => return Err(ParseError::BadlyTypedFnArg)
            }
            _ => return Err(ParseError::SingleBadFnArg)
        }
    }
    Ok(retval)
}

fn untyped_args(args: &[Sexp]) -> Result<Vec<(String, Type)>, ParseError> {
    let mut retval: Vec<(String, Type)> = Vec::with_capacity(args.len());
    for arg in args.iter() {
        match arg {
            Sexp::Atom(S(name)) => retval.push((name.to_string(), Type::Any)),
            _ => return Err(ParseError::SingleBadFnArg)
        }
    }
    Ok(retval)
}

fn parse_binds(bindings: &Vec<Sexp>) -> Result<Vec<(String, Expr)>, ParseError> {
    bindings.iter().map(parse_bind).collect()
}

fn parse_bind(binding: &Sexp) -> Result<(String, Expr), ParseError> {
    match binding {
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(name)), e] => {
                    Ok((name.clone(), parse_expr(e)?))
                },
                _ => Err(ParseError::BadLetBinding)
            }
        }
        _ => Err(ParseError::BadLetExpression)
    }
}
