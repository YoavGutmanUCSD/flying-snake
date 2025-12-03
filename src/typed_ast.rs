use crate::ast::{Expr, Op1, Op2};
use crate::context::FnDefs;
use crate::errors::TypeError;
use crate::types::union;
use crate::types::Type;
use crate::types::Type::*;
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
    }
}

/* Not yet! This is for generating optimized code. Not necessary in Eastern Diamondback!
 */
fn strictify(
    e: &Expr,
    env: &HashMap<String, Type>,
    fn_env: &FnDefs,
) -> Result<(TypedExpr<Type>, Type), TypeError> {
    match e {}
}
