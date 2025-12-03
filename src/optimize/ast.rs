use crate::{
    ast::{Op1, Op2},
    types::Type,
};

pub struct TypedExpr(pub TypedExpr_, pub Type);

pub enum TypedExpr_ {
    Number(i64),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, TypedExpr)>, Box<TypedExpr>),
    UnOp(Op1, Box<TypedExpr>),
    BinOp(Op2, Box<TypedExpr>, Box<TypedExpr>),
    If(Box<TypedExpr>, Box<TypedExpr>, Box<TypedExpr>),
    Loop(Box<TypedExpr>),
    Break(Box<TypedExpr>),
    Set(String, Box<TypedExpr>),
    Block(Vec<TypedExpr>),
    Call(String, Vec<TypedExpr>),
    Print(Box<TypedExpr>),
    Cast(Box<TypedExpr>),
}

impl TypedExpr {
    #[inline]
    pub fn type_(&self) -> Type {
        self.1
    }
}
