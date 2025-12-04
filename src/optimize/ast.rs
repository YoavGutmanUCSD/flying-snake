#![allow(dead_code)]

use crate::ast::{Op1, Op2};
pub use crate::checks::Check;
use crate::types::Type;
use crate::validate::ast::{BindingSymbol, ValidatedBinding, ValidatedCall, ValidatedExpr};

#[derive(Clone, Debug)]
pub struct TypedExpr(pub TypedExpr_, pub Type);

impl TypedExpr {
    #[inline]
    pub fn type_(&self) -> Type {
        self.1
    }
}

#[derive(Clone, Debug)]
pub struct TypedBinding {
    pub symbol: BindingSymbol,
    pub value: TypedExpr,
}

#[derive(Clone, Debug)]
pub struct TypedCall {
    pub name: String,
    pub args: Vec<TypedExpr>,
}

#[derive(Clone, Debug)]
pub enum TypedExpr_ {
    Number(i64),
    Boolean(bool),
    Symbol(BindingSymbol),
    Input,
    Let(Vec<TypedBinding>, Box<TypedExpr>),
    UnOp(Check<1>, Op1, Box<TypedExpr>),
    BinOp(Check<2>, Op2, Box<TypedExpr>, Box<TypedExpr>),
    If(Check<1>, Box<TypedExpr>, Box<TypedExpr>, Box<TypedExpr>),
    Loop(Box<TypedExpr>),
    Break(Box<TypedExpr>),
    Set(Check<1>, BindingSymbol, Box<TypedExpr>),
    Block(Vec<TypedExpr>),
    Call(TypedCall),
    Print(Box<TypedExpr>),
    Cast(Box<TypedExpr>),
}

impl From<TypedExpr> for ValidatedExpr {
    fn from(expr: TypedExpr) -> Self {
        match expr.0 {
            TypedExpr_::Number(i) => ValidatedExpr::Number(i),
            TypedExpr_::Boolean(b) => ValidatedExpr::Boolean(b),
            TypedExpr_::Symbol(symbol) => ValidatedExpr::Symbol(symbol),
            TypedExpr_::Input => ValidatedExpr::Input,
            TypedExpr_::Let(bindings, body) => {
                let validated_bindings = bindings
                    .into_iter()
                    .map(|binding| ValidatedBinding {
                        symbol: binding.symbol,
                        value: binding.value.into(),
                    })
                    .collect();
                ValidatedExpr::Let(validated_bindings, Box::new((*body).into()))
            }
            TypedExpr_::UnOp(checks, op, value) => {
                ValidatedExpr::UnOp(checks, op, Box::new((*value).into()))
            }
            TypedExpr_::BinOp(checks, op, left, right) => ValidatedExpr::BinOp(
                checks,
                op,
                Box::new((*left).into()),
                Box::new((*right).into()),
            ),
            TypedExpr_::If(checks, cond, then_e, else_e) => ValidatedExpr::If(
                checks,
                Box::new((*cond).into()),
                Box::new((*then_e).into()),
                Box::new((*else_e).into()),
            ),
            TypedExpr_::Loop(body) => ValidatedExpr::Loop(Box::new((*body).into())),
            TypedExpr_::Break(value) => ValidatedExpr::Break(Box::new((*value).into())),
            TypedExpr_::Set(checks, symbol, value) => {
                ValidatedExpr::Set(checks, symbol, Box::new((*value).into()))
            }
            TypedExpr_::Block(exprs) => {
                ValidatedExpr::Block(exprs.into_iter().map(ValidatedExpr::from).collect())
            }
            TypedExpr_::Call(TypedCall { name, args }) => ValidatedExpr::Call(ValidatedCall {
                name,
                args: args.into_iter().map(ValidatedExpr::from).collect(),
            }),
            TypedExpr_::Print(value) => ValidatedExpr::Print(Box::new((*value).into())),
            TypedExpr_::Cast(value) => ValidatedExpr::Cast(Box::new((*value).into()), expr.1),
        }
    }
}
